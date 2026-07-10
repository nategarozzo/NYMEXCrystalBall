# ============================================================
# model_b_monte_carlo.R
# Model B — Monte Carlo simulation using momentum-blended
# regime-conditioned historical block bootstrap
# Gap amplification scales daily returns by fair value gap
# Input: data/raw/ng_futures_daily.rds
#        data/features/master_panel.rds
#        data/forecasts/model_a_objects.rds
# Output: data/forecasts/forecast_b.rds
#         data/forecasts/monte_carlo_objects.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(zoo)

ng_futures_daily <- readRDS("data/raw/ng_futures_daily.rds")
master_panel     <- readRDS("data/features/master_panel.rds")
model_a_objects  <- readRDS("data/forecasts/model_a_objects.rds")

# N set in run_forecast.R
n_sims   <- 1000
n_blocks <- ceiling(N / 5)

# ============================================================
# ### STEP 1: GAP AMPLIFICATION PARAMETER ###
# ============================================================

# Alpha controls how much the fair value gap amplifies daily returns
# Higher alpha = bolder directional forecasts when gap is large
# Alpha = 0 means no amplification (pure regime bootstrap)
# Alpha = 6 chosen based on backtest analysis

alpha         <- 6  # adjust this single line to tune amplification
current_gap   <- model_a_objects$current_gap
gap_amplifier <- 1 + alpha * abs(current_gap)

cat("Alpha:", alpha, "\n")
cat("Current gap (log):", round(current_gap, 4), "\n")
cat("Gap amplifier:", round(gap_amplifier, 2), "\n")

# ============================================================
# ### STEP 2: BUILD DAILY RETURN SERIES ###
# ============================================================

daily_returns <- ng_futures_daily |>
  filter(date >= as.Date("2017-01-01"), !is.na(c1_price)) |>
  arrange(date) |>
  mutate(
    log_price       = log(c1_price),
    log_return_1d   = log_price - lag(log_price, 1),
    rolling_vol_20d = rollapply(log_return_1d, width = 20,
                                FUN = sd, align = "right",
                                fill = NA, na.rm = TRUE),
    week_ending     = ceiling_date(date, unit = "week", week_start = 6) - 1
  ) |>
  drop_na()

# ============================================================
# ### STEP 3: JOIN STORAGE SURPLUS FOR REGIME CLASSIFICATION ###
# ============================================================

daily_returns <- daily_returns |>
  left_join(
    master_panel |> select(week_ending, storage_5yr_surplus),
    by = "week_ending"
  ) |>
  fill(storage_5yr_surplus, .direction = "down")

# ============================================================
# ### STEP 4: CLASSIFY EACH DAY INTO REGIME ###
# ============================================================

vol_median <- median(daily_returns$rolling_vol_20d, na.rm = TRUE)

daily_returns <- daily_returns |>
  mutate(
    season = case_when(
      month(date) %in% c(12, 1, 2) ~ "winter",
      month(date) %in% c(6, 7, 8)  ~ "summer",
      TRUE                           ~ "shoulder"
    ),
    vol_regime     = if_else(rolling_vol_20d > vol_median, "high", "low"),
    storage_regime = if_else(storage_5yr_surplus < 0, "deficit", "surplus")
  )

# ============================================================
# ### STEP 5: DETERMINE CURRENT REGIME ###
# ============================================================

last_date    <- max(daily_returns$date)
current_c1   <- ng_futures_daily |>
  filter(date == last_date) |>
  pull(c1_price)
current_vol  <- daily_returns |>
  filter(date == last_date) |>
  pull(rolling_vol_20d)
current_stor <- master_panel |>
  filter(!is.na(storage_5yr_surplus)) |>
  arrange(desc(week_ending)) |>
  slice(1) |>
  pull(storage_5yr_surplus)

current_season   <- case_when(
  month(last_date) %in% c(12, 1, 2) ~ "winter",
  month(last_date) %in% c(6, 7, 8)  ~ "summer",
  TRUE                                ~ "shoulder"
)
current_vol_reg  <- if_else(current_vol > vol_median, "high", "low")
current_stor_reg <- if_else(current_stor < 0, "deficit", "surplus")
current_regime   <- paste(current_season, current_vol_reg,
                          current_stor_reg, sep = "-")

cat("\nCurrent regime:", current_regime, "\n")
cat("Current C1: $", round(current_c1, 2), "\n")
cat("Current vol:", round(current_vol, 4), "\n")
cat("Storage surplus:", round(current_stor, 1), "Bcf\n")

# ============================================================
# ### STEP 6: COMPUTE MOMENTUM SIGNAL ###
# ============================================================

momentum_signal   <- daily_returns |>
  filter(date <= last_date) |>
  arrange(date) |>
  tail(5) |>
  pull(log_return_1d) |>
  mean(na.rm = TRUE)

momentum_strength <- abs(momentum_signal) /
  sd(daily_returns$log_return_1d, na.rm = TRUE)

recent_weight   <- pmin(momentum_strength / 3, 0.80)
regime_weight   <- 1 - recent_weight
n_recent_blocks <- round(recent_weight * n_blocks)
n_regime_blocks <- n_blocks - n_recent_blocks

cat("\nMomentum signal:", round(momentum_signal, 4), "\n")
cat("Momentum strength (z-score):", round(momentum_strength, 2), "\n")
cat("Recent weight:", round(recent_weight, 2), "\n")
cat("Regime weight:", round(regime_weight, 2), "\n")

# ============================================================
# ### STEP 7: BUILD SAMPLING POOLS ###
# ============================================================

recent_pool_weeks <- daily_returns |>
  filter(date <= last_date) |>
  arrange(date) |>
  tail(45) |>
  mutate(log_return_1d = pmax(pmin(log_return_1d, 0.20), -0.20)) |>
  group_by(week_ending) |>
  summarise(returns = list(log_return_1d), n_days = n(), .groups = "drop") |>
  filter(n_days >= 4)

regime_pool_weeks <- daily_returns |>
  filter(
    season         == current_season,
    vol_regime     == current_vol_reg,
    storage_regime == current_stor_reg,
    !(date >= as.Date("2022-01-01") & date <= as.Date("2023-06-30"))
  ) |>
  mutate(log_return_1d = pmax(pmin(log_return_1d, 0.20), -0.20)) |>
  group_by(week_ending) |>
  summarise(returns = list(log_return_1d), n_days = n(), .groups = "drop") |>
  filter(n_days >= 4)

cat("\nRecent pool weeks:", nrow(recent_pool_weeks), "\n")
cat("Regime pool weeks:", nrow(regime_pool_weeks), "\n")

# ============================================================
# ### STEP 8: RUN MONTE CARLO — AMPLIFIED FOR MEDIAN ###
# ============================================================

# Two simulations:
# 1. Amplified (gap_amplifier) — used for median forecast line
# 2. Base (amplifier = 1) — used for confidence band width

run_simulation <- function(amplifier) {
  set.seed(42)
  paths <- matrix(NA, nrow = n_sims, ncol = N)
  log_start <- log(current_c1)
  
  for (sim in 1:n_sims) {
    if (n_recent_blocks > 0 && nrow(recent_pool_weeks) > 0) {
      rb <- sample(nrow(recent_pool_weeks), n_recent_blocks, replace = TRUE)
      rr <- unlist(recent_pool_weeks$returns[rb])
    } else { rr <- numeric(0) }
    
    if (n_regime_blocks > 0 && nrow(regime_pool_weeks) > 0) {
      gb <- sample(nrow(regime_pool_weeks), n_regime_blocks, replace = TRUE)
      gr <- unlist(regime_pool_weeks$returns[gb])
    } else { gr <- numeric(0) }
    
    all_returns <- c(rr, gr)
    if (length(all_returns) >= N) {
      daily_rets <- all_returns[1:N]
    } else {
      extra      <- sample(unlist(regime_pool_weeks$returns),
                           N - length(all_returns), replace = TRUE)
      daily_rets <- c(all_returns, extra)
    }
    
    lp <- log_start
    for (day in 1:N) {
      lp          <- lp + daily_rets[day] * amplifier
      paths[sim, day] <- exp(lp)
    }
  }
  paths
}

cat("\nRunning amplified simulation (median)...\n")
sim_paths_amp  <- run_simulation(gap_amplifier)

cat("Running base simulation (band)...\n")
sim_paths_base <- run_simulation(1)

# ============================================================
# ### STEP 9: EXTRACT FORECAST STATISTICS ###
# ============================================================

# Median from amplified simulation — directionally informed
# Band from base simulation — honest uncertainty without compounding

sim_summary <- tibble(
  trading_day = 1:N,
  forecast    = apply(sim_paths_amp,  2, median),
  lower_90    = apply(sim_paths_base, 2, quantile, probs = 0.10),
  lower_60    = apply(sim_paths_base, 2, quantile, probs = 0.20),
  upper_60    = apply(sim_paths_base, 2, quantile, probs = 0.80),
  upper_90    = apply(sim_paths_base, 2, quantile, probs = 0.90)
)

# ============================================================
# ### STEP 10: MAP TO CALENDAR DATES ###
# ============================================================

future_dates <- seq.Date(last_date + 1, by = "day", length.out = N * 3) |>
  as_tibble() |>
  rename(date = value) |>
  mutate(is_trading = !wday(date) %in% c(1, 7)) |>
  filter(is_trading) |>
  head(N) |>
  mutate(trading_day = row_number())

forecast_b <- sim_summary |>
  left_join(future_dates, by = "trading_day") |>
  mutate(model = "B") |>
  select(model, date, trading_day, forecast,
         lower_90, lower_60, upper_60, upper_90)

# ============================================================
# ### STEP 11: SAVE OUTPUT ###
# ============================================================

monte_carlo_objects <- list(
  sim_paths_amp     = sim_paths_amp,
  sim_paths_base    = sim_paths_base,
  sim_summary       = sim_summary,
  forecast_b        = forecast_b,
  current_regime    = current_regime,
  alpha             = alpha,
  gap_amplifier     = gap_amplifier,
  current_gap       = current_gap,
  momentum_signal   = momentum_signal,
  momentum_strength = momentum_strength,
  recent_weight     = recent_weight,
  regime_weight     = regime_weight,
  n_sims            = n_sims,
  N                 = N
)

dir.create("data/forecasts", recursive = TRUE, showWarnings = FALSE)

saveRDS(forecast_b,          "data/forecasts/forecast_b.rds")
saveRDS(monte_carlo_objects, "data/forecasts/monte_carlo_objects.rds")

cat("\nMonte Carlo complete\n")
cat("Regime:", current_regime, "\n")
cat("Alpha:", alpha, "| Gap amplifier:", round(gap_amplifier, 2), "\n")
cat("Momentum blend: recent =", round(recent_weight, 2),
    "| regime =", round(regime_weight, 2), "\n")

print(forecast_b |>
        filter(trading_day %in% c(1, 5, 10, 15, 20)) |>
        select(date, forecast, lower_90, upper_90))