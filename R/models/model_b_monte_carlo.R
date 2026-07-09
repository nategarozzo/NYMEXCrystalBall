# ============================================================
# model_b_monte_carlo.R
# Model B — Monte Carlo simulation using momentum-blended
# regime-conditioned historical block bootstrap
# Pure price path simulation — no fundamental drift
# Blends recent returns (momentum) with regime pool (historical analog)
# Input: data/raw/ng_futures_daily.rds
#        data/features/master_panel.rds
# Output: data/forecasts/forecast_b.rds
#         data/forecasts/monte_carlo_objects.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(zoo)

ng_futures_daily <- readRDS("data/raw/ng_futures_daily.rds")
master_panel     <- readRDS("data/features/master_panel.rds")

# N set in run_forecast.R
n_sims   <- 1000
n_blocks <- ceiling(N / 5)

# ============================================================
# ### STEP 1: BUILD DAILY RETURN SERIES ###
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
# ### STEP 2: JOIN STORAGE SURPLUS FOR REGIME CLASSIFICATION ###
# ============================================================

daily_returns <- daily_returns |>
  left_join(
    master_panel |> select(week_ending, storage_5yr_surplus),
    by = "week_ending"
  ) |>
  fill(storage_5yr_surplus, .direction = "down")

# ============================================================
# ### STEP 3: CLASSIFY EACH DAY INTO REGIME ###
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
# ### STEP 4: DETERMINE CURRENT REGIME ###
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

cat("Current regime:", current_regime, "\n")
cat("Current C1: $", round(current_c1, 2), "\n")
cat("Current vol:", round(current_vol, 4), "\n")
cat("Storage surplus:", round(current_stor, 1), "Bcf\n")

# ============================================================
# ### STEP 5: COMPUTE MOMENTUM SIGNAL ###
# ============================================================

# Use 5-day rolling mean return as momentum signal
# Strong momentum = sample more from recent returns
# Weak momentum = sample more from regime pool

momentum_signal   <- daily_returns |>
  filter(date <= last_date) |>
  arrange(date) |>
  tail(5) |>
  pull(log_return_1d) |>
  mean(na.rm = TRUE)

momentum_strength <- abs(momentum_signal) /
  sd(daily_returns$log_return_1d, na.rm = TRUE)

recent_weight <- pmin(momentum_strength / 3, 0.80)
regime_weight <- 1 - recent_weight

n_recent_blocks <- round(recent_weight * n_blocks)
n_regime_blocks <- n_blocks - n_recent_blocks

cat("\nMomentum signal:", round(momentum_signal, 4), "\n")
cat("Momentum strength (z-score):", round(momentum_strength, 2), "\n")
cat("Recent weight:", round(recent_weight, 2), "\n")
cat("Regime weight:", round(regime_weight, 2), "\n")
cat("Recent blocks per sim:", n_recent_blocks, "\n")
cat("Regime blocks per sim:", n_regime_blocks, "\n")

# ============================================================
# ### STEP 6: BUILD SAMPLING POOLS ###
# ============================================================

# Recent pool — last 45 trading days
recent_pool_returns <- daily_returns |>
  filter(date <= last_date) |>
  arrange(date) |>
  tail(45) |>
  mutate(log_return_1d = pmax(pmin(log_return_1d, 0.20), -0.20))

recent_pool_weeks <- recent_pool_returns |>
  group_by(week_ending) |>
  summarise(
    returns = list(log_return_1d),
    n_days  = n(),
    .groups = "drop"
  ) |>
  filter(n_days >= 4)

# Regime pool — historical analog excluding 2022-2023 spike
regime_pool_returns <- daily_returns |>
  filter(
    season         == current_season,
    vol_regime     == current_vol_reg,
    storage_regime == current_stor_reg,
    !(date >= as.Date("2022-01-01") & date <= as.Date("2023-06-30"))
  ) |>
  mutate(log_return_1d = pmax(pmin(log_return_1d, 0.20), -0.20))

regime_pool_weeks <- regime_pool_returns |>
  group_by(week_ending) |>
  summarise(
    returns = list(log_return_1d),
    n_days  = n(),
    .groups = "drop"
  ) |>
  filter(n_days >= 4)

cat("\nRecent pool weeks:", nrow(recent_pool_weeks), "\n")
cat("Regime pool weeks:", nrow(regime_pool_weeks), "\n")

# ============================================================
# ### STEP 7: RUN MONTE CARLO SIMULATION ###
# ============================================================

set.seed(42)

current_log_nom <- log(current_c1)
sim_paths       <- matrix(NA, nrow = n_sims, ncol = N)

for (sim in 1:n_sims) {
  
  # Sample blocks from recent pool (momentum)
  if (n_recent_blocks > 0 && nrow(recent_pool_weeks) > 0) {
    recent_blocks  <- sample(nrow(recent_pool_weeks),
                             n_recent_blocks, replace = TRUE)
    recent_returns <- unlist(recent_pool_weeks$returns[recent_blocks])
  } else {
    recent_returns <- numeric(0)
  }
  
  # Sample blocks from regime pool (historical analog)
  if (n_regime_blocks > 0 && nrow(regime_pool_weeks) > 0) {
    regime_blocks  <- sample(nrow(regime_pool_weeks),
                             n_regime_blocks, replace = TRUE)
    regime_returns <- unlist(regime_pool_weeks$returns[regime_blocks])
  } else {
    regime_returns <- numeric(0)
  }
  
  # Combine and shuffle blocks
  all_returns <- c(recent_returns, regime_returns)
  
  # Trim or pad to exactly N returns
  if (length(all_returns) >= N) {
    daily_rets <- all_returns[1:N]
  } else {
    extra      <- sample(unlist(regime_pool_weeks$returns),
                         N - length(all_returns), replace = TRUE)
    daily_rets <- c(all_returns, extra)
  }
  
  # Simulate price path — pure momentum, no fundamental drift
  log_price <- current_log_nom
  
  for (day in 1:N) {
    log_price           <- log_price + daily_rets[day]
    sim_paths[sim, day] <- exp(log_price)
  }
}

# ============================================================
# ### STEP 8: EXTRACT FORECAST STATISTICS ###
# ============================================================

sim_summary <- tibble(
  trading_day = 1:N,
  forecast    = apply(sim_paths, 2, median),
  lower_90    = apply(sim_paths, 2, quantile, probs = 0.10),
  lower_60    = apply(sim_paths, 2, quantile, probs = 0.20),
  upper_60    = apply(sim_paths, 2, quantile, probs = 0.80),
  upper_90    = apply(sim_paths, 2, quantile, probs = 0.90)
)

# ============================================================
# ### STEP 9: MAP TO CALENDAR DATES ###
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
# ### STEP 10: SAVE OUTPUT ###
# ============================================================

monte_carlo_objects <- list(
  sim_paths         = sim_paths,
  sim_summary       = sim_summary,
  forecast_b        = forecast_b,
  current_regime    = current_regime,
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

cat("\nMonte Carlo simulation complete\n")
cat("Regime:", current_regime, "\n")
cat("Momentum blend: recent =", round(recent_weight, 2),
    "| regime =", round(regime_weight, 2), "\n")
cat("Simulations:", n_sims, "\n")

print(forecast_b |>
        filter(trading_day %in% c(1, 5, 10, 15, 20)) |>
        select(date, forecast, lower_90, upper_90))