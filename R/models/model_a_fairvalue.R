# ============================================================
# model_a_fairvalue.R
# Model A — Fundamental Fair Value + Mean Reversion
# Input: data/features/master_panel.rds
#        data/raw/ng_futures_daily.rds
# Output: data/forecasts/forecast_a.rds
#         data/forecasts/model_a_objects.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(fredr)

source("R/data_pulls/setup.R")

master_panel     <- readRDS("data/features/master_panel.rds")
ng_futures_daily <- readRDS("data/raw/ng_futures_daily.rds")

# ============================================================
# ### STEP 1: FORECAST HORIZON ###
# ============================================================

# N = number of trading days to forecast ahead
# N and n_weeks set in run_forecast.R before sourcing this script

n_weeks <- ceiling(N / 5)

cat("Forecast horizon:", N, "trading days (", n_weeks, "weeks)\n")

# ============================================================
# ### STEP 2: PREPARE TRAINING DATA ###
# ============================================================

train_data <- master_panel |>
  filter(in_model_window == 1) |>
  select(
    week_ending,
    log_real_price,
    storage_5yr_surplus,
    storage_yoy,
    tcu,
    hdd_pop_dev,
    log_gas_rigs,
    season_fall
  ) |>
  drop_na()

cat("Training observations:", nrow(train_data), "\n")
cat("Training window:", format(min(train_data$week_ending)),
    "to", format(max(train_data$week_ending)), "\n")

# ============================================================
# ### STEP 3: FIT FAIR VALUE REGRESSION ###
# ============================================================

fair_value_model <- lm(
  log_real_price ~ storage_5yr_surplus + storage_yoy +
    tcu + hdd_pop_dev + log_gas_rigs + season_fall,
  data = train_data
)

cat("\nR-squared:", round(summary(fair_value_model)$r.squared, 3), "\n")

# ============================================================
# ### STEP 4: ESTIMATE MEAN REVERSION SPEED (PHI) ###
# ============================================================

train_data <- train_data |>
  mutate(
    fair_value = predict(fair_value_model),
    gap        = log_real_price - fair_value
  )

phi_model <- lm(gap ~ lag(gap, 1) - 1, data = train_data)
phi       <- coef(phi_model)["lag(gap, 1)"]

cat("Phi:", round(phi, 4), "\n")
cat("Half-life:", round(log(0.5) / log(phi), 1), "weeks\n")

# ============================================================
# ### STEP 5: CURRENT FAIR VALUE AND GAP ###
# ============================================================

# Get current CPI
current_cpi <- fredr_series_observations(
  series_id         = "CPIAUCSL",
  observation_start = as.Date("2026-01-01"),
  frequency         = "m"
) |>
  filter(!is.na(value)) |>
  arrange(desc(date)) |>
  slice(1) |>
  pull(value)

# Most recent value of each fundamental variable independently
# Uses best available data for each variable up to latest C1 date
current_fundamentals <- tibble(
  storage_5yr_surplus = master_panel |>
    filter(!is.na(storage_5yr_surplus)) |>
    arrange(desc(week_ending)) |>
    slice(1) |> pull(storage_5yr_surplus),
  
  storage_yoy = master_panel |>
    filter(!is.na(storage_yoy)) |>
    arrange(desc(week_ending)) |>
    slice(1) |> pull(storage_yoy),
  
  tcu = master_panel |>
    filter(!is.na(tcu)) |>
    arrange(desc(week_ending)) |>
    slice(1) |> pull(tcu),
  
  hdd_pop_dev = master_panel |>
    filter(!is.na(hdd_pop_dev)) |>
    arrange(desc(week_ending)) |>
    slice(1) |> pull(hdd_pop_dev),
  
  log_gas_rigs = master_panel |>
    filter(!is.na(log_gas_rigs)) |>
    arrange(desc(week_ending)) |>
    slice(1) |> pull(log_gas_rigs),
  
  season_fall = if_else(month(Sys.Date()) %in% c(9, 10, 11), 1, 0)
)

# Fair value from current fundamentals
current_fair_value <- predict(fair_value_model,
                              newdata = current_fundamentals)

# Latest C1 price as forecast origin
latest_c1 <- ng_futures_daily |>
  filter(!is.na(c1_price)) |>
  arrange(desc(date)) |>
  slice(1) |>
  pull(c1_price)

forecast_origin  <- ng_futures_daily |>
  filter(!is.na(c1_price)) |>
  pull(date) |>
  max()

latest_log_price <- log(latest_c1 / current_cpi * 100)
current_gap      <- latest_log_price - current_fair_value

cat("\nFair value ($):",
    round(exp(current_fair_value) * current_cpi / 100, 2), "\n")
cat("Current C1 ($):", round(latest_c1, 2), "\n")
cat("Gap ($):",
    round((exp(latest_log_price) -
             exp(current_fair_value)) * current_cpi / 100, 2), "\n")
cat("Signal:", ifelse(current_gap < 0,
                      "BULLISH (undervalued)",
                      "BEARISH (overvalued)"), "\n")

# ============================================================
# ### STEP 6: GENERATE WEEKLY FORECAST ###
# ============================================================

forecast_a <- map_dfr(1:n_weeks, function(h) {
  log_forecast <- current_fair_value + phi^h * current_gap
  tibble(
    model    = "A",
    horizon  = h,
    date     = forecast_origin + (h * 7),
    forecast = exp(log_forecast) * current_cpi / 100
  )
})

print(forecast_a)

# ============================================================
# ### STEP 7: SAVE OUTPUT ###
# ============================================================

model_a_objects <- list(
  fair_value_model   = fair_value_model,
  phi                = phi,
  current_fair_value = current_fair_value,
  current_gap        = current_gap,
  current_cpi        = current_cpi,
  forecast_origin    = forecast_origin,
  latest_c1          = latest_c1,
  forecast           = forecast_a,
  N                  = N,
  n_weeks            = n_weeks
)

dir.create("data/forecasts", recursive = TRUE, showWarnings = FALSE)

saveRDS(forecast_a,      "data/forecasts/forecast_a.rds")
saveRDS(model_a_objects, "data/forecasts/model_a_objects.rds")