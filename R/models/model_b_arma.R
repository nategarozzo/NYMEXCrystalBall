# ============================================================
# model_b_arma.R
# Model B — Univariate ARMA(1,1) on daily log C1 futures price
# Input: data/raw/ng_futures_daily.rds
# Output: data/forecasts/forecast_b.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(fable)
library(tsibble)

ng_futures_daily <- readRDS("data/raw/ng_futures_daily.rds")

# ============================================================
# ### STEP 1: PREPARE DAILY C1 PRICE SERIES ###
# ============================================================

# Filter to training window and remove holidays/missing days
# Use trading day index rather than calendar date to avoid
# gaps from weekends and holidays breaking the tsibble

daily_ts <- ng_futures_daily |>
  filter(
    date >= as.Date("2017-01-01"),
    !is.na(c1_price)
  ) |>
  arrange(date) |>
  mutate(
    log_price   = log(c1_price),
    trading_day = row_number()
  ) |>
  as_tsibble(index = trading_day)

# ============================================================
# ### STEP 2: FIT ARIMA(1,0,1) ###
# ============================================================

# ARIMA(1,0,1) = ARMA(1,1) on log C1 price levels
# No differencing needed — stationarity confirmed by ADF test
# AR(1) captures strong price persistence
# MA(1) captures minor noise correction at h=1

fit_arma <- daily_ts |>
  model(ARIMA(log_price ~ 1 + pdq(1,0,1) + PDQ(0,0,0),
              stepwise     = FALSE,
              approximation = FALSE))

report(fit_arma)

# ============================================================
# ### STEP 3: GENERATE FORECAST AND FORMAT OUTPUT ###
# ============================================================

# Generate next 20 business days from last observed date
# excluding weekends (holidays handled implicitly)

future_dates <- seq.Date(
  from       = max(daily_ts$date) + 1,
  by         = "day",
  length.out = 40
) |>
  as_tibble() |>
  rename(date = value) |>
  mutate(day = wday(date, label = TRUE)) |>
  filter(!day %in% c("Sat", "Sun")) |>
  head(20) |>
  mutate(trading_day = max(daily_ts$trading_day) + row_number()) |>
  select(trading_day, date)

# Generate forecast and join correct calendar dates
forecast_b <- fit_arma |>
  forecast(h = 20) |>
  hilo(level = 80) |>
  as_tibble() |>
  mutate(
    lower_80 = `80%`$lower,
    upper_80 = `80%`$upper
  ) |>
  select(trading_day, forecast = .mean, lower_80, upper_80) |>
  left_join(future_dates, by = "trading_day") |>
  mutate(
    # Convert from log C1 price back to dollars
    forecast = exp(forecast),
    lower_80 = exp(lower_80),
    upper_80 = exp(upper_80),
    model    = "B"
  ) |>
  select(model, date, forecast, lower_80, upper_80)

# ============================================================
# ### STEP 4: SAVE OUTPUT ###
# ============================================================

dir.create("data/forecasts", recursive = TRUE, showWarnings = FALSE)

saveRDS(forecast_b, "data/forecasts/forecast_b.rds")

print(forecast_b, n = 20)