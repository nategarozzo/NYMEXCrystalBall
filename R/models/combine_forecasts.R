# ============================================================
# combine_forecasts.R
# Combines Model A, B, and C forecasts via weighted average
# Model B (Monte Carlo) provides daily path texture and band width
# Models A and C provide weekly fundamental and market anchors
# Band centered on combined forecast with asymmetric scaling
# Inputs: data/forecasts/forecast_a.rds
#         data/forecasts/forecast_b.rds
#         data/forecasts/forecast_c.rds
#         data/forecasts/model_a_objects.rds
# Output: data/forecasts/forecast_combined.rds
# ============================================================

library(tidyverse)
library(lubridate)

forecast_a      <- readRDS("data/forecasts/forecast_a.rds")
forecast_b      <- readRDS("data/forecasts/forecast_b.rds")
forecast_c      <- readRDS("data/forecasts/forecast_c.rds")
model_a_objects <- readRDS("data/forecasts/model_a_objects.rds")

# N and n_weeks set in run_forecast.R

# ============================================================
# ### STEP 1: INTERPOLATE MODEL A TO DAILY ###
# ============================================================

current_c1 <- model_a_objects$latest_c1

anchor_trading_days <- c(0, forecast_a$horizon * 5)
anchor_prices_a     <- c(current_c1, forecast_a$forecast)

forecast_a_daily <- tibble(trading_day = 1:N) |>
  mutate(
    forecast_a = approx(anchor_trading_days, anchor_prices_a,
                        xout = trading_day, rule = 2)$y
  )

# ============================================================
# ### STEP 2: INTERPOLATE MODEL C TO DAILY ###
# ============================================================

anchor_prices_c <- c(current_c1, forecast_c$forecast)

forecast_c_daily <- tibble(trading_day = 1:N) |>
  mutate(
    forecast_c = approx(anchor_trading_days, anchor_prices_c,
                        xout = trading_day, rule = 2)$y
  )

# ============================================================
# ### STEP 3: DEFINE HORIZON-VARYING WEIGHTS ###
# ============================================================

# Short term: Monte Carlo (B) dominates — momentum and volatility texture
# Long term: Fair value (A) dominates — fundamental mean reversion
# Futures curve (C) contributes evenly throughout

get_weights <- function(trading_day) {
  week <- ceiling(trading_day / 5)
  if (week <= 1)      c(A = 0.10, B = 0.70, C = 0.20)
  else if (week <= 2) c(A = 0.25, B = 0.55, C = 0.20)
  else if (week <= 3) c(A = 0.45, B = 0.35, C = 0.20)
  else if (week <= 4) c(A = 0.60, B = 0.20, C = 0.20)
  else                c(A = 0.70, B = 0.10, C = 0.20)
}

# ============================================================
# ### STEP 4: ASYMMETRIC BAND PARAMETERS ###
# ============================================================

# Model A conviction determines band asymmetry
# Bullish signal: widen upside, shrink downside
# Bearish signal: widen downside, shrink upside

conviction <- tanh(abs(model_a_objects$current_gap) * 3)
direction  <- sign(-model_a_objects$current_gap)
asym_upper <- 1 + direction * conviction * 0.4
asym_lower <- 1 - direction * conviction * 0.4

cat("Model A signal:", ifelse(direction > 0, "BULLISH", "BEARISH"), "\n")
cat("Conviction:", round(conviction, 3), "\n")
cat("Upper band scale:", round(asym_upper, 3), "\n")
cat("Lower band scale:", round(asym_lower, 3), "\n")

# ============================================================
# ### STEP 5: COMBINE DAILY FORECASTS ###
# ============================================================

combined <- forecast_b |>
  select(date, trading_day,
         forecast_b = forecast,
         lower_90_b = lower_90,
         lower_60_b = lower_60,
         upper_60_b = upper_60,
         upper_90_b = upper_90) |>
  left_join(forecast_a_daily, by = "trading_day") |>
  left_join(forecast_c_daily, by = "trading_day") |>
  rowwise() |>
  mutate(
    w        = list(get_weights(trading_day)),
    
    # Combined forecast — weighted blend of all three models
    forecast = w["A"] * forecast_a +
      w["B"] * forecast_b +
      w["C"] * forecast_c,
    
    # Band half-widths from base Monte Carlo simulation
    # These represent honest uncertainty without amplification
    half_width_up   = upper_90_b - forecast_b,
    half_width_down = forecast_b  - lower_90_b,
    half_width_up60 = upper_60_b - forecast_b,
    half_width_dn60 = forecast_b  - lower_60_b,
    
    # Center band on combined forecast with asymmetric scaling
    lower_90 = forecast - half_width_down * asym_lower,
    upper_90 = forecast + half_width_up   * asym_upper,
    lower_60 = forecast - half_width_dn60 * asym_lower,
    upper_60 = forecast + half_width_up60 * asym_upper,
    
    model = "combined"
  ) |>
  ungroup() |>
  select(model, date, trading_day, forecast,
         lower_90, lower_60, upper_60, upper_90,
         forecast_a, forecast_b, forecast_c)

# ============================================================
# ### STEP 6: SAVE OUTPUT ###
# ============================================================

saveRDS(combined, "data/forecasts/forecast_combined.rds")

cat("\nCombined forecast summary:\n")
print(combined |>
        filter(trading_day %in% c(1, 5, 10, 15, 20)) |>
        select(date, trading_day, forecast, forecast_a,
               forecast_b, forecast_c, lower_90, upper_90))