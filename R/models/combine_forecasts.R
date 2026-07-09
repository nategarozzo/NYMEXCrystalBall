# ============================================================
# combine_forecasts.R
# Combines Model A, B, and C forecasts via weighted average
# Confidence intervals computed empirically from historical C1 moves
# Excludes 2022-2023 price spike period from interval estimation
# Inputs: data/forecasts/forecast_a.rds
#         data/forecasts/forecast_b.rds
#         data/forecasts/forecast_c.rds
#         data/raw/ng_futures_daily.rds
# Output: data/forecasts/forecast_combined.rds
# ============================================================

library(tidyverse)
library(lubridate)

forecast_a       <- readRDS("data/forecasts/forecast_a.rds")
forecast_b       <- readRDS("data/forecasts/forecast_b.rds")
forecast_c       <- readRDS("data/forecasts/forecast_c.rds")
ng_futures_daily <- readRDS("data/raw/ng_futures_daily.rds")

# ============================================================
# ### STEP 1: ALIGN MODEL B TO WEEKLY ###
# ============================================================

# Model B produces 20 daily forecasts
# Take trading days 5, 10, 15, 20 as weekly waypoints
# to match Models A and C frequency

forecast_b_weekly <- forecast_b |>
  mutate(horizon = row_number()) |>
  filter(horizon %in% c(5, 10, 15, 20)) |>
  mutate(horizon = horizon / 5) |>
  select(model, horizon, forecast)

# ============================================================
# ### STEP 2: ALIGN DATES ###
# ============================================================

reference_dates <- forecast_a |>
  select(horizon, date)

forecast_b_weekly <- forecast_b_weekly |>
  left_join(reference_dates, by = "horizon") |>
  select(model, date, horizon, forecast)

# ============================================================
# ### STEP 3: DEFINE WEIGHTS ###
# ============================================================

# Fixed weights based on Baumeister et al. (2025) findings
# Model A dominates at 4-week horizon
# Update to dynamic inverse-MSPE weights once backtesting history available

weights <- c(A = 0.60, B = 0.25, C = 0.15)

cat("Model weights:\n")
cat("  Model A (BVAR):", weights["A"], "\n")
cat("  Model B (ARMA):", weights["B"], "\n")
cat("  Model C (Futures):", weights["C"], "\n")

# ============================================================
# ### STEP 4: COMBINE POINT FORECASTS ###
# ============================================================

combined <- forecast_a |>
  select(horizon, date, forecast_a = forecast) |>
  left_join(
    forecast_b_weekly |> select(horizon, forecast_b = forecast),
    by = "horizon"
  ) |>
  left_join(
    forecast_c |> select(horizon, forecast_c = forecast),
    by = "horizon"
  ) |>
  mutate(
    forecast = weights["A"] * forecast_a +
      weights["B"] * forecast_b +
      weights["C"] * forecast_c,
    model = "combined"
  ) |>
  select(model, date, horizon, forecast, forecast_a, forecast_b, forecast_c)

# ============================================================
# ### STEP 5: EMPIRICAL CONFIDENCE INTERVALS ###
# ============================================================

# Compute empirical quantiles of historical C1 price moves
# Excludes 2022-2023 spike period which inflates tail uncertainty
# Uses trading-day horizons: h=1 (5 days), h=2 (10), h=3 (15), h=4 (20)

base_data <- ng_futures_daily |>
  filter(
    date >= as.Date("2017-01-01"),
    !is.na(c1_price),
    !(date >= as.Date("2022-01-01") & date <= as.Date("2023-06-30"))
  ) |>
  arrange(date)

trading_day_horizons <- c(0, 1, 2, 3, 5, 10, 15, 20)

get_quantile <- function(h, prob) {
  if (h == 0) return(0)
  base_data |>
    mutate(move = lead(c1_price, h) - c1_price) |>
    pull(move) |>
    quantile(prob, na.rm = TRUE)
}

empirical_bands <- map_dfr(trading_day_horizons, function(h) {
  tibble(
    horizon_days = h,
    p10          = get_quantile(h, 0.10),
    p20          = get_quantile(h, 0.20),
    p80          = get_quantile(h, 0.80),
    p90          = get_quantile(h, 0.90)
  )
})

# Map weekly horizons (h=1,2,3,4) to trading day equivalents
# h=1 week = 5 trading days, h=2 = 10, h=3 = 15, h=4 = 20

combined <- combined |>
  mutate(
    trading_days = horizon * 5,
    lower_90     = forecast + approx(empirical_bands$horizon_days,
                                     empirical_bands$p10,
                                     xout = trading_days)$y,
    lower_60     = forecast + approx(empirical_bands$horizon_days,
                                     empirical_bands$p20,
                                     xout = trading_days)$y,
    upper_60     = forecast + approx(empirical_bands$horizon_days,
                                     empirical_bands$p80,
                                     xout = trading_days)$y,
    upper_90     = forecast + approx(empirical_bands$horizon_days,
                                     empirical_bands$p90,
                                     xout = trading_days)$y
  ) |>
  select(model, date, horizon, forecast,
         lower_90, lower_60, upper_60, upper_90,
         forecast_a, forecast_b, forecast_c)

# ============================================================
# ### STEP 6: SAVE OUTPUT ###
# ============================================================

saveRDS(combined, "data/forecasts/forecast_combined.rds")

print(combined)