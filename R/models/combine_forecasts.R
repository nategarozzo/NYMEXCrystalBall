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

# n_weeks and N set in run_forecast.R

# ============================================================
# ### STEP 1: ALIGN MODEL B TO WEEKLY ###
# ============================================================

# Model B produces N daily forecasts
# Take every 5th trading day as weekly waypoint to match
# Models A and C frequency

forecast_b_weekly <- forecast_b |>
  mutate(horizon = row_number()) |>
  filter(horizon %% 5 == 0) |>
  mutate(horizon = horizon / 5) |>
  filter(horizon <= n_weeks) |>
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
# ### STEP 3: DEFINE HORIZON-VARYING WEIGHTS ###
# ============================================================

# Weights shift with horizon — ARMA dominates short term,
# fair value model dominates longer term
# Model C contributes evenly throughout

get_weights <- function(h) {
  if (h <= 1)      c(A = 0.10, B = 0.70, C = 0.20)
  else if (h <= 2) c(A = 0.25, B = 0.55, C = 0.20)
  else if (h <= 3) c(A = 0.45, B = 0.35, C = 0.20)
  else if (h <= 4) c(A = 0.60, B = 0.20, C = 0.20)
  else             c(A = 0.70, B = 0.10, C = 0.20)
}

cat("Model weights by horizon:\n")
for (h in 1:min(n_weeks, 5)) {
  w <- get_weights(h)
  cat("  Week", h, "— A:", w["A"], "B:", w["B"], "C:", w["C"], "\n")
}

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
  rowwise() |>
  mutate(
    w        = list(get_weights(horizon)),
    forecast = w["A"] * forecast_a +
      w["B"] * forecast_b +
      w["C"] * forecast_c,
    model    = "combined"
  ) |>
  ungroup() |>
  select(model, date, horizon, forecast, forecast_a, forecast_b, forecast_c)

# ============================================================
# ### STEP 5: EMPIRICAL CONFIDENCE INTERVALS ###
# ============================================================

# Compute empirical quantiles of historical C1 price moves
# Excludes 2022-2023 spike period
# Extends to N trading days

base_data <- ng_futures_daily |>
  filter(
    date >= as.Date("2017-01-01"),
    !is.na(c1_price),
    !(date >= as.Date("2022-01-01") & date <= as.Date("2023-06-30"))
  ) |>
  arrange(date)

get_quantile <- function(h, prob) {
  if (h == 0) return(0)
  base_data |>
    mutate(move = lead(c1_price, h) - c1_price) |>
    pull(move) |>
    quantile(prob, na.rm = TRUE)
}

# Build empirical bands at fine-grained trading day intervals up to N
trading_day_horizons <- unique(c(0, 1, 2, 3, seq(5, N, by = 5)))

empirical_bands <- map_dfr(trading_day_horizons, function(h) {
  tibble(
    horizon_days = h,
    p10          = get_quantile(h, 0.10),
    p20          = get_quantile(h, 0.20),
    p80          = get_quantile(h, 0.80),
    p90          = get_quantile(h, 0.90)
  )
})

combined <- combined |>
  mutate(
    trading_days = horizon * 5,
    lower_90     = forecast + approx(empirical_bands$horizon_days,
                                     empirical_bands$p10,
                                     xout = trading_days, rule = 2)$y,
    lower_60     = forecast + approx(empirical_bands$horizon_days,
                                     empirical_bands$p20,
                                     xout = trading_days, rule = 2)$y,
    upper_60     = forecast + approx(empirical_bands$horizon_days,
                                     empirical_bands$p80,
                                     xout = trading_days, rule = 2)$y,
    upper_90     = forecast + approx(empirical_bands$horizon_days,
                                     empirical_bands$p90,
                                     xout = trading_days, rule = 2)$y
  ) |>
  select(model, date, horizon, forecast,
         lower_90, lower_60, upper_60, upper_90,
         forecast_a, forecast_b, forecast_c)

# ============================================================
# ### STEP 6: SAVE OUTPUT ###
# ============================================================

saveRDS(combined, "data/forecasts/forecast_combined.rds")

print(combined)