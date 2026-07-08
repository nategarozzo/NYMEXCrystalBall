# ============================================================
# combine_forecasts.R
# Combines Model A, B, and C forecasts via weighted average
# Inputs: data/forecasts/forecast_a.rds
#         data/forecasts/forecast_b.rds
#         data/forecasts/forecast_c.rds
# Output: data/forecasts/forecast_combined.rds
# ============================================================

library(tidyverse)
library(lubridate)

forecast_a <- readRDS("data/forecasts/forecast_a.rds")
forecast_b <- readRDS("data/forecasts/forecast_b.rds")
forecast_c <- readRDS("data/forecasts/forecast_c.rds")

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
  select(model, horizon, forecast, lower_80, upper_80)

# ============================================================
# ### STEP 2: ALIGN DATES ###
# ============================================================

# Use Model A dates as the reference since it has
# the most current forecast origin after forward-fill fix

reference_dates <- forecast_a |>
  select(horizon, date)

forecast_b_weekly <- forecast_b_weekly |>
  left_join(reference_dates, by = "horizon") |>
  select(model, date, horizon, forecast, lower_80, upper_80)

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
# ### STEP 4: COMBINE FORECASTS ###
# ============================================================

# Join all three models by horizon
# Compute weighted average of point forecasts and interval bounds

combined <- forecast_a |>
  select(horizon, date,
         forecast_a = forecast,
         lower_a    = lower_80,
         upper_a    = upper_80) |>
  left_join(
    forecast_b_weekly |>
      select(horizon,
             forecast_b = forecast,
             lower_b    = lower_80,
             upper_b    = upper_80),
    by = "horizon"
  ) |>
  left_join(
    forecast_c |>
      select(horizon,
             forecast_c = forecast,
             lower_c    = lower_80,
             upper_c    = upper_80),
    by = "horizon"
  ) |>
  mutate(
    # Combined point forecast
    forecast = weights["A"] * forecast_a +
      weights["B"] * forecast_b +
      weights["C"] * forecast_c,
    
    # Combined lower bound
    lower_80 = weights["A"] * lower_a +
      weights["B"] * lower_b +
      weights["C"] * lower_c,
    
    # Combined upper bound
    upper_80 = weights["A"] * upper_a +
      weights["B"] * upper_b +
      weights["C"] * upper_c,
    
    model = "combined"
  ) |>
  select(model, date, horizon, forecast, lower_80, upper_80,
         forecast_a, forecast_b, forecast_c)

# ============================================================
# ### STEP 5: SAVE OUTPUT ###
# ============================================================

saveRDS(combined, "data/forecasts/forecast_combined.rds")

print(combined)