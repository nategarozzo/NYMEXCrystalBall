# ============================================================
# model_c_futures.R
# Model C — Futures curve implied price path
# Input: data/raw/ng_weekly_futures_curve.rds (historical)
#         Manual current curve input (update weekly)
# Output: data/forecasts/forecast_c.rds
# ============================================================

library(tidyverse)
library(lubridate)

weekly_curve <- readRDS("data/raw/ng_weekly_futures_curve.rds")

# ============================================================
# ### STEP 1: CURRENT FUTURES CURVE — UPDATE WEEKLY ###
# ============================================================

# Source: CME Group NYMEX Henry Hub Natural Gas settlements
# https://www.cmegroup.com/markets/energy/natural-gas/natural-gas.html
# Update every Friday before running run_forecast.R

current_curve <- tibble(
  contract = c("C1", "C2", "C3", "C4"),
  price    = c(3.20, 3.35, 3.48, 3.52),  # update weekly
  as_of    = as.Date("2026-07-04")        # update weekly
)

F1 <- current_curve |> filter(contract == "C1") |> pull(price)
F2 <- current_curve |> filter(contract == "C2") |> pull(price)

# ============================================================
# ### STEP 2: GENERATE WEEKLY FORECAST PATH ###
# ============================================================

# Linear interpolation from C1 today to C2 at h=4 weeks
# h=1: 1/4 of the way from C1 to C2
# h=2: 2/4 of the way from C1 to C2
# h=3: 3/4 of the way from C1 to C2
# h=4: fully at C2 (prompt month has rolled to September)

forecast_path <- tibble(horizon = 1:4) |>
  mutate(
    forecast = F1 + (horizon / 4) * (F2 - F1)
  )

# ============================================================
# ### STEP 3: ESTIMATE HISTORICAL FORECAST ERRORS ###
# ============================================================

# For each week in historical data compute what Model C
# would have forecast vs what C1 actually did h weeks later
# Standard deviation of those errors gives sigma at each horizon

historical_errors <- weekly_curve |>
  filter(!is.na(contract1), !is.na(contract2)) |>
  arrange(date)

horizon_errors <- map_dfr(1:4, function(h) {
  historical_errors |>
    mutate(
      forecast_h = contract1 + (h / 4) * (contract2 - contract1),
      actual_h   = lead(contract1, h),
      error_h    = actual_h - forecast_h
    ) |>
    filter(!is.na(error_h)) |>
    summarise(
      horizon = h,
      sigma_h = sd(error_h, na.rm = TRUE)
    )
})

horizon_errors

# ============================================================
# ### STEP 4: BUILD FORECAST OBJECT WITH CONFIDENCE BAND ###
# ============================================================

forecast_c <- forecast_path |>
  left_join(horizon_errors, by = "horizon") |>
  mutate(
    lower_80 = forecast - 1.282 * sigma_h,
    upper_80 = forecast + 1.282 * sigma_h,
    date     = current_curve$as_of[1] + (horizon * 7),
    model    = "C"
  ) |>
  select(model, date, horizon, forecast, lower_80, upper_80)

# ============================================================
# ### STEP 5: SAVE OUTPUT ###
# ============================================================

dir.create("data/forecasts", recursive = TRUE, showWarnings = FALSE)

saveRDS(forecast_c, "data/forecasts/forecast_c.rds")