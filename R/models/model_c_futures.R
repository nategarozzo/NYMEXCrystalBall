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
F3 <- current_curve |> filter(contract == "C3") |> pull(price)
F4 <- current_curve |> filter(contract == "C4") |> pull(price)

# ============================================================
# ### STEP 2: GENERATE WEEKLY FORECAST PATH ###
# ============================================================

# Map each weekly horizon to position along the futures curve
# Weeks 1-4:  interpolate C1 → C2
# Weeks 5-8:  interpolate C2 → C3
# Weeks 9-12: interpolate C3 → C4
# Beyond 12:  hold at C4 (no further curve data)

# n_weeks set in run_forecast.R

get_curve_price <- function(h) {
  if (h <= 4) {
    F1 + (h / 4) * (F2 - F1)
  } else if (h <= 8) {
    F2 + ((h - 4) / 4) * (F3 - F2)
  } else if (h <= 12) {
    F3 + ((h - 8) / 4) * (F4 - F3)
  } else {
    F4  # hold at C4 beyond 12 weeks
  }
}

forecast_c <- tibble(horizon = 1:n_weeks) |>
  mutate(
    forecast = map_dbl(horizon, get_curve_price),
    date     = current_curve$as_of[1] + (horizon * 7),
    model    = "C"
  ) |>
  select(model, date, horizon, forecast)

# ============================================================
# ### STEP 3: SAVE OUTPUT ###
# ============================================================

dir.create("data/forecasts", recursive = TRUE, showWarnings = FALSE)

saveRDS(forecast_c, "data/forecasts/forecast_c.rds")

print(forecast_c)