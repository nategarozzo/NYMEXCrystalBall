# ============================================================
# interpolate_path.R
# Prepares daily path for Shiny app visualization
# Combined forecast is already daily from combine_forecasts.R
# This script adds historical prices and saves the output object
# Inputs: data/forecasts/forecast_combined.rds
#         data/forecasts/model_a_objects.rds
#         data/raw/ng_futures_daily.rds
# Output: data/forecasts/daily_path.rds
# ============================================================

library(tidyverse)
library(lubridate)

combined         <- readRDS("data/forecasts/forecast_combined.rds")
model_a_objects  <- readRDS("data/forecasts/model_a_objects.rds")
ng_futures_daily <- readRDS("data/raw/ng_futures_daily.rds")

# ============================================================
# ### STEP 1: FORECAST ORIGIN ###
# ============================================================

last_price_date <- ng_futures_daily |>
  filter(!is.na(c1_price)) |>
  pull(date) |>
  max()

current_price <- ng_futures_daily |>
  filter(date == last_price_date) |>
  pull(c1_price)

cat("Forecast origin:", format(last_price_date), "\n")
cat("Current C1: $", round(current_price, 2), "\n")

# ============================================================
# ### STEP 2: ADD ORIGIN ROW TO DAILY PATH ###
# ============================================================

# Add today's actual price as the starting point
# so the chart connects historical to forecast seamlessly

origin_row <- tibble(
  model       = "combined",
  date        = last_price_date,
  trading_day = 0,
  forecast    = current_price,
  lower_90    = current_price,
  lower_60    = current_price,
  upper_60    = current_price,
  upper_90    = current_price,
  forecast_a  = current_price,
  forecast_b  = current_price,
  forecast_c  = current_price
)

daily_path <- bind_rows(origin_row, combined) |>
  arrange(trading_day)

# ============================================================
# ### STEP 3: HISTORICAL PRICE FOR CHART ###
# ============================================================

# 90 days of historical C1 prices for context
historical <- ng_futures_daily |>
  filter(
    date >= last_price_date - 90,
    date <= last_price_date,
    !is.na(c1_price)
  ) |>
  select(date, c1_price)

# ============================================================
# ### STEP 4: MODEL A DIAGNOSTIC INFO ###
# ============================================================

fair_value_dollars <- exp(model_a_objects$current_fair_value) *
  model_a_objects$current_cpi / 100

gap_dollars <- (exp(model_a_objects$current_log_price %||%
                      log(current_price / model_a_objects$current_cpi * 100)) -
                  exp(model_a_objects$current_fair_value)) *
  model_a_objects$current_cpi / 100

signal <- ifelse(model_a_objects$current_gap < 0,
                 "BULLISH (undervalued)",
                 "BEARISH (overvalued)")

conviction <- tanh(abs(model_a_objects$current_gap) * 3)

cat("Fair value: $", round(fair_value_dollars, 2), "\n")
cat("Signal:", signal, "\n")
cat("Conviction:", round(conviction, 3), "\n")

# ============================================================
# ### STEP 5: SAVE OUTPUT ###
# ============================================================

output <- list(
  daily_path         = daily_path,
  historical         = historical,
  origin_date        = last_price_date,
  origin_price       = current_price,
  fair_value         = fair_value_dollars,
  signal             = signal,
  conviction         = conviction,
  current_gap        = model_a_objects$current_gap,
  N                  = N,
  n_weeks            = nrow(combined |> filter(trading_day %% 5 == 0)),
  as_of              = Sys.Date()
)

saveRDS(output, "data/forecasts/daily_path.rds")

cat("\nDaily path saved:", nrow(daily_path), "days\n")
cat("Historical prices:", nrow(historical), "days\n")