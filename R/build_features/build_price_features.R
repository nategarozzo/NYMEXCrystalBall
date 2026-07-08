# ============================================================
# build_price_features.R
# Price-based features for NYMEXCrystalBall
# Inputs: ng_futures_daily.rds, ng_weekly_futures_curve.rds
# Output: data/features/price_features.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(zoo)
library(fredr)

source("R/data_pulls/setup.R")

ng_futures_daily <- readRDS("data/raw/ng_futures_daily.rds")
weekly_curve     <- readRDS("data/raw/ng_weekly_futures_curve.rds")

# ============================================================
# ### STEP 1: ALIGN C1 PRICE TO WEEKLY (FRIDAY CLOSE) ###
# ============================================================

# Take the last available C1 price on or before each Friday
# If Friday is a holiday, use Thursday's close

c1_weekly <- ng_futures_daily |>
  filter(!is.na(c1_price)) |>
  mutate(
    week_ending = ceiling_date(date, unit = "week", week_start = 6) - 1
  ) |>
  group_by(week_ending) |>
  arrange(desc(date)) |>
  slice(1) |>
  ungroup() |>
  select(week_ending, c1_price)

# ============================================================
# ### STEP 2: LOG REAL C1 PRICE ###
# ============================================================

cpi <- fredr_series_observations(
  series_id         = "CPIAUCSL",
  observation_start = as.Date("2000-01-01"),
  frequency         = "m"
) |>
  mutate(year_month = floor_date(date, "month")) |>
  select(year_month, cpi = value) |>
  # Extend to current month so forward-fill covers recent weeks
  complete(year_month = seq(min(year_month),
                            floor_date(Sys.Date(), "month"),
                            by = "month")) |>
  fill(cpi, .direction = "down")

price_features <- c1_weekly |>
  mutate(year_month = floor_date(week_ending, "month")) |>
  left_join(cpi, by = "year_month") |>
  mutate(
    # Feature 1: log real C1 price (deflated by CPI)
    log_real_price = log(c1_price / cpi * 100),
    
    # Feature 12: log C1 price lagged 1 week
    log_price_lag1 = lag(log_real_price, 1),
    
    # Feature 13: log C1 price lagged 4 weeks
    log_price_lag4 = lag(log_real_price, 4)
  ) |>
  select(-year_month, -cpi)

# ============================================================
# ### STEP 3: ROLLING 20-DAY REALIZED VOLATILITY ###
# ============================================================

# Compute from daily C1 returns rather than spot returns

daily_returns <- ng_futures_daily |>
  filter(!is.na(c1_price)) |>
  arrange(date) |>
  mutate(
    log_return      = log(c1_price / lag(c1_price)),
    rolling_20d_vol = rollapply(log_return, width = 20,
                                FUN = sd, align = "right",
                                fill = NA, na.rm = TRUE),
    week_ending     = ceiling_date(date, unit = "week", week_start = 6) - 1
  )

weekly_vol <- daily_returns |>
  group_by(week_ending) |>
  arrange(desc(date)) |>
  slice(1) |>
  ungroup() |>
  select(week_ending, rolling_20d_vol)

# ============================================================
# ### STEP 4: FUTURES CURVE FEATURES ###
# ============================================================

curve_features <- weekly_curve |>
  rename(week_ending = date) |>
  mutate(
    c1_c4_spread  = contract4 - contract1,
    spot_c1_basis = spot - contract1
  ) |>
  select(week_ending, c1_c4_spread, spot_c1_basis)

# ============================================================
# ### STEP 5: JOIN ALL PRICE FEATURES ###
# ============================================================

price_features_final <- price_features |>
  left_join(weekly_vol, by = "week_ending") |>
  left_join(curve_features, by = "week_ending") |>
  arrange(week_ending)

saveRDS(price_features_final, "data/features/price_features.rds")

glimpse(price_features_final)