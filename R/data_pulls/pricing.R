library(tidyverse)
library(eia)
library(fredr)

source("~/NYMEXCrystalBall/R/data_pulls/setup.R")

# ============================================================
# ### DAILY HENRY HUB SPOT PRICES ###
# ============================================================

pull_spot_daily <- function() {
  fredr_series_observations(
    series_id         = "DHHNGSP",
    observation_start = as.Date("2000-01-01")
  ) |>
    select(date, value) |>
    rename(spot_price = value)
}

spot_daily <- pull_spot_daily()

saveRDS(spot_daily, "data/raw/spot_daily.rds")

# ============================================================
# ### DAILY NYMEX C1 FUTURES PRICE (NG=F) ###
# ============================================================

library(tidyquant)

pull_ng_futures_daily <- function() {
  tq_get(
    "NG=F",
    from = "2000-01-01",
    to   = Sys.Date()
  ) |>
    select(date, c1_price = close)
}

ng_futures_daily <- pull_ng_futures_daily()

saveRDS(ng_futures_daily, "data/raw/ng_futures_daily.rds")
# ============================================================
# ### MONTHLY FRONT-MONTH FUTURES PRICES ###
# ============================================================

pull_futures_monthly <- function() {
  fredr_series_observations(
    series_id         = "MHHNGSP",
    observation_start = as.Date("2000-01-01")
  ) |>
    select(date, value) |>
    rename(front_month = value)
}

futures_monthly <- pull_futures_monthly()

saveRDS(futures_monthly, "data/raw/futures_monthly.rds")

# ============================================================
# ### WEEKLY SPOT AND FORWARD CURVE (CONTRACTS 1-4) ###
# ============================================================

pull_weekly_curve <- function() {
  series <- c(
    spot      = "RNGWHHD",
    contract1 = "RNGC1",
    contract2 = "RNGC2",
    contract3 = "RNGC3",
    contract4 = "RNGC4"
  )
  
  map_dfr(names(series), function(s) {
    eia_data(
      dir    = "natural-gas/pri/fut",
      data   = "value",
      facets = list(series = series[[s]]),
      freq   = "weekly",
      start  = "2000-01-01"
    ) |>
      mutate(
        contract = s,
        date     = as.Date(period),
        value    = as.numeric(value)
      ) |>
      select(date, contract, value)
  }) |>
    pivot_wider(names_from = contract, values_from = value) |>
    arrange(date)
}

weekly_curve <- pull_weekly_curve()

saveRDS(weekly_curve, "data/raw/ng_weekly_futures_curve.rds")

# ============================================================
# ### DAILY WTI CRUDE OIL PRICE ###
# ============================================================

pull_wti_price <- function() {
  fredr_series_observations(
    series_id         = "DCOILWTICO",
    observation_start = as.Date("2000-01-01")
  ) |>
    select(date, value) |>
    rename(wti_price = value)
}

wti_clean <- pull_wti_price()

saveRDS(wti_clean, "data/raw/wti_price.rds")