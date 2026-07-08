# ============================================================
# build_macro_features.R
# Macro features for NYMEXCrystalBall
# Inputs: FRED TCU (capacity utilization)
# Output: data/features/macro_features.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(fredr)

source("R/data_pulls/setup.R")

# ============================================================
# ### STEP 1: HELPER — SNAP MONTHLY DATES TO NEXT FRIDAY ###
# ============================================================

# Same function used in build_supply_features.R
# wday() uses 1=Sun, 2=Mon, ... 6=Fri, 7=Sat

snap_to_friday <- function(dates) {
  map_dbl(dates, function(d) {
    d <- as.Date(d)
    days_ahead <- (6 - wday(d)) %% 7
    as.numeric(d + days_ahead)
  }) |> as.Date(origin = "1970-01-01")
}

# ============================================================
# ### STEP 2: PULL CAPACITY UTILIZATION FROM FRED ###
# ============================================================

# TCU: Total Capacity Utilization
# Monthly, published with ~1 month lag
# Federal Reserve via FRED

tcu_raw <- fredr_series_observations(
  series_id         = "TCU",
  observation_start = as.Date("2000-01-01"),
  frequency         = "m"
) |>
  select(date, tcu = value)

# ============================================================
# ### STEP 3: APPLY 1-MONTH PUBLICATION LAG ###
# ============================================================

# Federal Reserve publishes capacity utilization with ~1 month lag
# e.g. January data released mid-February
# Shift forward 1 month then snap to next Friday

tcu_lagged <- tcu_raw |>
  mutate(date = snap_to_friday(date %m+% months(1))) |>
  select(date, tcu)

# ============================================================
# ### STEP 4: CREATE WEEKLY FRIDAY SPINE ###
# ============================================================

weekly_spine <- tibble(
  week_ending = seq.Date(
    from = as.Date("2000-01-07"),
    to   = ceiling_date(Sys.Date(), "week") - 2,
    by   = "week"
  )
)

# ============================================================
# ### STEP 5: FORWARD-FILL TO WEEKLY ###
# ============================================================

tcu_weekly <- weekly_spine |>
  left_join(
    tcu_lagged |> rename(week_ending = date),
    by = "week_ending"
  ) |>
  fill(tcu, .direction = "down")

# ============================================================
# ### STEP 6: POST-INVASION INDICATOR ###
# ============================================================

# Binary variable = 1 from week of February 28, 2022 onward
# Captures structural shift in Henry Hub - TTF relationship
# and elevated global LNG demand following Russia's invasion of Ukraine

tcu_weekly <- tcu_weekly |>
  mutate(
    post_invasion = if_else(week_ending >= as.Date("2022-02-25"), 1, 0)
  )

# ============================================================
# ### STEP 7: SEASONAL INDICATOR  ###
# ============================================================

# Feature 18: meteorological season indicator
# Winter = Dec-Feb (high heating demand)
# Spring = Mar-May (shoulder season)
# Summer = Jun-Aug (high cooling/power burn demand)
# Fall   = Sep-Nov (shoulder season, storage injection wind-down)
# Spring is the reference category (omitted)

macro_features <- tcu_weekly |>
  mutate(
    month        = month(week_ending),
    season_winter = if_else(month %in% c(12, 1, 2), 1, 0),
    season_summer = if_else(month %in% c(6, 7, 8),  1, 0),
    season_fall   = if_else(month %in% c(9, 10, 11), 1, 0)
  ) |>
  select(
    week_ending,
    tcu,
    post_invasion,
    season_winter,
    season_summer,
    season_fall
  ) |>
  arrange(week_ending)

saveRDS(macro_features, "data/features/macro_features.rds")