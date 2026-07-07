# ============================================================
# build_supply_features.R
# Supply features for NYMEXCrystalBall
# Inputs: ng_production.rds, rig_count.rds, ng_exports.rds
# Output: data/features/supply_features.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(zoo)

ng_production <- readRDS("data/raw/ng_production.rds")
rig_count     <- readRDS("data/raw/rig_count.rds")
ng_exports    <- readRDS("data/raw/ng_exports.rds")

# ============================================================
# ### STEP 1: HELPER — SNAP MONTHLY DATES TO NEXT FRIDAY ###
# ============================================================

# Monthly EIA dates (e.g. 2026-06-01) rarely fall on Friday
# Snap each date to the next Friday so it joins correctly
# to the weekly Friday spine
# Note: wday() uses 1=Sun, 2=Mon, ... 6=Fri, 7=Sat

snap_to_friday <- function(dates) {
  map_dbl(dates, function(d) {
    d <- as.Date(d)
    days_ahead <- (6 - wday(d)) %% 7
    as.numeric(d + days_ahead)
  }) |> as.Date(origin = "1970-01-01")
}

# ============================================================
# ### STEP 2: APPLY 2-MONTH PUBLICATION LAG TO MONTHLY DATA ###
# ============================================================

# EIA Natural Gas Monthly publishes with ~2 month lag
# e.g. January data released late March
# Shift dates forward 2 months then snap to next Friday

production_lagged <- ng_production |>
  mutate(date = snap_to_friday(date %m+% months(2))) |>
  select(date, dry_production)

exports_lagged <- ng_exports |>
  mutate(date = snap_to_friday(date %m+% months(2))) |>
  select(date, total_exports, lng_exports)

# ============================================================
# ### STEP 3: CREATE WEEKLY FRIDAY SPINE ###
# ============================================================

weekly_spine <- tibble(
  week_ending = seq.Date(
    from = as.Date("2000-01-07"),
    to   = ceiling_date(Sys.Date(), "week") - 2,
    by   = "week"
  )
)

# ============================================================
# ### STEP 4: FORWARD-FILL MONTHLY DATA TO WEEKLY ###
# ============================================================

production_weekly <- weekly_spine |>
  left_join(
    production_lagged |> rename(week_ending = date),
    by = "week_ending"
  ) |>
  fill(dry_production, .direction = "down")

exports_weekly <- weekly_spine |>
  left_join(
    exports_lagged |> rename(week_ending = date),
    by = "week_ending"
  ) |>
  fill(total_exports, lng_exports, .direction = "down")

# ============================================================
# ### STEP 5: LOG TRANSFORMATIONS — PRODUCTION & EXPORTS ###
# ============================================================

production_weekly <- production_weekly |>
  mutate(log_dry_production = log(dry_production))

exports_weekly <- exports_weekly |>
  mutate(
    log_total_exports = log(total_exports),
    log_lng_exports   = log(lng_exports)
  )

# ============================================================
# ### STEP 6: RIG COUNT FEATURES ###
# ============================================================

# Snap all rig count dates to Friday to match weekly spine
# Baker Hughes occasionally publishes early on holiday weeks

rig_features <- rig_count |>
  mutate(
    week_ending = ceiling_date(week_ending, unit = "week", week_start = 6) - 1
  ) |>
  group_by(week_ending) |>
  summarise(gas_rigs = mean(gas_rigs), .groups = "drop") |>
  arrange(week_ending) |>
  mutate(
    # Feature 3: log gas rig count
    log_gas_rigs = log(gas_rigs),
    
    # Feature 17: 13-week moving average of rig count
    rig_count_13wk_ma = rollapply(gas_rigs, width = 13,
                                  FUN = mean, align = "right",
                                  fill = NA)
  )

# ============================================================
# ### STEP 7: JOIN ALL SUPPLY FEATURES ###
# ============================================================

supply_features <- weekly_spine |>
  left_join(production_weekly, by = "week_ending") |>
  left_join(exports_weekly,    by = "week_ending") |>
  left_join(rig_features,      by = "week_ending") |>
  select(
    week_ending,
    dry_production,
    log_dry_production,
    total_exports,
    log_total_exports,
    lng_exports,
    log_lng_exports,
    gas_rigs,
    log_gas_rigs,
    rig_count_13wk_ma
  ) |>
  arrange(week_ending)

saveRDS(supply_features, "data/features/supply_features.rds")