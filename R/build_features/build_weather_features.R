# ============================================================
# build_weather_features.R
# Weather features for NYMEXCrystalBall
# Inputs: degree_days_weekly.rds
# Output: data/features/weather_features.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(httr2)

degree_days_weekly <- readRDS("data/raw/degree_days_weekly.rds")

# ============================================================
# ### STEP 1: FILTER TO US TOTAL ###
# ============================================================

dd_us <- degree_days_weekly |>
  filter(region == "us_total") |>
  select(week_ending, type, weight, value)

# ============================================================
# ### STEP 2: COMPUTE 10-YEAR HISTORICAL AVERAGE ###
# ============================================================

# For each week, find the closest observation within ±4 days
# across the prior 10 years to handle 364 vs 365 day drift
# Same approach validated in build_storage_features.R

compute_10yr_avg <- function(df) {
  df |>
    arrange(week_ending) |>
    mutate(
      historical_avg = map_dbl(week_ending, function(d) {
        targets <- d - years(1:10)
        vals <- map_dbl(targets, function(t) {
          closest <- df |>
            filter(abs(as.numeric(week_ending - t)) <= 4) |>
            arrange(abs(as.numeric(week_ending - t))) |>
            slice(1) |>
            pull(value)
          if (length(closest) == 0) NA_real_ else closest
        })
        if (sum(!is.na(vals)) < 10) NA_real_ else mean(vals, na.rm = TRUE)
      }),
      deviation = value - historical_avg
    )
}

# Apply separately to each series
dd_with_devs <- dd_us |>
  group_by(type, weight) |>
  group_modify(~ compute_10yr_avg(.x)) |>
  ungroup()

# ============================================================
# ### STEP 3: PIVOT TO WIDE FORMAT ###
# ============================================================

weather_features <- dd_with_devs |>
  mutate(series = case_when(
    type == "Heating" & weight == "Population"  ~ "hdd_pop",
    type == "Cooling" & weight == "Population"  ~ "cdd_pop",
    type == "Heating" & weight == "UtilityGas"  ~ "hdd_gas"
  )) |>
  select(week_ending, series, value, deviation) |>
  pivot_wider(
    names_from  = series,
    values_from = c(value, deviation)
  ) |>
  rename(
    hdd_pop     = value_hdd_pop,
    cdd_pop     = value_cdd_pop,
    hdd_gas     = value_hdd_gas,
    hdd_pop_dev = deviation_hdd_pop,
    cdd_pop_dev = deviation_cdd_pop,
    hdd_gas_dev = deviation_hdd_gas
  ) |>
  arrange(week_ending)

saveRDS(weather_features, "data/features/weather_features.rds")

glimpse(weather_features)