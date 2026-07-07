# ============================================================
# align_panel.R
# Master weekly panel for NYMEXCrystalBall
# Joins all feature files into a single aligned dataset
# Output: data/features/master_panel.rds
# ============================================================

library(tidyverse)
library(lubridate)

price_features   <- readRDS("data/features/price_features.rds")
storage_features <- readRDS("data/features/storage_features.rds")
weather_features <- readRDS("data/features/weather_features.rds")
supply_features  <- readRDS("data/features/supply_features.rds")
macro_features   <- readRDS("data/features/macro_features.rds")

# ============================================================
# ### STEP 1: BUILD MASTER SPINE ###
# ============================================================

# Use full date range from 2000 to present
# Model will filter to 2017-present at estimation time

master_spine <- tibble(
  week_ending = seq.Date(
    from = as.Date("2000-01-07"),
    to   = ceiling_date(Sys.Date(), "week") - 2,
    by   = "week"
  )
)

# ============================================================
# ### STEP 2: JOIN ALL FEATURE FILES ###
# ============================================================

master_panel <- master_spine |>
  left_join(price_features,   by = "week_ending") |>
  left_join(storage_features, by = "week_ending") |>
  left_join(weather_features, by = "week_ending") |>
  left_join(supply_features,  by = "week_ending") |>
  left_join(macro_features,   by = "week_ending") |>
  arrange(week_ending)

# ============================================================
# ### STEP 3: ADD DERIVED IDENTIFIERS ###
# ============================================================

master_panel <- master_panel |>
  mutate(
    year         = year(week_ending),
    month        = month(week_ending),
    week_of_year = week(week_ending),
    in_model_window = if_else(week_ending >= as.Date("2017-01-01"), 1, 0)
  )

# ============================================================
# ### STEP 4: VALIDATE ###
# ============================================================

cat("Master panel dimensions:", nrow(master_panel), "rows x", ncol(master_panel), "cols\n")
cat("Date range:", format(min(master_panel$week_ending)), "to", format(max(master_panel$week_ending)), "\n")
cat("Model window rows:", sum(master_panel$in_model_window), "\n")
cat("\nNA counts per column:\n")
master_panel |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "na_count") |>
  filter(na_count > 0) |>
  arrange(desc(na_count)) |>
  print(n = 50)

# ============================================================
# ### STEP 5: SAVE ###
# ============================================================

saveRDS(master_panel, "data/features/master_panel.rds")

cat("\nMaster panel saved to data/features/master_panel.rds\n")