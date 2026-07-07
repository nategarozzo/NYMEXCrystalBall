# ============================================================
# build_storage_features.R
# Storage features for NYMEXCrystalBall
# Inputs: ng_storage.rds
# Output: data/features/storage_features.rds
# ============================================================

library(tidyverse)
library(lubridate)

ng_storage <- readRDS("data/raw/ng_storage.rds")

# ============================================================
# ### STEP 1: RENAME DATE TO WEEK_ENDING ###
# ============================================================

# Storage data is dated Friday (as-of date)
# Friday close is our panel reference day — no shift needed

storage <- ng_storage |>
  rename(week_ending = date) |>
  arrange(week_ending)

# ============================================================
# ### STEP 2: LOG TOTAL STORAGE ###
# ============================================================

storage <- storage |>
  mutate(
    # Feature 4: log total working gas in storage (Bcf)
    log_storage = log(total)
  )

# ============================================================
# ### STEP 3: YEAR-OVER-YEAR CHANGE ###
# ============================================================

# 52-week lag works reliably for 1-year lookback
storage <- storage |>
  mutate(
    storage_yoy = total - lag(total, 52)
  )

# ============================================================
# ### STEP 4: 5-YEAR AVERAGE AND SURPLUS/DEFICIT ###
# ============================================================

# Use date-based nearest-neighbor matching within ±4 days
# to handle the 364 vs 365 day drift across years

compute_5yr_avg <- function(df) {
  df |>
    mutate(
      storage_5yr_avg = map_dbl(week_ending, function(d) {
        targets <- d - years(1:5)
        vals <- map_dbl(targets, function(t) {
          closest <- df |>
            filter(abs(as.numeric(week_ending - t)) <= 4) |>
            arrange(abs(as.numeric(week_ending - t))) |>
            slice(1) |>
            pull(total)
          if (length(closest) == 0) NA_real_ else closest
        })
        if (sum(!is.na(vals)) < 5) NA_real_ else mean(vals, na.rm = TRUE)
      }),
      storage_5yr_surplus = total - storage_5yr_avg
    )
}

storage <- compute_5yr_avg(storage)

# ============================================================
# ### STEP 5: SELECT FINAL FEATURES ###
# ============================================================

storage_features <- storage |>
  select(
    week_ending,
    total,
    log_storage,
    storage_yoy,
    storage_5yr_avg,
    storage_5yr_surplus
  ) |>
  arrange(week_ending)

saveRDS(storage_features, "data/features/storage_features.rds")