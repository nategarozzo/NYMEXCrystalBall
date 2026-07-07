library(tidyverse)
library(eia)
library(fredr)

source("~/NYMEXCrystalBall/R/data_pulls/setup.R")

# ================================================================
# ### WEEKLY STORAGE BY US REGION ###
# ================================================================

pull_ng_storage <- function() {
  series <- c(
    east          = "NW2_EPG0_SWO_R31_BCF",
    midwest       = "NW2_EPG0_SWO_R32_BCF",
    south_central = "NW2_EPG0_SWO_R33_BCF",
    mountain      = "NW2_EPG0_SWO_R34_BCF",
    pacific       = "NW2_EPG0_SWO_R35_BCF",
    total         = "NW2_EPG0_SWO_R48_BCF"
  )
  
  map_dfr(names(series), function(s) {
    eia_data(
      dir    = "natural-gas/stor/wkly",
      data   = "value",
      facets = list(series = series[[s]]),
      freq   = "weekly",
      start  = "2010-01-01"
    ) |>
      mutate(
        region = s,
        date   = as.Date(period),
        value  = as.numeric(value)
      ) |>
      select(date, region, value)
  }) |>
    pivot_wider(names_from = region, values_from = value) |>
    select(date, east, midwest, south_central, mountain, pacific, total) |>
    arrange(date)
}

storage_clean <- pull_ng_storage()

saveRDS(storage_clean, "data/raw/ng_storage.rds")


