library(tidyverse)
library(eia)
library(fredr)
library(readxl)

source("~/NYMEXCrystalBall/R/data_pulls/setup.R")

# ============================================================
# ### MONTHLY NATURAL GAS PRODUCTION ###
# ============================================================

pull_ng_production <- function() {
  processes <- c(
    dry_production      = "FPD",
    gross_withdrawals   = "FGW",
    marketed_production = "VGM",
    shale_withdrawals   = "FGS"
  )
  
  map_dfr(names(processes), function(p) {
    eia_data(
      dir    = "natural-gas/prod/sum",
      data   = "value",
      facets = list(duoarea = "NUS", process = processes[[p]]),
      freq   = "monthly",
      start  = "2000-01"
    ) |>
      filter(units == "MMCF") |>
      mutate(
        series = p,
        date   = as.Date(paste0(period, "-01")),
        value  = as.numeric(value)
      ) |>
      select(date, series, value)
  }) |>
    pivot_wider(names_from = series, values_from = value) |>
    arrange(date)
}

production_clean <- pull_ng_production()

saveRDS(production_clean, "data/raw/ng_production.rds")

# ============================================================
# ### WEEKLY BAKER HUGHES GAS RIG COUNT ###
# ============================================================

pull_rig_count <- function() {
  # Auto-discover current weekly file URL from Baker Hughes page
  page <- request("https://rigcount.bakerhughes.com/na-rig-count") |>
    req_perform() |>
    resp_body_string()
  
  current_url <- str_extract_all(page, "/static-files/[a-f0-9-]+") |>
    unlist() |>
    pluck(2) |>
    paste0("https://rigcount.bakerhughes.com", ... = _)
  
  files <- c(
    "https://rigcount.bakerhughes.com/static-files/e98bcf83-c458-4a88-8f35-4ac4d77628bb", # archive 2013-Aug 2025
    current_url                                                                           # current weekly, auto-discovered
  )
  
  map_dfr(files, function(url) {
    tmp <- tempfile(fileext = ".xlsx")
    
    request(url) |>
      req_perform(path = tmp)
    
    read_excel(tmp, sheet = "NAM Weekly", skip = 9) |>
      filter(
        Country  == "UNITED STATES",
        DrillFor == "Gas"
      ) |>
      group_by(week_ending = as.Date(US_PublishDate)) |>
      summarise(gas_rigs = sum(`Rig Count Value`, na.rm = TRUE), .groups = "drop")
  }) |>
    distinct(week_ending, .keep_all = TRUE) |>
    arrange(week_ending)
}

rig_count <- pull_rig_count()

saveRDS(rig_count, "data/raw/rig_count.rds")

# ============================================================
# ### MONTHLY NATURAL GAS EXPORTS ###
# ============================================================

pull_ng_exports <- function() {
  series <- c(
    total_exports = "EEX",
    lng_exports   = "ENG"
  )
  
  map_dfr(names(series), function(s) {
    eia_data(
      dir    = "natural-gas/move/expc",
      data   = "value",
      facets = list(duoarea = "NUS-Z00", process = series[[s]]),
      freq   = "monthly",
      start  = "2000-01"
    ) |>
      filter(units == "MMCF") |>
      mutate(
        series = s,
        date   = as.Date(paste0(period, "-01")),
        value  = as.numeric(value)
      ) |>
      select(date, series, value)
  }) |>
    pivot_wider(names_from = series, values_from = value) |>
    arrange(date)
}

exports <- pull_ng_exports()

saveRDS(exports_clean, "data/raw/ng_exports.rds")