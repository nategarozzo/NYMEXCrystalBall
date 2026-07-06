library(tidyverse)
library(eia)
library(fredr)

source("~/NYMEXCrystalBall/R/data_pulls/setup.R")

base_url <- "https://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/daily_data"

region_map <- c(
  "1" = "new_england",
  "2" = "middle_atlantic",
  "3" = "en_central",
  "4" = "wn_central",
  "5" = "south_atlantic",
  "6" = "es_central",
  "7" = "ws_central",
  "8" = "mountain",
  "9" = "pacific",
  "CONUS" = "us_total"
)

pull_noaa_degree_days <- function(years, type = c("Heating", "Cooling"),
                                  weight = "Population") {
  type <- match.arg(type)
  filename <- paste0(weight, ".", type, ".txt")
  
  map_dfr(years, function(yr) {
    url <- paste(base_url, yr, filename, sep = "/")
    
    resp <- request(url) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()
    
    if (resp_status(resp) != 200) {
      warning("Failed to fetch: ", url)
      return(NULL)
    }
    
    raw <- resp |>
      resp_body_string() |>
      read_delim(delim = "|", skip = 3, col_types = cols(.default = "c"),
                 show_col_types = FALSE)
    
    raw |>
      rename(region_id = 1) |>
      filter(region_id %in% names(region_map)) |>
      pivot_longer(-region_id, names_to = "date_chr", values_to = "value") |>
      mutate(
        date    = as.Date(date_chr, format = "%Y%m%d"),
        value   = as.numeric(value),
        region  = region_map[region_id],
        type    = type,
        weight  = weight
      ) |>
      select(date, region, type, weight, value)
  })
}

years <- 2000:2026

hdd_daily <- pull_noaa_degree_days(years, type = "Heating", weight = "Population")
cdd_daily <- pull_noaa_degree_days(years, type = "Cooling", weight = "Population")

# Also pull gas-customer-weighted HDD — more relevant for nat gas demand
hdd_gas_daily <- pull_noaa_degree_days(years, type = "Heating", weight = "UtilityGas")

degree_days_daily <- bind_rows(hdd_daily, cdd_daily, hdd_gas_daily)

# Aggregate to weekly (week ending Friday, matching EIA storage report convention)
degree_days_weekly <- degree_days_daily |>
  mutate(week_ending = ceiling_date(date, unit = "week", week_start = 5) - 1) |>
  group_by(week_ending, region, type, weight) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

saveRDS(degree_days_daily,  "data/raw/degree_days_daily.rds")
saveRDS(degree_days_weekly, "data/raw/degree_days_weekly.rds")