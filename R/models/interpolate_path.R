# ============================================================
# interpolate_path.R
# Produces smooth daily price path and asymmetric confidence bands
# for Shiny app visualization
# Inputs: data/forecasts/forecast_combined.rds
#         data/forecasts/model_a_objects.rds
#         data/raw/ng_futures_daily.rds
# Output: data/forecasts/daily_path.rds
# ============================================================

library(tidyverse)
library(lubridate)

combined         <- readRDS("data/forecasts/forecast_combined.rds")
model_a_objects  <- readRDS("data/forecasts/model_a_objects.rds")
ng_futures_daily <- readRDS("data/raw/ng_futures_daily.rds")

# ============================================================
# ### STEP 1: SET FORECAST ORIGIN ###
# ============================================================

last_price_date <- ng_futures_daily |>
  filter(!is.na(c1_price)) |>
  pull(date) |>
  max()

current_price <- ng_futures_daily |>
  filter(date == last_price_date) |>
  pull(c1_price)

cat("Forecast origin:", format(last_price_date), "\n")
cat("Current C1 price: $", round(current_price, 2), "\n")

# ============================================================
# ### STEP 2: COMPUTE ASYMMETRIC BAND SCALING ###
# ============================================================

# Use Model A's current gap to determine directional conviction
# Large gap = strong conviction = more asymmetry in confidence band
# Bullish signal: widen upside band, shrink downside band
# Bearish signal: widen downside band, shrink upside band

current_gap_a  <- model_a_objects$current_gap
conviction     <- tanh(abs(current_gap_a) * 3)
direction      <- sign(-current_gap_a)  # +1 if bullish, -1 if bearish

asymmetry_upper <- 1 + direction * conviction * 0.4
asymmetry_lower <- 1 - direction * conviction * 0.4

cat("Model A signal:", ifelse(direction > 0, "BULLISH", "BEARISH"), "\n")
cat("Conviction:", round(conviction, 3), "\n")
cat("Upper band scale:", round(asymmetry_upper, 3), "\n")
cat("Lower band scale:", round(asymmetry_lower, 3), "\n")

# ============================================================
# ### STEP 3: COMPUTE EMPIRICAL BANDS ###
# ============================================================

base_data <- ng_futures_daily |>
  filter(
    date >= as.Date("2017-01-01"),
    !is.na(c1_price),
    !(date >= as.Date("2022-01-01") & date <= as.Date("2023-06-30"))
  ) |>
  arrange(date)

get_quantile <- function(h, prob) {
  if (h == 0) return(0)
  base_data |>
    mutate(move = lead(c1_price, h) - c1_price) |>
    pull(move) |>
    quantile(prob, na.rm = TRUE)
}

trading_day_horizons <- unique(c(0, 1, 2, 3, seq(5, N, by = 5)))

empirical_bands <- map_dfr(trading_day_horizons, function(h) {
  tibble(
    horizon_days = h,
    p10          = get_quantile(h, 0.10),
    p20          = get_quantile(h, 0.20),
    p80          = get_quantile(h, 0.80),
    p90          = get_quantile(h, 0.90)
  )
})

# ============================================================
# ### STEP 4: BUILD ANCHOR POINTS ###
# ============================================================

anchor_dates    <- c(last_price_date, combined$date)
anchor_forecast <- c(current_price,   combined$forecast)

# ============================================================
# ### STEP 5: BUILD DAILY PATH WITH ASYMMETRIC BANDS ###
# ============================================================

daily_dates <- seq.Date(last_price_date, max(combined$date), by = "day")

daily_path <- tibble(date = daily_dates) |>
  mutate(
    is_trading_day = !wday(date, label = TRUE) %in% c("Sat", "Sun"),
    trading_day    = cumsum(is_trading_day) - 1,
    
    # Interpolate forecast center line
    forecast = approx(as.numeric(anchor_dates), anchor_forecast,
                      xout = as.numeric(date))$y,
    
    # Raw empirical quantiles at each trading day
    p10_raw = approx(empirical_bands$horizon_days, empirical_bands$p10,
                     xout = trading_day, rule = 2)$y,
    p90_raw = approx(empirical_bands$horizon_days, empirical_bands$p90,
                     xout = trading_day, rule = 2)$y,
    p20_raw = approx(empirical_bands$horizon_days, empirical_bands$p20,
                     xout = trading_day, rule = 2)$y,
    p80_raw = approx(empirical_bands$horizon_days, empirical_bands$p80,
                     xout = trading_day, rule = 2)$y,
    
    # Apply asymmetric scaling based on Model A conviction
    p10 = p10_raw * asymmetry_lower,
    p90 = p90_raw * asymmetry_upper,
    p20 = p20_raw * asymmetry_lower,
    p80 = p80_raw * asymmetry_upper,
    
    # Final band edges — zero width at forecast origin
    lower_90 = if_else(date == last_price_date, current_price, forecast + p10),
    upper_90 = if_else(date == last_price_date, current_price, forecast + p90),
    lower_60 = if_else(date == last_price_date, current_price, forecast + p20),
    upper_60 = if_else(date == last_price_date, current_price, forecast + p80)
  ) |>
  select(date, is_trading_day, trading_day, forecast,
         lower_60, upper_60, lower_90, upper_90)

# ============================================================
# ### STEP 6: HISTORICAL PRICE FOR CHART ###
# ============================================================

historical <- ng_futures_daily |>
  filter(
    date >= last_price_date - 90,
    date <= last_price_date,
    !is.na(c1_price)
  ) |>
  select(date, c1_price)

# ============================================================
# ### STEP 7: SAVE OUTPUT ###
# ============================================================

output <- list(
  daily_path     = daily_path,
  historical     = historical,
  combined       = combined,
  origin_date    = last_price_date,
  origin_price   = current_price,
  conviction     = conviction,
  direction      = direction,
  N              = N,
  n_weeks        = nrow(combined),
  as_of          = Sys.Date()
)

saveRDS(output, "data/forecasts/daily_path.rds")

cat("\nDaily path saved:", nrow(daily_path), "days\n")
cat("Historical prices:", nrow(historical), "days\n")
cat("Weekly waypoints:", nrow(combined), "\n")