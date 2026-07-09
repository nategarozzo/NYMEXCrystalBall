# ============================================================
# model_a_bvar.R
# Model A — Bayesian Vector Autoregression (BVAR)
# Input: data/features/master_panel.rds
# Output: data/forecasts/forecast_a.rds
# ============================================================

library(tidyverse)
library(lubridate)
library(BVAR)
library(fredr)

source("R/data_pulls/setup.R")

# ============================================================
# ### STEP 1: LOAD AND PREPARE DATA ###
# ============================================================

master_panel <- readRDS("data/features/master_panel.rds")

model_data <- master_panel |>
  filter(in_model_window == 1) |>
  select(
    week_ending,
    log_real_price,
    log_dry_production,
    log_gas_rigs,
    log_storage,
    log_lng_exports,
    hdd_pop_dev,
    tcu
  ) |>
  drop_na()

cat("Training observations:", nrow(model_data), "\n")
cat("Training window:", format(min(model_data$week_ending)), "to",
    format(max(model_data$week_ending)), "\n")

# Prepare matrix for BVAR
bvar_data <- model_data |>
  select(-week_ending) |>
  as.matrix()

# ============================================================
# ### STEP 2: SET MINNESOTA PRIOR ###
# ============================================================

# Minnesota prior with data-driven lambda selection
# Lambda controls prior tightness — optimised via marginal likelihood
# Low lambda = tight prior (shrink toward random walk)
# High lambda = loose prior (closer to OLS)

mn_prior <- bv_mn(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
  alpha  = bv_alpha(mode = 2),
  psi    = bv_psi()
)

# ============================================================
# ### STEP 3: FIT BVAR(1) ###
# ============================================================

# VAR(1) — one lag of each variable
# 10,000 MCMC draws with 5,000 burn-in
# Consistent with Baumeister et al. (2025) specification

set.seed(42)

fit_bvar <- bvar(
  data    = bvar_data,
  lags    = 1,
  n_draw  = 10000,
  n_burn  = 5000,
  priors  = bv_priors(mn = mn_prior),
  verbose = TRUE
)

summary(fit_bvar)

# ============================================================
# ### STEP 4: GENERATE 4-WEEK FORECAST ###
# ============================================================

forecast_bvar <- predict(
  fit_bvar,
  horizon    = 4,
  conf_bands = c(0.5)
)

# Extract draws for log_real_price (variable 1)
# Dimensions: [draws, horizons, variables]
forecast_draws <- forecast_bvar$fcast[, , 1]

# ============================================================
# ### STEP 5: CONVERT BACK TO DOLLAR PRICES ###
# ============================================================

# log_real_price = log(c1_price / cpi * 100)
# c1_price = exp(log_real_price) * cpi / 100
# Note: log_real_price is now based on NYMEX C1 futures (NG=F)

current_cpi <- fredr_series_observations(
  series_id         = "CPIAUCSL",
  observation_start = as.Date("2026-01-01"),
  frequency         = "m"
) |>
  filter(!is.na(value)) |>
  arrange(desc(date)) |>
  slice(1) |>
  pull(value)

cat("CPI used for back-transformation:", current_cpi, "\n")

# ============================================================
# ### STEP 6: BUILD FORECAST OBJECT — POINT FORECAST ONLY ###
# ============================================================

# Confidence intervals handled empirically in combine_forecasts.R
# Only median point forecast extracted here

forecast_a <- tibble(
  horizon  = 1:4,
  median   = apply(forecast_draws, 2, median)
) |>
  mutate(
    forecast = exp(median) * current_cpi / 100,
    date     = max(model_data$week_ending) + (horizon * 7),
    model    = "A"
  ) |>
  select(model, date, horizon, forecast)

# ============================================================
# ### STEP 7: SAVE OUTPUT ###
# ============================================================

dir.create("data/forecasts", recursive = TRUE, showWarnings = FALSE)

saveRDS(forecast_a, "data/forecasts/forecast_a.rds")

print(forecast_a)