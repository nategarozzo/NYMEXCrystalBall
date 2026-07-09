# ============================================================
# run_forecast.R
# Master forecast script for NYMEXCrystalBall
# Set N here then sources all model scripts
# ============================================================

library(tidyverse)
library(lubridate)
library(fredr)
library(fable)
library(tsibble)
library(tidyquant)

source("R/data_pulls/setup.R")

# ============================================================
# ### SET FORECAST HORIZON ###
# ============================================================

# N = number of trading days to forecast ahead
# 20 = 4 weeks (prompt month)
# 45 = 9 weeks
# 60 = 12 weeks (maximum — limited by C4 futures data)

N <- 45

# ============================================================
# ### RUN ALL MODELS ###
# ============================================================

cat("Running NYMEXCrystalBall forecast —", N, "days ahead\n")
cat("================================================\n\n")

cat("### Model A — Fair Value + Mean Reversion ###\n")
source("R/models/model_a_fairvalue.R")

cat("\n### Model B — ARMA Daily ###\n")
source("R/models/model_b_arma.R")

cat("\n### Model C — Futures Curve ###\n")
source("R/models/model_c_futures.R")

cat("\n### Combining Forecasts ###\n")
source("R/models/combine_forecasts.R")

cat("\n### Interpolating Daily Path ###\n")
source("R/models/interpolate_path.R")

cat("\n================================================\n")
cat("Forecast complete. Results saved to data/forecasts/\n")