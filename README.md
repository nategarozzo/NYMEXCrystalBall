# NYMEXCrystalBall

## **Data Dictionary**

### Index

|  |  |
|------------------------------------|------------------------------------|
| `week_ending` | Friday date of each weekly observation |
| `year` | Calendar year extracted from `week_ending` |
| `month` | Calendar month (1–12) extracted from `week_ending` |
| `week_of_year` | Week number within the calendar year |
| `in_model_window` | Binary flag equal to 1 for weeks from January 2017 onward, 0 otherwise |

### Price

|  |  |
|------------------------------------|------------------------------------|
| `spot_price` | Henry Hub natural gas spot price in dollars per MMBtu |
| `log_real_price` | Natural log of the Henry Hub spot price deflated by the U.S. CPI |
| `log_price_lag1` | `log_real_price` from the prior week |
| `log_price_lag4` | `log_real_price` from four weeks prior |
| `rolling_20d_vol` | Standard deviation of daily log returns over the trailing 20 trading days |
| `c1_c4_spread` | NYMEX Contract 4 settlement price minus Contract 1 settlement price in dollars per MMBtu |
| `spot_c1_basis` | Henry Hub spot price minus NYMEX Contract 1 settlement price in dollars per MMBtu |

### Storage

|  |  |
|------------------------------------|------------------------------------|
| `total` | Total U.S. working gas in underground storage for the lower 48 states in Bcf |
| `log_storage` | Natural log of total U.S. working gas in storage |
| `storage_yoy` | Difference between current storage and storage for the same week one year prior in Bcf |
| `storage_5yr_avg` | Average storage level for the same week of year across the prior five calendar years in Bcf |
| `storage_5yr_surplus` | Difference between current storage and `storage_5yr_avg` in Bcf |

### Weather

|  |  |
|------------------------------------|------------------------------------|
| `hdd_pop` | Weekly sum of U.S. population-weighted heating degree days |
| `cdd_pop` | Weekly sum of U.S. population-weighted cooling degree days |
| `hdd_gas` | Weekly sum of U.S. utility gas customer-weighted heating degree days |
| `hdd_pop_dev` | `hdd_pop` minus the 10-year historical average for the same week of year |
| `cdd_pop_dev` | `cdd_pop` minus the 10-year historical average for the same week of year |
| `hdd_gas_dev` | `hdd_gas` minus the 10-year historical average for the same week of year |

### Supply

|  |  |
|----|----|
| `dry_production` | U.S. dry natural gas production in MMcf per month |
| `log_dry_production` | Natural log of U.S. dry natural gas production |
| `total_exports` | Total U.S. natural gas exports by pipeline and LNG vessel in MMcf per month |
| `log_total_exports` | Natural log of total U.S. natural gas exports |
| `lng_exports` | U.S. LNG exports by vessel in MMcf per month |
| `log_lng_exports` | Natural log of U.S. LNG exports |
| `gas_rigs` | Number of U.S. gas-directed rotary rigs in operation as reported by Baker Hughes |
| `log_gas_rigs` | Natural log of the gas-directed rig count |
| `rig_count_13wk_ma` | 13-week trailing simple moving average of `gas_rigs` |

### Macro

|  |  |
|----|----|
| `tcu` | U.S. total industry capacity utilization as a percentage of total capacity |
| `post_invasion` | Binary variable equal to 1 for all weeks from February 28, 2022 onward and 0 before |
| `season_winter` | Binary variable equal to 1 if the week falls in December, January, or February |
| `season_summer` | Binary variable equal to 1 if the week falls in June, July, or August |
| `season_fall` | Binary variable equal to 1 if the week falls in September, October, or November |
