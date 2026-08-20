# NYMEXCrystalBall

*A forecasting tool for the (rolling) prompt-month Henry Hub natural gas futures contract. Generates price path forecasts between 5 and 45 days in advance with 60% and 90% confidence intervals.*

## Overview

In order to produce a weekly forecast, three different models are combined:

- Model A - Fair value regression using fundamental data (storage, rig counts, capacity utilization, WTI/HH ratio) with mean reversion

- Model B - NYMEX futures curve interpolation

- Model C - Empirical conficence bands calibrated from historical price moves

Model weights are dynamically shifted depending on the horizon, with the futures curve dominating at short horizons and fundamentals taking over at longer ones.

Performance is the strongest in summer and shoulder months at horizons of 30-45 days, while winter performance is limited by weather uncertainty.

**For a full description of how the model works, including performance metrics, use this link to access the writeup.**
