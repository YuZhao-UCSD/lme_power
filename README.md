# Linear mixed model sample size calculations

This Shiny App performs sample size calculations and power curve plots for the linear mixed model with random intercepts and slopes when used to test for differences in fixed effects slope between groups. Input parameters are random effect variance and residual error variance as estimated by a REML fit to representative pilot data or data from a representative prior clinical trial or cohort study.

To use the App, you can run this in RStudio:

```R
library(shiny)
runGitHub("lme_power", "YuZhao-UCSD")
```

and make sure you have the following packages installed:

```R
install.packages('longpower')
install.packages('ggplot2')
install.packages('shiny')
```

For more information, please see our paper:

Zhao, Y. and S. D. Edland (2021): “Power formulas for mixed effects models withrandom slope and intercept comparing rate of change across groups,”Int J Biostat, doi:10.1515/ijb-2020-0107.