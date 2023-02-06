library(here)
library(mlrMBO)
library(deweather)
library(dplyr)
library(tidyr)
library(lubridate)

source(here("scripts", "bayes_optimise.R"))
source(here("scripts", "hvblockedfolds.R"))
source(here("scripts", "gbm.cverr.R"))
source(here("scripts", "hyperparameter_optimisation.R"))

load(here("data", "processed", "all_validated_sites.RData"))
load(here("data", "processed", "meteo_full.RData"))

seasonality_vars <- c("weekday", "jday", "trend")
meteo_seasonality <- prepData(meteo_full, add=seasonality_vars) %>%
  select(-rain_12, -rain_06, -rain_06_modify,
         -wd_cos, -wd_sin, -wd_math,
         -dew_point, -ceil_hgt, -visibility)

m <- lapply(all_validated, FUN=function(x) min(x$DateTime)) #these throw up an error so
m_ind <- which(year(do.call("c", m)) == 2022)
all_validated_nm <- all_validated[-m_ind]

optim_runs_by_site <- lapply(all_validated,
                             FUN=run_hyperparameter_optimisation,
                             meteo_seasonality)

save(optim_runs_by_site, file=here("results", "optimal_hyperparams_by_site.RData"))
