library(here)
library(smoof)
library(mlrMBO)
library(deweather)
library(dplyr)
library(tidyr)
library(lubridate)

source(here("scripts", "bayes_optimise.R"))
source(here("scripts", "hvblockedfolds.R"))
source(here("scripts", "gbm.cverr.R"))
load(here("data", "processed", "all_validated_sites.RData"))
load(here("data", "processed", "meteo_full.RData"))

seasonality_vars <- c("weekday", "jday", "trend")
meteo_seasonality <- prepData(meteo_full, add=seasonality_vars) %>%
  select(-rain_12, -rain_06, -rain_06_modify,
         -wd_cos, -wd_sin, -wd_math,
         -dew_point, -ceil_hgt, -visibility)