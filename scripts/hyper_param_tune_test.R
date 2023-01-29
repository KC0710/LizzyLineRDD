library(here)
library(openair)
library(worldmet)
library(deweather)
library(mlrMBO)
library(dplyr)

load(here("data", "processed", "aq.list_prebondst_valid.RData"))

dflist <- aq.list_prebondst_valid
df <- dflist[[1]]

#heathrow_meteo <- importNOAA(year=2019:2022)
load(here("data", "raw", "heathrow_meteo.RData"))
meteo_vars <- c("air_temp", 
                "wd", 
                "ws",
                "atmos_pres", 
                "precip", 
                "RH") #exclude Monin-Obukov Length for now.
seasonality_vars <- c("trend", "hour", "doy")
meteo_conf <- heathrow_meteo %>% select(date,
                                        air_temp,
                                        wd,
                                        ws,
                                        atmos_pres,
                                        precip,
                                        RH) #meteorological confounders

#add meteorological confounders to air concentration data
dfm <- merge(x=df, y=meteo_conf, by="date", all.x=TRUE)
dfm <- prepData(dfm, add=c("hour", "trend", "weekday", "jday"))

dfmstar <- dfm %>% select(-site, -code, -date, -nox, -pm10)
dfm_mat <- model.matrix(no2 ~ . -1,
                        data=na.omit(dfmstar))
dfmy <- with(na.omit(dfmstar), no2)
L <- 168

source(here("scripts", "hvblockedfolds.R"))
cv.folds <- hvblockedfolds(na.omit(dfmstar), 
                           fix.fraction = 0.25,
                           block.size = 48,
                           nfolds = 5)
test.folds <- cv.folds$test.id
train.folds <- cv.folds$train.id

source(here("scripts", "gbm.cverr.R"))
obj.fun <- makeSingleObjectiveFunction(
  name = "gbm_cv_bayes",
  fn = function(P){
    set.seed(42)
    gbm.cverr(x = dfm_mat,
              y = dfmy,
              distribution = 'gaussian',
              cv.test.folds = test.folds,
              cv.train.folds = train.folds,
              interaction.depth = P["interaction.depth"],
              n.minobsinnode = P["n.minobsinnode"],
              shrinkage = P["shrinkage"],
              bag.fraction = P["bag.fraction"],
              n.trees = 100,
              n.cores = 3
    )
  },
  par.set = makeParamSet(
    makeNumericParam("shrinkage",                      lower = 0.001, upper = 0.5),
    makeIntegerParam("interaction.depth",              lower = 1,     upper = 10),
    makeIntegerParam("n.minobsinnode",                 lower = 5,     upper = 15),
    makeNumericParam("bag.fraction",                   lower = 0.5,   upper = 1)
  ),
  minimize = TRUE
)


do_bayes <- function(n_design = NULL, opt_steps = NULL, of = obj.fun, seed = 42) {
  set.seed(seed)
  des <- generateDesign(n=n_design,
                        par.set = getParamSet(of),
                        fun = lhs::randomLHS)
  control <- makeMBOControl() %>%
    setMBOControlTermination(., iters = opt_steps)
  
  run <- mbo(fun = of,
             design = des,
             learner = makeLearner("regr.km",
                                   predict.type = "se",
                                   covtype = "matern3_2",
                                   control = list(trace = FALSE)),
             control = control, 
             show.info = TRUE)
  
  return(run)
}

runs <- do_bayes(n_design = 42, of = obj.fun, opt_steps = 10, seed = 42)

save(runs, file=here("results", "hyper_param_tune_test.RData"))
