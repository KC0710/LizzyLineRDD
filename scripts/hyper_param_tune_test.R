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

source(here("scripts", "hv.block_folds.R"))
cv.folds <- hv.block_folds(na.omit(dfmstar), L)
test.folds <- cv.folds$test_id
train.folds <- cv.folds$train_id

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
              n.trees = 100
    )
  },
  par.set = makeParamSet(
    makeNumericParam("shrinkage",                      lower = 0.005, upper = 0.01),
    makeIntegerParam("interaction.depth",              lower = 2,     upper = 10),
    makeIntegerParam("n.minobsinnode",                 lower = 5,     upper = 15),
    makeNumericParam("bag.fraction",                   lower = 0.5,   upper = 0.9)
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
  
  ## kriging with a matern(3,2) covariance function is the default surrogate model for numerical domains
  ## but if you wanted to override this you could modify the makeLearner() call below to define your own
  ## GP surrogate model with more or less smoothness, or use an entirely different method
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


des <- generateDesign(n=42,
                      par.set = getParamSet(obj.fun),
                      fun = lhs::randomLHS)

runs <- do_bayes(n_design = 15, of = obj.fun, opt_steps = 10, seed = 42)

save(runs, file=here("results", "hyper_param_tune_test.RData"))
