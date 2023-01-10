library(here)
library(openair)
library(worldmet)
library(deweather)
library(mlrMBO)
load(here("data", "processed", "aq.list_prebondst_valid.RData"))

dflist <- aq.list_prebondst_valid
df <- dflist[[1]]

heathrow_meteo <- importNOAA(year=2019:2022)
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

dfmstar <- dfm %>% select(-site, -code)
dfm_mat <- model.matrix(no2 ~ . -1 -date -nox -pm10,
                        data=dfmstar)
dfm_xgb <- xgb.DMatrix(data = dfm_mat, 
                       label = na.omit(dfmstar)$no2)
L <- 168

#obj.coefdeterm <- function(preds, dtrain){
  labels <- getinfo(dtrain, "label")
  SStot <- sum((labels - mean(labels))^2)
  grad <- rep(2*sum(labels-preds), times=length(labels))/SStot
  hess <- -rep(2, times=length(labels))/SStot
  return(list(grad = grad, hess = hess))
}

#feval.RMSE <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  u = (preds-labels)^2
  err <- sqrt(sum(u) / length(u))
  return(list(metric = "RMSE", value = err))
}

obj.fun <- makeSingleObjectiveFunction(
  name = "xgb_cv_bayes",
  fn = function(x){
    set.seed(42)
    cv <- xgb.cv(params = list(
      booster          = "gbtree",
      learning_rate    = x["learning_rate"],
      max_depth        = x["max_depth"],
      subsample        = x["subsample"],
      colsample_bytree = x["colsample_bytree"],
      objective = "reg:squarederror",
      eval_metric = "rmse"),
      data = dfm_xgb, ## must set in global.Env()
      nround = 7000, ## Set this large and use early stopping
      nthread = 26, ## Adjust based on your machine
      folds = hv.block_folds(dfm_xgb, L, 5)$test_id,
      train_folds = hv.block_folds(dfm_xgb, L, 5)$train_id,
      prediction = FALSE,
      showsd = TRUE,
      early_stopping_rounds = 25, ## If evaluation metric does not improve on out-of-fold sample for 25 rounds, stop
      verbose = 1,
      print_every_n = 500)
    
    cv$evaluation_log %>% pull(4) %>% min  ## column 4 is the eval metric here, 
  },
  par.set = makeParamSet(
    makeNumericParam("learning_rate",          lower = 0.005, upper = 0.01),
    makeIntegerParam("max_depth",              lower = 2,      upper = 10),
    makeNumericParam("subsample",              lower = 0.20,  upper = .8),
    makeNumericParam("colsample_bytree",       lower = 0.20,  upper = .8)
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
  ## GP surrogate model with more or lesss smoothness, or use an entirely different method
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
