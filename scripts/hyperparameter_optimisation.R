gbm_inputs <- function(aq.dat, meteo.data){
  aq.dat_m <- aq.dat %>% rename(date = DateTime) %>%
    merge(meteo.data, by=c("date", "Year", "Month", "Day", "Hour")) %>%
    select(-Year, -Month, -Day, -Pollutant,
           -date, -Classification, -Site_combine,
           -Latitude, -Longitude, -Site) %>% tidyr::drop_na()
  
  yobs <- aq.dat_m$Value
  X <- model.matrix(Value ~ . -1, aq.dat_m)
  
  cv.folds <- hvblockedfolds(X, block.size=48)
  cv.train.folds <- cv.folds$train.id
  cv.test.folds <- cv.folds$test.id
  return(list(yobs=yobs, modmat=X, test.id=cv.test.folds, train.id=cv.train.folds))
}

run_hyperparameter_optimisation <- function(aq.dat, meteo.data){
  
  aq_by_pollutant <- split(aq.dat,
                           aq.dat$Pollutant)
  gbm_inputs <- lapply(aq_by_pollutant,
                       FUN=gbm_inputs,
                       meteo.data)
  
  lapply(gbm_inputs, FUN=function(lst) {bayes_optimise(lst$modmat,
                                                       lst$yobs,
                                                       lst$test.id,
                                                       lst$train.id)})
}


