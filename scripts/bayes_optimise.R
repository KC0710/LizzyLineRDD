do_bayes <- function(n_design = NULL, opt_steps = NULL, of = obj.fun, seed = 42) {
  set.seed(seed)
  des <- generateDesign(n=n_design,
                        par.set = getParamSet(of),
                        fun = lhs::randomLHS)
  control <- makeMBOControl(on.surrogate.error="warn") %>%
    setMBOControlTermination(., iters = opt_steps)
  
  run <- mbo(fun = of,
             design = des,
             learner = makeLearner("regr.km",
                                   predict.type = "se", #mean and standard errors
                                   covtype = "matern3_2",
                                   control = list(trace = FALSE)),
             control = control, 
             show.info = TRUE)
  
  return(run)
}

bayes_optimise <- function(X, obs, test.folds, train.folds, n.trees=1000, n_design=40, opt_steps=30, n.cores=6, seed=42){
  
  obj.fun <- makeSingleObjectiveFunction(
    name = "gbm_cv_bayes",
    fn = function(P){
      set.seed(42)
      gbm.cverr(x = X,
                y = obs,
                distribution = 'gaussian',
                cv.test.folds = test.folds,
                cv.train.folds = train.folds,
                interaction.depth = P["interaction.depth"],
                n.minobsinnode = P["n.minobsinnode"],
                shrinkage = P["shrinkage"],
                bag.fraction = P["bag.fraction"],
                n.trees = n.trees,
                n.cores = n.cores
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

  res <- do_bayes(n_design = n_design, opt_steps = opt_steps, of = obj.fun, seed = seed)
  return(res)
}
