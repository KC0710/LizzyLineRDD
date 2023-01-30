hvblockedfolds <- function(obs, block.size, fix.fraction=0.25, nfolds=5) {
  nobs <- NROW(obs)
  split.point <- ceiling(fix.fraction*nobs)
  test.block <- obs[split.point:nobs, ]
  f <- cut(seq_len(NROW(test.block)), breaks = nfolds, labels = FALSE)
  
  indices <- 1:nobs
  train.id <- list()
  test.id <- list()
  i <- 1
  while (i <= nfolds) {
    ts.id <- split.point + indices[which(f == i)] - 1
    train.id[[i]] <- 1:(ts.id[1] - block.size - 1)
    test.id[[i]] <- ts.id
    
    i <- i + 1
  }
  #rsplit <- map2(train.id,
                 #test.id,
                 #function(x,y) list(analysis=x, assessment=y))
  
  #hvfolds <- lapply(rsplit, make_splits, data = obs)
  #hvfolds <- manual_rset(hvfolds, hvfold_names)
  #hvfold_names <- paste(rep("Fold", times=nfolds), as.character(1:nfolds))

  return(list(train.id=train.id, test.id=test.id))
}
