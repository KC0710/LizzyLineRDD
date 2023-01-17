hv.block_folds <- function(obs, L, nfolds=10) {
  #https://github.com/vcerqueira/blog/tree/main/src
  nobs <- NROW(obs)
  indices <- 1:nobs
  f <- cut(seq_len(NROW(obs)), breaks = nfolds, labels = FALSE) 
  train.id <- list()
  test.id <- list()
  seq. <- seq_len(nfolds)
  lseq <- seq.[length(seq.)] 
  for (i in seq.) {
    ts.id <- which(f == i)
    # upper cut
    kcuts <- integer(0L)
    if (i + 1L <= lseq) {
      upper.fold <- which(f == i + 1L)
      upper.cut <- upper.fold[1:L]
      if (any(is.na(upper.cut))) {
        upper.cut <- upper.cut[!is.na(upper.cut)]
      }
      
      kcuts <- c(kcuts, upper.cut)
    }
    # lower cut
    if (i - 1L >= 1L) {
      lower.fold <- which(f == i - 1L)
      len.lf <- length(lower.fold)
      idx <- (len.lf - L + 1L):len.lf
      idx <- idx[idx > 0]
      lower.cut <- lower.fold[idx]
      kcuts <- c(kcuts, lower.cut)
    }
    train.id[[i]] <- indices[-c(ts.id, kcuts)]
    test.id[[i]]  <- indices[ts.id]
  }
  #rsplit <- map2(train.id,
                 #test.id,
                 #function(x,y) list(analysis=x, assessment=y))
  
  #hvfolds <- lapply(rsplit, make_splits, data = obs)
  #hvfolds <- manual_rset(hvfolds, hvfold_names)
  #hvfold_names <- paste(rep("Fold", times=nfolds), as.character(1:nfolds))

  return(list(train_id=train.id, test_id=test.id))
}
