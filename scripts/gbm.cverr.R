#' Tune gbm via cross-validation to find the best set of metaparameters
#'
#' This function serves two purposes. First, it computes the cross-validation
#' error across a grid of \code{gbm} metaparameters input by the user, allowing
#' the model to be easily tuned for a given problem. This process can be 
#' executed in parallel on linux-based machines. Second, it alleviates the 
#' burden of selecting a maximum number of trees for a given set of 
#' metaparameters by allowing the algorithm to run until the best
#' number of trees has been selected according to the cross-validation error, in
#' contrast to the standard approach to \code{gbm}, in which a maximum
#' \code{n.trees} must also be tuned. This allows users to avoid two
#' types of problem associated with an inappropriate selection for
#' \code{n.trees}: (1) failing to specify enough trees and therefore using a 
#' sub-optimal model, and (2) specifying far more trees than are necessary, 
#' therefore making the \code{gbm} run for far more time than necessary.
#' 
#' The main output of \code{gbm.cverr} is a data frame with rows corresponding
#' to sets of metaparameters and columns corresponding to (1) the values 
#' defining each set of metaparameters, (2) the minimum cross-validation error
#' corresponding to each row, and (3) the number of trees that yielded the 
#' minimum cross-validation error in each row. These results are intended to 
#' allow users to make informed decisions about the metaparameters passed to 
#' \code{gbm} when fitting the model that will be interpreted and/or used for
#' prediction in the future. 
#' 
#' Note that the metaparamter values passed to \code{w}, \code{var.monotone}, 
#' \code{interaction.depth}, \code{n.minobsinnode}, \code{shrinkage}, and 
#' \code{bag.fraction} will be fully crossed and evaluated.
#'
#' @param x A \eqn{n x p} matrix or data frame of predictors.
#' @param y A \eqn{n x 1} matrix or vector corresponding to the observed 
#' outcome.
#' @param distribution The distribution to use when fitting each \code{gbm}
#' model. For continuous outcomes, the available distributions are "gaussian" 
#' (squared error) and "laplace" (absolute loss). For dichotomous outcomes, the
#' available distributions are  "bernoulli" (logistic regression for 0-1 
#' outcomes) and "adaboost" (the AdaBoost exponential loss for 0-1 outcomes). 
#' Finally, the "poisson" distribution is available for count outcomes.
#' @param cv.folds Number of cross-validation folds to perform.
#' @param fit.best Logical variable indicating whether or not the best set of 
#' metaparameters (estimated according to cross-validation error) will be 
#' utilized to fit and return a \code{\link{gbm.fit}} object to the complete 
#' data.
#' @param nt.start Initial number of trees used to model y.
#' @param nt.inc Number of trees incrementally added until the cross-validation
#' error is minimized or until \code{max.time} is reached (see below).
#' @param verbose If TRUE, then \code{gbm.cverr} will print status information
#' to the console.
#' @param w a vector of weights of the same length as y. NOTE: to
#' evaluate the effect of different weight vectors, a list can be
#' passed to w in which each element follows the structure described
#' above.
#' @param var.monotone an optional vector, the same length as the number of 
#' predictors, indicating which variables have a monotone increasing (+1), 
#' decreasing (-1), or arbitrary (0) relationship with the outcome. NOTE: to
#' evaluate the effect of different monotonicity constraints, a list can be
#' passed to var.monotone in which each element follows the structure described
#' above.
#' @param interaction.depth The maximum depth of variable interactions: 1 builds
#' an additive model, 2 builds a model with up to two-way interactions, etc. 
#' NOTE: Multiple values can be passed in a vector to evaluate the 
#' cross-validation  error using multiple interaction depths.
#' @param n.minobsinnode The minimum number of observations (not total weights) 
#' in the terminal nodes of the trees. NOTE: Multiple values can be passed in a
#' vector to evaluate the cross-validation error using multiple minimum node 
#' sizes.
#' @param shrinkage A shrinkage parameter applied to each tree in the expansion.
#' Also known as the learning rate or step-size reduction. NOTE: Multiple values
#' can be passed in a vector to evaluate the cross-validation error using 
#' multiple shrinkage penalties.
#' @param bag.fraction The fraction of independent training observations (or 
#' patients) randomly selected to propose the next tree in the expansion, 
#' depending on the obs.id vector multiple training data rows may belong to a 
#' single 'patient'. This introduces randomness into the model fit. NOTE: 
#' Multiple values can be passed in a vector to evaluate the cross-validation 
#' error using multiple bag fractions.
#' @param n.cores Number of cores that will be used to estimate cross-validation
#' folds in parallel. Only available on linux-based machines.
#' @param max.time Maximum number of seconds that the model will continue adding
#' trees for a given set of metaparameters. This optional argument allows users 
#' to find the best possible solution in scenarios characterized by limited 
#' computational resources.
#' @param seed Seed that will guarantee \code{gbm.cverr} to produce identical 
#' results across multiple runs. Utilizing \code{set.seed} prior to calling 
#' \code{gbm.cverr} does NOT ensure equal results if \code{bag.fraction < 1}
#'
#' @return An object with 2-5 elements and a summary function. The elements
#' of \code{gbm.cverr.res} are,
#' \item{gbm.fit}{If \code{fit.best} was \code{TRUE}, then this element is the
#' \code{\link{gbm.fit}} object fit to \code{x} and \code{y} using the best set
#' of metaparameters identified by \code{gbm.cverr}.}
#' \item{w}{List of the optional weight vectors provided by the user. Will not
#' be returned if \code{w} was left \code{NULL} when calling \code{gbm.cverr}.}
#' \item{var.montone}{List of the optional monotonoicity parameters proivided
#' by the user. Will not be returned if \code{var.monotone} was left \code{NULL}
#' when calling \code{gbm.cverr}.}
#' \item{cv.err}{A list with length corresponding to the number of metaparameter
#' combinations that were evaluated by \code{gbm.cverr}. Each element is a
#' vector quantifying the cross-validation error across all trees corresponding
#' to the given set of metaparameters.}
#' \item{res}{A data frame with ten columns and as many rows as there were 
#' unique combinations of metaparameters. This data frame is the basis of the
#' summary function for \code{gbm.cverr.res} objects (see below), but it differs
#' from the summary object in two ways: (1) it is not sorted in terms of the
#' minimum cross-validaiton error, but rather according to the order in which
#' the metaparameters were passed to \code{gbm.cverr}, and (2) it contains two
#' additional columns. The column \code{best.meta} is a dummy variable that
#' simply indexes the best set of metaparameters, and \code{timer.end} is a
#' dummy variable indicating whether or not the optimal number of trees was
#' found for a given set of metaparameters (FALSE) or whether the user-specified
#' maximum search time was reached prior to minimizing the cross-validation
#' error (TRUE). If the timer ran out, then the estimated optimal number of 
#' trees is likely underestimated. If this occurred for metaparameter set
#' \code{k}, then in order to evaluate the extent to which the error
#' was still decresaing when the timer ended, we recommend investigating a
#' plot of \code{gbm.cverr.res$cv.err[k]}. The rest of the elements of 
#' \code{res} are discussed below.}
#' 
#' Calling \code{summary(gbm.cverr.res)} produces a data frame with rows 
#' corresponding to sets of metaparameters and columns that denote for each row,
#' \item{min.cv.error}{Minimum cross-validation error resulting from the given
#' set of metaparameters.}
#' \item{w.index}{The index of the (optional) list of weight vectors
#' corresponding to the given set of metaparameters. This will be omitted if
#' a list of weights was not provided to \code{gbm.cverr} through the input
#' parameter \code{w}.}
#' \item{var.monotone.index}{The index of the (optional) list of monotonicity
#' vectors corresponding to the given set of metaparameters. This will be
#' omitted if a list of weights was not provided to \code{gbm.cverr} through
#' the input parameter \code{var.monotone}.}
#' \item{interaction.depth}{The interaction depth corresponding to the
#' given set of metaparameters.}
#' \item{n.minobsinnode}{Minimum number of observations in the terminal
#' nodes of the trees for the given set of metaparameters.}
#' \item{shrinkage}{The shrinkage parameter corresponding to the
#' given set of metaparameters.}
#' \item{bag.fraction}{The fraction of independent training observations 
#' randomly selected to propose the next tree corresponding to
#' the given set of metaparameters.}
#' \item{n.trees}{The optimum number of trees to utilize given the set of
#' metaprameters denoted in the row. Note that entries in this column will be
#' marked with '>=' if the boosting procedure was terminated due to time running
#' out for this set of metaparameters, determined by the user-specified 
#' \code{max.time} passed to \code{gbm.cverr}}
#'
#' In the summary object and output, sets of metaparameters (rows) are ordered 
#' from best (top row) to worst (last row) in terms of the resulting 
#' cross-validation error.
#'
#' @author Daniel B. McArtor (dmcartor@nd.edu)
#'
#' @examples
#'data(wellbeing)
#'y <- wellbeing[,25]
#'x <- wellbeing[,1:20]
#'
#'mm <- gbm.cverr(x = x, y = y, 
#'                distribution = 'gaussian', 
#'                cv.folds = 2, 
#'                
#'                nt.start = 100, 
#'                nt.inc = 100, 
#'                max.time = 1, 
#'                
#'                seed = 12345,
#'                interaction.depth = c(1, 5), 
#'                shrinkage = 0.01,
#'                n.minobsinnode = c(5, 50), 
#'                verbose = TRUE)
#'
#'summary(mm)
#'
#'# Investigate gbm results based on the best set of metaparameters
#'mm$gbm.fit
#'summary(mm$gbm.fit)
#'
#' @export
gbm.cverr <- function(
  # Necessary input
  x, y, distribution = "gaussian", cv.train.folds,
  cv.test.folds,
  
  # GBM metaparameters
  interaction.depth = 1, # number or vector
  n.minobsinnode = 10, # number or vector
  shrinkage = 0.001, # number or vector
  bag.fraction = 0.5, # number or vector
  
  w = NULL, # vector or list of vectors
  var.monotone = NULL, # vector or list of vectors
  #feature.fraction = 0.8, #number of vector

  # Find best number of trees
  n.trees = 1000,
  
  # Time management
  n.cores = 1, max.time = NULL,
  
  # Verbosity
  verbose = TRUE,
  
  # Reproducible results
  seed = NULL){
  
  ##############################################################################
  ## Set up input and source required packages
  ##############################################################################
  
  # ----------------------------------------------------------------------------
  # Set up grid of metaparameters to evaluate
  # ----------------------------------------------------------------------------
  
  # Weights
  if(!is.list(w)){
    w <- list(w)
  }
  w.inds <- 1:length(w)
  
  # Monotone variance indicators
  return.var <- T
  if(is.null(var.monotone)){return.var <- F}
  if(!is.list(var.monotone)){
    var.monotone <- list(var.monotone)
  }
  var.inds <- 1:length(var.monotone)
  
  # ----------------------------------------------------------------------------
  # Misc data management
  # ----------------------------------------------------------------------------
  
  x <- as.data.frame(x)
  n <- length(y)
  if(n != nrow(x)){stop('Differing number of observations in x and y')}
  
  # Get seeds for each set of metaparameters
  if(is.null(seed)){seed <- round(stats::runif(1, 0, 1) * .Machine$integer.max)}
  set.seed(seed)
  seeds <- round(stats::runif(1, 0, 1) * .Machine$integer.max)
  
  # Needs weights to evaluate loss function, so use equality if w = NULL
  return.w <- T
  if(is.null(w)){
    return.w <- F
    w[[1]] <- rep(1, n)
  }
  
  # Check to make sure a valid distribution has been specified
  if(!(distribution %in% 
       c('gaussian', 'adaboost', 'bernoulli', 'laplace', 'poisson'))){
    stop(paste0('Specified distribution unavailable. Please select from:\n',
                'gaussian, adaboost, bernoulli, laplace, poisson'))
  }
  
  if(is.null(max.time)){max.time <- Inf}
  
  ##############################################################################
  ## Program loss functions for each GBM distribution
  ##############################################################################
  loss <- function(fx, yobs, wt, distrb){
    
    err <- NULL
    
    if(distrb == 'gaussian'){
      err <- sum(wt * (yobs - fx)^2) / sum(wt)
    }
    
    if(distrb == 'adaboost'){
      err <- sum(wt * exp(-(2 * yobs - 1) * fx)) / sum(wt)
    }
    
    if(distrb == 'bernoulli'){
      err <- -2 / sum(wt) * sum(wt * (
        yobs * fx - log(1 + exp(fx))
      ))
    }
    
    if(distrb == 'laplace'){
      err <- sum(wt * abs(yobs - fx)) / sum(wt)
    }
    
    if(distrb == 'poisson'){
      err <- -2 * sum(wt * (yobs * fx - exp(fx))) / sum(wt)
    }
    
    return(err)
  }
  
  ##############################################################################
  ## Cross-validate for a given combination of metaparameters
  ##############################################################################
  
  res <- function(nfolds){
    if(verbose){
      cat(paste(rep('=', 80), collapse = ''), fill = T)
    }

    # Begin cross-validation
    cv.err <- 
      #parallel::mclapply(1:nfolds, mc.cores = n.cores, FUN = function(fld){
      lapply(1:nfolds, FUN = function(fld){
        # Training and test indices for this fold
        print("testfld")
        test <- cv.test.folds[[fld]]
        train <- cv.train.folds[[fld]]
        # Fit the model to the training data using the initial number of folds
        set.seed(fld)
        mm.cv <- gbm::gbm.fit(x[train, ],
                              y[train],
                              offset = NULL,
                              misc = NULL,
                              distribution = "gaussian",
                              w = NULL, 
                              var.monotone = NULL,
                              n.trees = n.trees, 
                              interaction.depth = interaction.depth,
                              n.minobsinnode = n.minobsinnode, 
                              shrinkage = shrinkage, 
                              bag.fraction = bag.fraction,
                              nTrain = length(train),
                              train.fraction = NULL,
                              keep.data = TRUE, 
                              verbose = TRUE,
                              var.names = NULL, 
                              response.name = "y", 
                              group = NULL)
        
        # Get test error
        ytest.cv <- y[test]
        wtest.cv <- rep(1, length(ytest.cv)) #w[test]
        fx.cv <-
          predict(mm.cv, newdata = x[test,], n.trees = n.trees)
        err <- loss(fx.cv, yobs = ytest.cv, wt = wtest.cv, distrb = distribution)
        
        if (is.na(err)) stop("Stopping. Error is NA.")
        return(list(err = err, mm.cv = mm.cv))
    })
    # Compute CV error across folds
    print(cv.err)
    err <- Reduce('+', lapply(cv.err, FUN = function(err){err[[1]]})) / 
      length(cv.err)
    
    return(err)
  }
  
  nfolds <- length(cv.train.folds)
  
  return(res(nfolds))
}