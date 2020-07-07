# Create models with random elements from data.test
# returns the variance between prediction on data.test
bagFun <- function(data.pointer, cl = NULL, ...){

  # Check for correct arguments
  attr <- strsplit(deparse(data.pointer$formula), " ~ ", fixed = T)
  attr <- sapply(attr, "[[", 1)
  if(sapply(data.pointer$train[attr], class) != "numeric")
    stop("Predicted argument must be numeric for BagV reliability test")

  predictions <- NULL
  if(data.pointer$nThread > 1) predictions <- matrix(parallel::parSapply(cl, 1:50, bagPredPar, data.pointer, ... = ...), nrow = nrow(data.pointer$test), ncol = 50, byrow = F)
  else predictions <- matrix(sapply(1:50, bagPred, data.pointer, ...), nrow = nrow(data.pointer$test), ncol = 50, byrow = F)

  # calculates and return the variance
  return(apply(predictions, 1, stats::var))
}


bagPredPar <- function(x, data.pointer, ...){
  rows <- nrow(data.pointer$train)

  # Create a mask over data.test with random repeatable elements
  # Usees bag to create a model and prediction over data.test
  bag <- data.pointer$train[sample(rows, rows, replace=TRUE),]
  bag.model <- data.pointer$modelF(data.pointer$formula, bag, ...)
  bag.prediction <- data.pointer$predictF(bag.model, data.pointer$test, ...)

  return(bag.prediction)
}

bagPred <- function(x, data.pointer, ...){
  rows <- nrow(data.pointer$train)

  # Create a mask over data.test with random repeatable elements
  # Usees bag to create a model and prediction over data.test
  bag <- data.pointer$train[sample(rows, rows, replace=TRUE),]
  bag.model <- data.pointer$modelF(data.pointer$formula, bag, ...)
  bag.prediction <- data.pointer$predictF(bag.model, data.pointer$test, ...)

  return(bag.prediction)
}
