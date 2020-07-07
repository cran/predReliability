saFun <- function(data.pointer, cl = NULL, ...){
#sa <- function(data.test, data.train, formula, model.function, predict.function){
  # Check for correct arguments
  attr <- strsplit(deparse(data.pointer$formula), " ~ ", fixed = T)
  attr <- sapply(attr, "[[", 1)
  if(sapply(data.pointer$train[attr], class) != "numeric")
    stop("Predicted argument must be numeric for cnk reliability test")

  # Create model used in phaseOne
  model <- data.pointer$modelF(data.pointer$formula, data.pointer$train, ...)

  # Create SA for each testing instance
  sa.values <- sapply(X = 1:nrow(data.pointer$test), phaseOne, data.pointer, model, attr, cl, ...)

  return(sa.values)
}

phaseOne <- function(x, data.pointer, model, attr, cl, ...){
  # Create phaseOne prediction for x-th testing instance
  prediction <- data.pointer$predictF(model, data.pointer$test[x,], ...)

  # Define epsilon
  eps <- c(-0.01, -0.1, -0.5, -1, -2, 0.01, 0.1, 0.5, 1, 2)
  data.max <- max(data.pointer$train[attr])
  data.min <- min(data.pointer$train[attr])

  # Create prediction for each eps (pos and negative)
  if(data.pointer$nThread > 1) pos.eps <- parallel::parSapply(cl, 1:length(eps), phaseTwo, data.pointer$test[x,], data.pointer, attr, data.max, data.min, prediction, eps, ...)
  else pos.eps <- sapply(1:(length(eps)), phaseTwo, data.pointer$test[x,], data.pointer, attr, data.max, data.min, prediction, eps, ...)


   # Calculate SAvar and SAbias
  sa.var = sum(pos.eps[6:10] - pos.eps[1:5]) / 5
  sa.bias = ( sum(pos.eps[6:10] - prediction) + sum(pos.eps[1:5] - prediction) ) /  10
  return(c(sa.var, sa.bias))

}

# Creates new data set by appending the newly generated training instance
# Builds the model and return the new predicted value
phaseTwo <- function(x, data.test, data.pointer, attr, data.max, data.min, k, eps, ...){

  # Modify the testing instance by adding delta to the
  delta <- eps[x] * (data.max - data.min)

  # Create new instance to add to data.train
  study.instance <- data.test
  study.instance[attr] <- k + delta

  # Append new instance
  #print(paste("ASDF", k+delta))
  data.train <- rbind(data.pointer$train, study.instance)

  model <- data.pointer$modelF(data.pointer$formula, data.train, ...)
  prediction <- data.pointer$predictF(model, data.pointer$test, ...)
  return(prediction)
}
