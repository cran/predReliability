lcvFun <- function(data.pointer, cl = NULL, n = 20, ...){

  # Check for correct arguments
  attr <- strsplit(deparse(data.pointer$formula), " ~ ", fixed = T)
  attr <- sapply(attr, "[[", 1)
  if(sapply(data.pointer$train[attr], class) != "numeric")
    stop("Predicted argument must be numeric for lcv reliability test")

  dm <- NULL

  # Adjust n neighbors
  n <- max(4, trunc(nrow(data.pointer$train) / n))

  # Create model and predict target variable
  model <- data.pointer$modelF(data.pointer$formula, data.pointer$train, ...)
  predictions <- data.pointer$predictF(model, data.pointer$test, ...)

  # Merge testing and study data together
  # Create dis matrix by using daisy
  # Transform into matrix
  # The distance vector starts at nrow(data) + 1 since we want to skip testing isntances
  # Wrap into a pointer for future use
  dm <- newPointer(utils::tail(as.matrix(cluster::daisy(rbind(data.pointer$test[-(names(data.pointer$test) == attr)], data.pointer$train[-(names(data.pointer$train) == attr)])), stand = T), nrow(data.pointer$train)))

  ret.val <- sapply(1:nrow(data.pointer$test), lcv, n, dm, data.pointer)

  return(ret.val)
}


lcv <- function(x, n,  dm, data.pointer, ...){
  # Calculate index order of n closest
  index <- utils::head(order(dm$value[, x], decreasing = F), n)

  # Calculate loo error for each set of neighbors
  loo.err <- unlist(sapply(1:n, loo, data.pointer$train[index, ], data.pointer))

  # Calculate LCV
  d <- stats::dnorm(10 * dm$value[index, x])
  if(sum(d) == 0) d = 1
  return(sum(d * loo.err) / sum(d))

}

loo <- function(x, data, data.pointer, ...){

  model <- data.pointer$modelF(data.pointer$formula, data.pointer$train[-x,])
  prediction <- data.pointer$predictF(model, data[x,])
  return(abs(data[x, 1] - prediction))
}
