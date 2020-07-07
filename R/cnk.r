cnkFun <- function(data.pointer, cl = NULL, ...){

  # Check for correct arguments
  attr <- base::strsplit(base::deparse(data.pointer$formula), " ~ ", fixed = T)
  attr <- base::sapply(attr, "[[", 1)
  if(sapply(data.pointer$train[attr], class) != "numeric")
    stop("Predicted argument must be numeric for cnk reliability test")

  # Create model and predict target variable
  model <-  data.pointer$modelF(data.pointer$formula, data.pointer$train, ...)
  # Wrap into pointer for future use
  predictions <- newPointer(data.pointer$predictF(model, data.pointer$test, ...))

  # Merge testing and study data together
  # Create dis matrix by using daisy
  # Transform into matrix
  # The distance vector starts at nrow(data) + 1 since we want to skip testing isntances
  # Wrap into a pointer for future use
  a <- as.matrix(cluster::daisy(base::rbind(data.pointer$test[-(base::names(data.pointer$data.test) == attr)], data.pointer$train[-(base::names(data.pointer$train) == attr)]), stand = T))
  dm <- newPointer(utils::tail(a/base::max(a), base::nrow(data.pointer$train) - 1))

  # Create a new pointer to cut data
  data.pointer.cut <- newPointer(data.pointer$train[names(data.pointer$train) == attr])

  # Loop over all testing instances to calculate cnk
  predictions.cnk <- base::sapply(1:base::nrow(data.pointer$test), cnk, predictions, data.pointer.cut, dm, attr)

  return(predictions.cnk)

}

cnk <- function(x, predictions, data.train, dm, attr){
  # Find the index ordering of distances
  # take the first k = 5 values
  # Calculate its sum / 5 - predicted value
  a <-5
  return(base::mean(data.train$value[utils::head(base::order(dm$value[, x], decreasing = F), 5), attr]) - predictions$value[x])
}





