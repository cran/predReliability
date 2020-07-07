# Data Pointer class for faster function calls (https://www.stat.berkeley.edu/~paciorek/computingTips/Pointers_passing_reference_.html)
newDataPointer <- function(data.test, data.train, types, formula, model.function, predict.function, nThread){  
  object <- new.env(parent<- globalenv())  
  object$train <- data.train  
  object$test <- data.test
  object$types <- types
  object$formula <- formula
  object$modelF <- model.function
  object$predictF <- predict.function
  object$nThread <- nThread
  class(object) <- 'pointer'
  
  return(object)  
}

# General pointer class
newPointer <- function(value){  
  object <- new.env(parent<- globalenv())  
  object$value <- value
  class(object) <- 'pointer'
  
  return(object)  
}

