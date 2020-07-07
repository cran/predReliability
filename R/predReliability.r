#' A reliability function
#'
#' A function used to calculate the reliability of individual predictions given by your model and prediction function with methods described in the paper (Bosnic, Z., & Kononenko, I. (2008) <doi:10.1007/s10489-007-0084-9>). It also allows you to make a correlation test to estimate which reliability estimate is the most accurate for your model.
#' @param data.test         a \code{\link[base]{data.frame}} object used as the testing data for your prediction model
#' @param data.train        a \code{\link[base]{data.frame}} object used as the training data for your prediction model
#' @param types              a \code{\link[base]{vector}} of reliability test types you want to perform c("bagv", "cnk", "lcv", "sa")
#' @param formula           a \code{\link[stats]{formula}} describing the model to be fitted
#' @param model.function    a function with arguments \code{\link[stats]{formula}} and \code{\link[base]{data.frame}} implementing the predictive model to be evaluated. The function model must return an onject representing a fitted model.
#' @param predict.function  a function with arguments model object \code{\link[base]{data.frame}} of testing instances that will be predicted based on the given model.
#' @param ceval             a flag whether a 10-fold correlation test should be made on the requested types (default set to false)
#' @param nThread           the number
#' @param ...               extra arguments you wish to be passed to your model and prediction function
#' @keywords predReliability
#' @export
#' @references
#' Bosnic, Z., & Kononenko, I. (2008). Comparison of approaches for estimating reliability of individual regression predictions. Data & Knowledge Engineering, 67(3), 504-516.
#' Bosnic, Z., & Kononenko, I. (2008). Estimation of individual prediction reliability using the local sensitivity analysis. Applied intelligence, 29(3), 187-203.
#' Bosnic, Z., & Kononenko, I. (2009). An overview of advances in reliability estimation of individual predictions in machine learning. Intelligent Data Analysis, 13(2), 385-401.
#' @importFrom cluster daisy
#' @examples
#'
#' estimates <- c("bagv", "cnk", "lcv", "sa")
#' predReliability(mtcars[1,], mtcars[-1,], estimates, mpg~., rpart::rpart, predict)

predReliability <- function(data.test, data.train, types, formula, model.function, predict.function, ceval = F, nThread = 1, ...){
  data.pointer <- NULL
  #pack the data into a pointer for faster function calling
  data.pointer <- newDataPointer(data.test, data.train, types, formula, model.function, predict.function, nThread)


  # Create a cluster in the user requested parallel execution
  # Give all the threads the neccassary data
  cl <- NULL
  if(nThread > 1){
    cl <- parallel::makeCluster(nThread)
  }

  # Generate return value data.frame
  return.value <- data.frame(first = integer(nrow(data.test)))



  for(type in types){
    return.col <- ncol(return.value) + 1

      if(type == 'bagv'){
      rel <- bagFun(data.pointer, cl, ...)

      return.value[, return.col] <- rel
      names(return.value)[return.col] = "bagv"
    }
    else if(type == 'cnk'){
      rel <- cnkFun(data.pointer, cl, ...)
      return.value[, return.col] <- rel
      names(return.value)[return.col] = "cnk"
    }
    else if(type == 'lcv'){
      rel <- lcvFun(data.pointer, cl, ...)

      return.value[, return.col] <- rel
      names(return.value)[return.col] = "lcv"
    }
    else if(type ==  'sa'){
      rel <- saFun(data.pointer, cl, ...)

      return.value[, return.col] <- rel[1,]
      names(return.value)[return.col] = "sa.var"
      return.value[, return.col + 1] <- rel[2,]
      names(return.value)[return.col + 1] = "sa.bias"
    }
    else stop("No reliability test for given type. Possible values are: bagv, cnk, lcv, sa")
  }


  return.value$first <- NULL



  if(ceval){
    return.list <- list(return.value)
    names(return.list) <- "Reliability"
    return.list[[2]] <- correlates(rbind(data.test, data.train), types, formula, model.function, predict.function)
    names(return.list)[2] <- "Correlation"
    return(return.list)
  }

  if(nThread > 1) parallel::stopCluster(cl)

  return(return.value)
}







