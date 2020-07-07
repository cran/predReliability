correlates <- function(data.all, types, formula, model.function, predict.function){
  # Split the data into 10 folds
  # Generate base fold size
  fold.base <- nrow(data.all) / 10
  # Calculate error
  fold.diff <- nrow(data.all) - as.integer(fold.base) * 10
  # Add base to empty folds
  folds <- integer(10) + as.integer(fold.base)
  # Add diff to the first folds
  folds[1:fold.diff] <- folds[1:fold.diff] + 1

  # Save begining
  beg <- 0
  index <- 1
  fold.data <- list()
  for(i in folds){
    fold <- (beg + 1):(beg + i)
    fold.data[[index]] <- getCor(data.all[fold,], data.all[-(fold),], types, formula, model.function, predict.function)
    beg <- beg + i
    index <- index + 1
  }

  # Merge all data into one data frame
  fold.data <- do.call("rbind", fold.data)
  # Final cor data.frame
  fold.cor <- data.frame(integer(1))
  row.names(fold.cor) <- "Cor"

  # Go over all types and handle them one by one
  fold.len <- 1
  for(type in types){
    if(type == 'bagv'){
      fold.cor[1, fold.len] <- stats::cor.test(fold.data[, type], fold.data[, "absErr"], method = "pearson")$estimate
      names(fold.cor)[fold.len] <- type
      fold.len <- fold.len + 1
    }
    else if(type == 'cnk'){
      fold.cor[1, fold.len] <- stats::cor.test(fold.data[, type], fold.data[, "absErr"], method = "pearson")$estimate
      fold.cor[1, fold.len + 1] <- stats::cor.test(fold.data[, type], fold.data[, "sigErr"], method = "pearson")$estimate
      names(fold.cor)[c(fold.len, fold.len + 1)] <- c("cnk abs", "cnk sig")
      fold.len <- fold.len + 2
    }
    else if(type == 'lcv'){
      fold.cor[1, fold.len] <- stats::cor.test(fold.data[, type], fold.data[, "absErr"], method = "pearson")$estimate
      names(fold.cor)[fold.len] <- type
      fold.len <- fold.len + 1
    }
    else if(type ==  'sa'){
      fold.cor[1, fold.len] <- stats::cor.test(fold.data[, "sa.var"], fold.data[, "absErr"], method = "pearson")$estimate
      fold.cor[1, fold.len + 1] <- stats::cor.test(fold.data[, "sa.bias"], fold.data[, "absErr"], method = "pearson")$estimate
      fold.cor[1, fold.len + 2] <- stats::cor.test(fold.data[, "sa.bias"], fold.data[, "sigErr"], method = "pearson")$estimate
      names(fold.cor)[fold.len:(fold.len + 2)] <- c("sa.var", "sa.bias abs", "sa.bias sig")
      fold.len <- fold.len + 2
    }
  }

  return(fold.cor)
}

getCor <- function(data.test, data.train, types, formula, model.function, predict.function){
  # Generate prediction model and predictions
  model <- model.function(formula, data.train)
  preds <- predict.function(model, data.test)
  # Get target class name
  attr <- strsplit(deparse(formula), " ~ ", fixed = T)
  attr <- sapply(attr, "[[", 1)

  # Calculate Abs and signed error
  sigError <- data.test[names(data.test) == attr] - preds
  names(sigError) <- "sigErr"
  absError <- abs(sigError)
  names(absError) <- "absErr"

  # Get Rel estimates
  estimates <- predReliability(data.test, data.train, types, formula, model.function, predict.function)

  return(cbind(estimates, absError, sigError))
}

