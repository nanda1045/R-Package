library(randomForest)

#' Fit a Random Forest Model for Binomial or Gaussian Distributions
#'
#' @param y The response variable, either a numeric vector for Gaussian regression or a factor vector for binomial classification.
#' @param X The data frame of predictor variables.
#' @param type A character string specifying the type of model: 'gaussian' for regression or 'binomial' for classification.
#' @param num_trees The number of trees to grow in the random forest.
#' @param mtry The number of variables to consider at each split. If NULL, the default value will be used.
#' @return A list containing the random forest model object and the model predictions.
randomforest <- function(y, X, type = 'gaussian', num_trees = 500, mtry = NULL) {
  # Validate the input data
  validate_data(y, X)

  # Determine the appropriate random forest model type
  if (type == 'gaussian') {
    if (is.null(mtry)) {
      mtry <- floor(sqrt(ncol(X)))
    }
    rf <- randomForest(x = X, y = y, ntree = num_trees, mtry = mtry, type = 'regression')
    model_predictions <- predict(rf, newdata = X)
  } else if (type == 'binomial') {
    y <- factor(y)
    if (is.null(mtry)) {
      mtry <- floor(sqrt(ncol(X)))
    }
    rf <- randomForest(x = X, y = y, ntree = num_trees, mtry = mtry, type = 'classification')
    model_predictions <- predict(rf, newdata = X, type = 'prob')[, 2]
  } else {
    stop("Invalid type. Must be either 'gaussian' or 'binomial'.")
  }

  return(list(model = rf, predictions = model_predictions))
}

#' Validate the input data for the random forest model
#'
#' @param y The response variable.
#' @param X The data frame of predictor variables.
#' @return NULL (invisible)
validate_data <- function(y, X) {
  if (length(y) != nrow(X)) {
    stop("The number of observations in the target variable and predictor data must be the same.")
  }

  if (any(is.na(y)) || any(is.na(X))) {
    stop("The input data must not contain any missing values.")
  }
}

