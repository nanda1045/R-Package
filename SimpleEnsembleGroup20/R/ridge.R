#' @title Fit Ridge Model
#'
#' @description Constructs a regularized predictive model using the glmnet package, utilizing L2 regularization
#' (ridge penalty) to balance complexity and predictive accuracy. This method helps manage multicollinearity,
#' reduces overfitting, and promotes coefficient shrinkage, which is beneficial when predictor count exceeds
#' observation count. Optionally, bootstrap aggregation (bagging) can be used to enhance model stability and
#' predictive performance by combining results from multiple bootstrap samples.
#'
#' @param y Response vector, either continuous for regression or binary for classification.
#' @param X Matrix or data frame of predictor variables, either numeric or categorical.
#' @param regularization_param Regularization strength for the ridge penalty; if NULL, determined via cross-validation.
#' @param intercept Logical, indicating whether to include an intercept term (default is TRUE, managed by standardization in glmnet).
#' @param type Character string specifying the model target type: 'binomial' for classification or 'gaussian' for regression.
#' @param baggingformodels Logical, indicating whether to employ bootstrap aggregation.
#' @param R Integer specifying the number of bootstrap samples for bagging.
#' @return A list containing the model details or, if bagging is employed, aggregated results from multiple samples.
#' @importFrom glmnet glmnet cv.glmnet
#' @export
#' @examples
#' # Gaussian Model (Regression)
#' library(MASS)
#' data(Boston)
#' X <- Boston[, c("lstat", "rm")]
#' y <- Boston$medv
#' result <- ridge(y = y, X = X, type = "gaussian", baggingformodels = FALSE)
#' print(result)
#'
#' # Binomial Model (Classification)
#' data(PimaIndiansDiabetes)
#' X <- PimaIndiansDiabetes[, c("glucose", "BMI")]
#' y <- PimaIndiansDiabetes$diabetes
#' result <- ridge(y = y, X = X, type = "binomial", baggingformodels = TRUE, R = 50)
#' print(result)

ridge <- function(y, X, regularization_param = NULL, intercept = TRUE, type = "gaussian", baggingformodels = FALSE, R = 100) {
  require(glmnet)

  validate_data(y, X)

  if (!type %in% c("binomial", "gaussian")) {
    stop("'type' must be either 'binomial' or 'gaussian'")
  }

  # Standardize X if intercept is not included
  X <- if (intercept) as.matrix(X) else scale(X, center = TRUE, scale = FALSE)

  # Cross-validation to find optimal lambda if not specified
  if (is.null(regularization_param)) {
    cv_fit <- cv.glmnet(x = X, y = y, alpha = 0, family = type)
    regularization_param <- cv_fit$lambda.min
  }

  # Fit model with or without bagging
  model_fit <- if (!baggingformodels) {
    glmnet(x = X, y = y, alpha = 0, lambda = regularization_param, family = type)
  } else {
    perform_bagging(y, X, type, regularization_param, R)
  }

  results <- list(
    model = model_fit,
    regularization_param = regularization_param,
    type = type
  )

  if (!baggingformodels) {
    results$coefficients <- coef(model_fit, s = "lambda.min")
    results$predictions <- predict(model_fit, newx = X, s = "lambda.min", type = if (type == "gaussian") "response" else "class")
  }

  return(results)
}

# Helper function to perform bagging
perform_bagging <- function(y, X, type, lambda, R) {
  samples <- replicate(R, {
    indices <- sample(length(y), replace = TRUE)
    glmnet(x = X[indices, , drop = FALSE], y = y[indices], alpha = 0, lambda = lambda, family = type)
  }, simplify = FALSE)

  # Aggregating coefficients example
  # Actual aggregation may need more sophisticated statistical approach
  list(
    models = samples
  )
}
