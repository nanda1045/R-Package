#' @title Fit Lasso Model
#'
#' @description Constructs a regularized predictive model using the glmnet package, utilizing L1 regularization
#' (lasso penalty) to balance model complexity and predictive accuracy. This technique addresses multicollinearity
#' concerns, mitigates overfitting, and encourages sparsity in coefficients, proving advantageous when the
#' number of predictors outstrips the number of observations. Optionally, a bagging strategy (bootstrap aggregation)
#' can be employed to bolster model robustness and predictive performance by amalgamating results from multiple
#' bootstrap samples.
#'
#' @param y Response vector, either continuous for regression or binary for classification.
#' @param X Matrix or data frame of predictor variables, either numeric or categorical.
#' @param regularization_param Regularization strength parameter for lasso penalty; if NULL, determined via cross-validation.
#' @param intercept Logical indicating whether to include an intercept term in the model.
#' @param type A character string specifying the target type: 'binomial' for classification or 'gaussian' for regression.
#' @param baggingformodels Logical indicating whether to employ bootstrap aggregation (bagging).
#' @param R Integer specifying the number of bootstrap samples for bagging if enabled.
#' @return A list containing the regularized predictive model details, or if baggingformodels is enabled, an aggregated
#' result from multiple bootstrap samples, including the model, regularization parameter value, coefficients,
#' and predicted values (continuous for gaussian, probabilities for binomial).
#' @importFrom glmnet glmnet cv.glmnet
#' @export
#' @examples
#' # Example for Gaussian Model (Regression)
#' data(Boston)
#' X <- Boston[, c("lstat", "rm")]
#' y <- Boston$medv
#' result <- lasso(y = y, X = X, type = "gaussian", baggingformodels = FALSE)
#' print(result)
#'
#' # Example for Binomial Model (Classification)
#' data(PimaIndiansDiabetes)
#' X <- PimaIndiansDiabetes[, c("glucose", "BMI")]
#' y <- PimaIndiansDiabetes$diabetes
#' result <- lasso(y = y, X = X, type = "binomial", baggingformodels = TRUE, R = 50)
#' print(result)

lasso <- function(y, X, regularization_param = NULL, intercept = TRUE, baggingformodels = FALSE, type = "gaussian", R = 100) {
  validate_data(y, X)

  if (!type %in% c("binomial", "gaussian")) {
    stop("'type' must be either 'binomial' or 'gaussian'")
  }

  family_spec <- type

  if (is.null(regularization_param)) {
    res <- cv.glmnet(x = as.matrix(X), y = y, alpha = 1, family = family_spec, intercept = intercept)
    regularization_param <- res$lambda.min
  }
  op <- if (baggingformodels) {
    perform_bagging(y, X, function(y_sample, X_sample) {
      fit <- glmnet(x = as.matrix(X_sample), y = y_sample, alpha = 1, lambda = regularization_param, family = family_spec, intercept = intercept)
      list(coefs = coef(fit), preds = predict(fit, newx = as.matrix(X_sample), type = "response"))
    }, R)
  } else {
    fit <- glmnet(x = as.matrix(X), y = y, alpha = 1, lambda = regularization_param, family = family_spec, intercept = intercept)

    preds <- predict(fit, newx = as.matrix(X), type = "response", s = "lambda.min")
    coefs <- coef(fit, s = "lambda.min") # Include intercept if intercept is TRUE
    list(model = fit, regularization_param = regularization_param, coefs = coefs, preds = preds)
  }
  op$type <- type # Identifying the target type for prediction function
  if (intercept) {
    op$names <- c("Intercept", colnames(X))
  } else {
    op$names <- colnames(X)
  }
  return(op)
}
