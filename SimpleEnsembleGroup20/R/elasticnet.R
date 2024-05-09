#' @title Fit Elastic Net Model
#'
#' @description Constructs a regularized predictive model using the glmnet package, balancing model
#' complexity and predictive prowess through a combination of L1 (lasso) and L2 (ridge) regularization penalties.
#' This technique addresses multicollinearity concerns, mitigates overfitting, and encourages sparsity in coefficients,
#' proving advantageous when the number of predictors outstrips the number of observations. Optionally, a
#' bagging strategy (bootstrap aggregation) can be employed to bolster model robustness and predictive
#' performance by amalgamating results from multiple bootstrap samples.
#'
#' @param y Response vector, continuous for regression or binary for classification.
#' @param X Matrix of predictor variables, numeric or categorical.
#' @param regularization_param Regularization strength parameter for elastic net penalty; if NULL, determined via cross-validation.
#' @param alpha Mixing parameter for elastic net penalty, with values between 0 and 1.
#' A value of 1 corresponds to lasso penalty, while 0 corresponds to ridge penalty.
#' @param intercept Logical indicating whether to include an intercept term in the model.
#' @param type A character string specifying the target type: 'binomial' for classification or 'gaussian' for regression.
#' @param baggingformodel Logical indicating whether to employ bootstrap aggregation (bagging).
#' @param R Integer specifying the number of bootstrap samples for bagging if enabled.
#' @return A list containing the regularized predictive model details, or if baggingformodel is enabled, an aggregated
#' result from multiple bootstrap samples, including the model, regularization parameter value, coefficients,
#' and predicted values (continuous for gaussian, probabilities for binomial).
#' @importFrom glmnet glmnet cv.glmnet
#' @export
#' @examples
#' # Example for Gaussian Model (Regression)
#' data(Boston)
#' X <- Boston[, c("lstat", "rm")]
#' y <- Boston$medv
#' result <- elasticnet(y = y, X = X, type = "gaussian", alpha = 0.5, baggingformodel = FALSE)
#' print(result)
#'
#' # Example for Binomial Model (Classification)
#' data(PimaIndiansDiabetes)
#' X <- PimaIndiansDiabetes[, c("glucose", "BMI")]
#' y <- PimaIndiansDiabetes$diabetes
#' result <- elasticnet(y = y, X = X, type = "binomial", alpha = 0.5, baggingformodel = TRUE, R = 50)
#' print(result)

elasticnet <- function(y, X, regularization_param = NULL, alpha = 0.5, intercept = TRUE, baggingformodel = FALSE, type = "gaussian", R = 100) {
  validate_data(y, X)

  if (!(is.numeric(alpha) && alpha >= 0 && alpha <= 1)) {
    stop("The 'alpha' parameter should be a numeric value within the range [0, 1]")
  }

  if (!type %in% c("binomial", "gaussian")) {
    stop("'type' must be either 'binomial' or 'gaussian'")
  }

  family_spec <- type

  if (is.null(regularization_param)) {
    res <- cv.glmnet(x = as.matrix(X), y = y, alpha = alpha, family = family_spec, intercept = intercept)
    regularization_param <- res$lambda.min
  }
  op <- if (baggingformodel) {
    perform_bagging(y, X, function(y_sample, X_sample) {
      fit <- glmnet(x = as.matrix(X_sample), y = y_sample, alpha = alpha, lambda = regularization_param, family = family_spec, intercept = intercept)
      list(coefs = coef(fit), preds = predict(fit, newx = as.matrix(X_sample), type = "response"))
    }, R)
  } else {
    fit <- glmnet(x = as.matrix(X), y = y, alpha = alpha, lambda = regularization_param, family = family_spec, intercept = intercept)

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
