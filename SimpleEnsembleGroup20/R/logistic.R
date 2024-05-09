#' Fit a Logistic Regression Model with Optional Bagging
#'
#' @description Fits a logistic regression model using maximum likelihood estimation. Supports both numerical
#' and categorical variables by converting categorical variables into dummy variables.
#' When bagging is TRUE, performs bagging to improve the robustness of the model fit.
#'
#' @param y Response variable, numeric vector of binary outcomes (0s and 1s).
#' @param X Predictor variables, matrix or data frame.
#' @param intercept Logical, if TRUE, an intercept term is added to the model.
#' @param bagging Logical, if TRUE, performs bagging.
#' @param R Integer, number of bootstrap samples for bagging.
#' @return A list containing model details such as coefficients, standard errors,
#'         z-values, p-values, and AIC values. If bagging is TRUE, returns
#'         a list of aggregated results from the bagging procedure.
#' @importFrom MASS ginv
#' @export
#' @examples
#' data(iris)
#' model <- logistic(iris$Species == "versicolor", iris[, -5], intercept = TRUE, bagging = FALSE)
#' print(model)
logistic <- function(y, X, intercept = TRUE, bagging = FALSE, R = 100) {
  if (intercept) {
    X <- cbind(Intercept = 1, X)
  }

  if (!bagging) {
    op <- logistic_internal(y, X, intercept)
  } else {
    op <- perform_bagging(y, X, function(response_sample, predictor_sample) {
      logistic_internal(response_sample, predictor_sample, intercept)
    }, R)
  }

  op$type <- "logistic"  # Identify the model type for predict_model compatibility
  op$names <- colnames(X)
  return(op)
}


#' Internal function to fit logistic regression model
#'
#' @param y Response variable vector of binary outcomes (0s and 1s).
#' @param X Design matrix with predictors.
#' @return A list containing model fitting details.

logistic_internal <- function(y, X, intercept) {
  if (intercept) {
    X <- cbind(Intercept = 1, X)
  }

  model <- glm(y ~ ., data = as.data.frame(X), family = binomial(link = "logit"))

  coefficients <- coef(model)
  se_coefficients <- summary(model)$coefficients[, "Std. Error"]
  z_values <- coefficients / se_coefficients
  p_values <- 2 * pnorm(-abs(z_values))
  AIC_value <- AIC(model)

  # Summary table construction
  summary_table <- data.frame(
    Estimate = coefficients,
    StdError = se_coefficients,
    zValue = z_values,
    Prz = p_values
  )

  # Set the row names of the summary table to match the coefficient names
  betacols <- colnames(X)
  if (length(coefficients) == length(betacols)) {
    rownames(summary_table) <- betacols
  } else {
    stop("Mismatch in the number of coefficients and predictor names")
  }

  names(coefficients) <- colnames(X)

  # Return a detailed list
  return(list(
    coefficients = coefficients,
    se_coefficients = se_coefficients,
    z_values = z_values,
    p_values = p_values,
    AIC = AIC_value,
    summary = summary_table,
    type = "logistic"  # Identifying the model type for prediction function
  ))
}
