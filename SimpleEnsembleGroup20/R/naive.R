#' Perform Bagging for Regression Models
#'
#' This function implements a generic bagging procedure for regression models. It takes a dataset, repeatedly samples
#' from it with replacement, fits a model on each sample, and then averages the results to improve model stability and
#' accuracy. It computes averaged coefficients, standard errors, t-values, p-values, and variable importance scores.
#'
#' @param y Response vector with outcomes.
#' @param X Predictor matrix or data frame.
#' @param naive A function that fits a model and returns a list containing at least coefficients and fitted values.
#' This function must accept two arguments: y (response) and X (predictors).
#' @param R The number of bootstrap replicates to use in the bagging process.
#'
#' @return A list containing:
#'   - coefficients` Averaged coefficients across all bootstrap samples.
#'   - coef` Standard errors of the averaged coefficients.
#'   - t_values t-values computed from averaged coefficients and their standard errors.
#'   - p_values p-values associated with the t-values.
#'   - predictions` Averaged predictions across all bootstrap samples.
#'   - variable_importance`Importance scores for each predictor, averaged across all samples.
#'
#' @examples
#' # Example usage with a linear model fitting function
#' data(mtcars)
#' naive <- function(y, X) {
#'   model <- lm(y ~ X)
#'   list(coefficients = coef(model), fitted_values = predict(model))
#' }
#' results <- bagging_perform(mtcars$mpg, mtcars[, -1], naive, R = 100)
#' print(results)
#'
#' @export
bagging_perform <- function(y, X, naive, R) {
  n <- length(y)
  coefficients_list <- list()
  predictions_matrix <- matrix(NA, nrow = n, ncol = R)  # Matrix to store predictions for each bootstrap sample
  variable_importance <- numeric(ncol(X))

  for (i in 1:R) {
    idx <- sample(1:n, replace = TRUE)
    predictor_sample <- X[idx, , drop = FALSE]
    response_sample <- y[idx]

    model <- naive(response_sample, predictor_sample)
    if (!is.null(model$coefficients) && length(model$fitted_values) == n) {
      coefficients_list[[i]] <- as.vector(model$coefficients)
      predictions_matrix[, i] <- model$fitted_values
      variable_importance <- variable_importance + (model$coefficients != 0)
    } else {
      cat("Mismatch or NULL data in iteration:", i, "\nLength of fitted_values:", length(model$fitted_values), "Expected:", n, "\n")
    }
  }

  # Calculate mean and standard deviation of coefficients
  all_coefs <- do.call(cbind, coefficients_list)
  mean_coefs <- rowMeans(all_coefs, na.rm = TRUE)  # Calculate row-wise mean
  std_coefs <- apply(all_coefs, 1, sd, na.rm = TRUE)  # Standard deviation by row
  coef <- std_coefs / sqrt(R)  # Approximate standard error

  # Recompute t-values and p-values
  t_values <- mean_coefs / coef
  df <- n - ncol(X) - 1
  p_values <- 2 * pt(-abs(t_values), df = df)

  # Calculate average predictions
  final_predictions <- rowMeans(predictions_matrix, na.rm = TRUE)

  # Normalize variable importance
  variable_importance <- variable_importance / R

  return(list(coefficients = mean_coefs, coef = coef, t_values = t_values, p_values = p_values, predictions = final_predictions, variable_importance = variable_importance))
}

