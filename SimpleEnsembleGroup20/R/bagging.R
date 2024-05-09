#' Perform Bagging for Regression Models (excluding linear and logistic)
#'
#'With the exception of linear and logistic regression, this function offers a generic bagging approach for regression models.
#'To increase the stability and accuracy of the model, it takes a dataset, samples it repeatedly using replacement, fits a model on each sample, and then averages the
#'#' results. It calculates variable relevance scores, t-values,
#'#' p-values, averaged coefficients, and standard errors.
#'
#' @param y Response vector with outcomes.
#' @param X Predictor matrix or data frame.
#' @param model_type The type of regression model to fit. Options: "ridge", "lasso", "elastic_net", "svm", "random_forest".
#' @param R The number of bootstrap replicates to use in the bagging process.
#'
#' @return A list containing:
#''coefficients': Averaged coefficients across all bootstrap samples.
#''se_coefficients': Standard errors of the averaged coefficients.
#' 't_values': t-values computed from averaged coefficients and their standard errors.
#'   - 'p_values': p-values associated with the t-values.
#'   - 'predictions': Averaged predictions across all bootstrap samples.
#'   - 'variable_importance': Importance scores for each predictor, averaged across all samples.
#'
#' @examples
#' # Example usage with Ridge Regression
#' data(mtcars)
#' results <- perform_bagging(mtcars$mpg, mtcars[, -1], model_type = "ridge", R = 100)
#' print(results)
#'
#' @export
# Define a Ridge Regression fitting function as an example
# Load necessary libraries
library(glmnet)

# Define the generic bagging function for regularized regression models
perform_bagging <- function(y, X, model_type, alpha, lambda = NULL, R = 100) {
  n <- length(y)
  results <- replicate(R, {
    indices <- sample(seq_len(n), replace = TRUE)
    # Determine lambda using cross-validation if not provided
    if (is.null(lambda)) {
      cv_fit <- cv.glmnet(x = X[indices, , drop = FALSE], y = y[indices], alpha = alpha)
      lambda <- cv_fit$lambda.min
    }
    # Fit the model using the sampled indices
    fit <- glmnet(x = X[indices, , drop = FALSE], y = y[indices], alpha = alpha, lambda = lambda)
    # Predict using the full dataset for consistency in output dimension
    predict(fit, newx = X, s = lambda)
  }, simplify = FALSE)  # Use simplify = FALSE to store results as a list

  # Calculate mean and standard deviation across all bootstrap predictions
  pred_matrix <- do.call(cbind, results)  # Bind all predictions into a matrix
  list(
    mean_prediction = rowMeans(pred_matrix),
    sd_prediction = apply(pred_matrix, 1, sd)
  )
}
