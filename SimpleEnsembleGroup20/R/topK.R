#' @title Select the Top K Informative Predictors Using SVD
#'
#' @description This function performs Singular Value Decomposition (SVD) on the predictor matrix \( X \)
#' and selects the top \( K \) most informative predictors based on the first right singular vector.
#' It checks that \( X \) is either a matrix or a data frame and ensures \( K \) is less than or equal to
#' the number of features in \( X \).
#'
#' @param predictor_matrix A matrix or data frame of predictors.
#' @param top_k The number of top informative predictors to return.
#' @return A list containing a data frame with the top \( K \) predictors, their names, and their scores.
#' @examples
#' \dontrun{
#' data(mtcars)
#' preddata <- mtcars[, -which(names(mtcars) == "mpg")]
#' result <- topk(preddata, 3)
#' print(result$toppreddata)
#' print(result$prednames)
#' print(result$scores)
#' }
#' @export
topk <- function(predictor_matrix, top_k) {
  if (!is.matrix(predictor_matrix) && !is.data.frame(predictor_matrix)) {
    stop("predictor_matrix must be a matrix or a data frame.")
  }

  if (top_k > ncol(predictor_matrix)) {
    stop("top_k must be less than or equal to the number of predictors in predictor_matrix.")
  }

  # Perform Singular Value Decomposition
  svd_result <- svd(predictor_matrix)

  # Extract the right singular vectors (V matrix)
  V_matrix <- svd_result$v

  # Identify the top K informative predictors based on the first right singular vector
  # Considering the absolute values to measure impact regardless of sign
  informative_scores <- abs(V_matrix[, 1])
  top_predictor_indices <- order(informative_scores, decreasing = TRUE)[1:top_k]

  # Retrieve the names of the top predictors
  top_prednames <- colnames(predictor_matrix)[top_predictor_indices]

  # Create a data frame of only the top K predictors
  toppreddata <- predictor_matrix[, top_prednames, drop = FALSE]

  return(list(
    toppreddata = toppreddata,
    prednames = top_prednames,
    scores = informative_scores[top_predictor_indices]
  ))
}
