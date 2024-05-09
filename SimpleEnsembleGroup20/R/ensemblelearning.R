#' @title Ensemble Model Fitting Using Standardized Prediction
#' @description Fits multiple models specified by the model list to the same dataset and combines their predictions using a
#' standardized prediction method. Supports regression (gaussian) and classification (binomial) types.
#' @param X Data frame or matrix of predictors.
#' @param y Vector of response variables.
#' @param model_type Character specifying whether to fit models for 'gaussian' (regression) or 'binomial' (classification).
#' @param model_list List of model fitting functions expressed as character strings.
#' @param threshold Threshold for converting probabilities to class labels in case of 'binomial' model type, default is 0.5.
#' @return A list containing combined predictions and details for each fitted model including individual model predictions,
#' as well as the accuracy/RMSE for regression and classification tasks.
#' @export
#' @examples
#'   data(mtcars)
#'   X <- mtcars[, -which(names(mtcars) == "mpg")]
#'   y <- mtcars$mpg  # For gaussian; use a binary response for 'binomial'
#'   model_list <- c("fit_linear_model", "fit_random_forest_model")
#'   results <- ensemble_model_fitting(X, y, model_type = 'gaussian', model_list = model_list)
#'   print(results$combined_predictions)
#'   # To view individual model details and predictions:
#'   print(results$model_details)
#'   print(results$individual_predictions)
library(caret)
library(xgboost)
library(randomForest)
library(glmnet)

ensemble_model_fitting <- function(X, y, model_type, folds = 10) {
  default_models <- if (model_type == 'binomial') {
    list(
      c("logistic"),
      c(1)
    )
  } else {
    list(
      c("linear", "randomForest", "ridge"),
      c(0.4, 0.4, 0.2)
    )
  }

  default_choice <- readline(prompt = "Do you want to run with default models? (yes/no): ")

  if (tolower(default_choice) == "yes") {
    selected_models <- default_models[[1]]
    proportions <- default_models[[2]]
  } else {
    if (!model_type %in% c('gaussian', 'binomial')) {
      stop("Unsupported model type provided: ", model_type)
    }

    # Define model choices based on the model type
    model_choices <- if (model_type == 'binomial') {
      c("logistic")
    } else {
      c("linear", "randomForest", "ridge", "lasso", "elasticNet")
    }

    cat("Available models for", model_type, ":\n")
    print(model_choices)

    # Get model selections from the user
    input <- readline(prompt = "Enter model numbers (separated by commas): ")
    model_indices <- as.numeric(strsplit(input, ",")[[1]])
    if (length(model_indices) == 0 || any(is.na(model_indices))) {
      cat("No valid models selected, using default models.\n")
      model_indices <- 1:min(2, length(model_choices))  # Default to first two models
    }
    selected_models <- model_choices[model_indices]

    # Get proportions from the user
    proportions <- sapply(selected_models, function(m) {
      as.numeric(readline(paste("Enter proportion for", m, ": ")))
    })

    # Normalize or set default proportions if input is invalid
    if (length(proportions) != length(selected_models) || any(is.na(proportions)) || sum(proportions) == 0) {
      cat("Invalid proportions provided, using equal proportions.\n")
      proportions <- rep(1 / length(selected_models), length(selected_models))
    } else {
      proportions <- proportions / sum(proportions)  # Normalize
    }
  }

  predictions_list <- list()
  model_details_list <- list()

  # Fit models and collect predictions
  for (i in seq_along(selected_models)) {
    model_name <- selected_models[i]
    proportion <- proportions[i]
    cat("Fitting model:", model_name, "with proportion", proportion, "\n")

    # Create a formula and fit the model
    formula <- as.formula(paste("y ~ ."))
    data <- data.frame(y = y, X)
    if (model_name == "logistic") {
      model_fit <- glm(formula, data = data, family = binomial())
    } else if (model_name == "linear") {
      model_fit <- lm(formula, data = data)
    } else if (model_name == "randomForest") {
      model_fit <- randomForest(formula, data = data)
    } else if (model_name == "ridge") {
      model_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = 0, nfolds = folds, family = ifelse(model_type == "binomial", "binomial", "gaussian"))
    } else if (model_name == "lasso") {
      model_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = 1, nfolds = folds, family = ifelse(model_type == "binomial", "binomial", "gaussian"))
    } else if (model_name == "elasticNet") {
      model_fit <- cv.glmnet(x = as.matrix(X), y = y, alpha = 0.5, nfolds = folds, family = ifelse(model_type == "binomial", "binomial", "gaussian"))
    } else {
      next  # Skip if model name is not recognized
    }

    if (!is.null(model_fit)) {
      if (model_name %in% c("logistic") && model_type == "binomial") {
        predictions <- predict(model_fit, newx = as.matrix(X), type = "response")
      } else {
        predictions <- predict(model_fit, newx = as.matrix(X))
      }
      predictions_list[[length(predictions_list) + 1]] <- predictions * proportion
      model_details_list[[length(model_details_list) + 1]] <- model_fit
    }
  }

  # Combine predictions
  combined_predictions <- Reduce("+", predictions_list)

  # Output combined predictions and model details
  cat("Combined Predictions:\n")
  print(combined_predictions)
  cat("\nModel Details:\n")
  print(model_details_list)

  return(list(
    combined_predictions = combined_predictions,
    individual_predictions = predictions_list,
    model_details = model_details_list
  ))
}
