# Test case 1: Using the iris dataset for classification
data(iris)
X <- iris[, -5]  # Features
y <- iris$Species  # Target variable

# Call the function for classification
results_classification <- ensemble_model_fitting(X, y, model_type = 'binomial')

# Test case 2: Using the mtcars dataset for regression
data(mtcars)
X <- mtcars[, -which(names(mtcars) == "mpg")]  # Features
y <- mtcars$mpg  # Target variable

# Call the function for regression
results_regression <- ensemble_model_fitting(X, y, model_type = 'gaussian')
