# Example usage for Gaussian regression
data(mtcars)
result_rf_gaussian <- randomforest(mtcars$mpg, mtcars[, -1], type = 'gaussian')
print(result_rf_gaussian)
