# Example usage with the Boston dataset
library(MASS)
data(Boston)
X <- Boston[, c("lstat", "rm")]
y <- Boston$medv
result <- lasso(y = y, X = X, type = "gaussian", baggingformodels = FALSE)
print(result)
