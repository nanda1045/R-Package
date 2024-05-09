# Example usage with the Boston dataset
library(MASS)
data(Boston)
X <- Boston[, c("lstat", "rm")]
y <- Boston$medv
result <- elasticnet(y = y, X = X, type = "gaussian", baggingformodel = FALSE)
print(result)

