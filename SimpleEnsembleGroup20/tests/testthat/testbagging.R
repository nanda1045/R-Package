
# Example usage with the mtcars dataset
data(mtcars)
y <- mtcars$mpg
X <- as.matrix(mtcars[, -1])

# Ridge bagging (alpha = 0 for ridge regression)
ridge_results <- perform_bagging(y, X, model_type = "ridge", alpha = 0, R = 50)
print("Ridge Regression Results:")
print(ridge_results)

# Lasso bagging (alpha = 1 for lasso regression)
lasso_results <- perform_bagging(y, X, model_type = "lasso", alpha = 1, R = 50)
print("Lasso Regression Results:")
print(lasso_results)

# Elastic Net bagging (alpha between 0 and 1)
elastic_net_results <- perform_bagging(y, X, model_type = "elastic_net", alpha = 0.5, R = 50)
print("Elastic Net Regression Results:")
print(elastic_net_results)
