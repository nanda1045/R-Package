#test case: 1

data(mtcars)

naive <- function(response, predictors) {
  model <- lm(response ~ ., data = data.frame(response, predictors))
  list(coefficients = coef(model), fitted_values = predict(model))
}

results <- bagging_perform(mtcars$mpg, mtcars[, -1], naive, R = 100)
print(results)
