#testcase: 1
data(iris)
model <- logistic(iris$Species == "versicolor", iris[, -5], intercept = TRUE, bagging = FALSE)
print(model)
