# Load the mtcars dataset
data(mtcars)

# Fit a linear model using the 'linearinternal' function
model <- linearinternal(mtcars$mpg, as.matrix(mtcars[, -1]), intercept = TRUE)

# Print the model
print(model)
