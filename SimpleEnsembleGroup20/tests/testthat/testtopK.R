#test case: 1

data(mtcars)
preddata <- mtcars[, -which(names(mtcars) == "mpg")]
result <- topk(preddata, 3)
print(result$toppreddata)
print(result$prednames)
print(result$scores)
