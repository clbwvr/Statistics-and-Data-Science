?mtcars



plot(mpg~hp, data=mtcars, pch=20)
fit <- lm(mpg~hp, data=mtcars)
summary(fit)

# The mean squared error (MSE) is the mean of the square of the residuals:
mse <- mean(residuals(fit)^2)
mse

# Root mean squared error (RMSE) is then the square root of MSE:
rmse <- sqrt(mse)
rmse

# Residual sum of squares (RSS) is the sum of the squared residuals:
rss <- sum(residuals(fit)^2)
rss

# Residual standard error (RSE) is the square root of (RSS / degrees of freedom):
rse <- sqrt( sum(residuals(fit)^2) / fit$df.residual ) 
rse

# The term test error in the context of regression (and other predictive analytics techniques) 
# usually refers to calculating a test statistic on test data, distinct from your training data.
# In other words, you estimate a model using a portion of your data (often an 80% sample) and then 
# calculating the error using the hold-out sample. Again, I illustrate using mtcars, this time with an 80% sample.
                            
train <- sample.int(nrow(mtcars), 26)
train

# Estimate the model, then predict with the hold-out data:
fit <- lm(mpg~hp, data=mtcars[train, ])
pred <- predict(fit, newdata=mtcars[-train, ])
pred

# Combine the original data and prediction in a data frame
test <- data.frame(actual=mtcars$mpg[-train], pred)
test$error <- with(test, pred-actual)
test
                                                                 
# Now compute your test statistics in the normal way. I illustrate MSE and RMSE:
test.mse <- with(test, mean(error^2))
test.mse
test.rmse <- sqrt(test.mse)
test.rmse