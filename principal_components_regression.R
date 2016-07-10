# Principal Components Regression


# PCR involves constructing the first M principal components Z1, ..., ZM 
# and then using them as the predictors is a linear regression model using least squares.
# The idea is that often a small number of principal components suffice to explain most 
# of the variability in the data, as well as the relationship with the response. 
# In other words, we assume that the directions in which X1, . . .,Xp 
# show the most variation are the directions that are associated with Y. 

# If this assumption holds, then fitting a least squares model to Z1,...,ZM
# will lead to better results than fitting a least squares model to X1,...,Xp, 
# because most or all of the information in the data that relates to the response is
# contained in Z1, . . ., ZM, and by estimating only M < p coefficients instead of p,
# we can mitigate overfitting.

# The first principal component is the linear combination of x-variables 
# that has maximum variance (among all linear combinations), 
# so it accounts for as much variation in the data as possible.
# The second principal component  is the linear combination of x-variables
# that accounts for as much of the remaining variation as possible, 
# with the constraint that the correlation between the first and second component is 0


# Simulate data
set.seed(123)
y <- 1:200 + 40*rnorm(200)
x1 <- 1:200 + 20*rnorm(200)
x2 <- 1:200 + 20*rnorm(200)
x3 <- 1:200 + 20*rnorm(200)
x4 <- 1:200 + 20*rnorm(200)

data <- data.frame(y,x1,x2,x3,x4)
idx <- sample(1:200,150,replace=F)
train <- data[idx,]
test <- data[-idx,]

# MLR 
fit.mlr <- lm(y~x1+x2+x3+x4,data=train)
summary(fit.mlr)
preds.mlr <- predict(fit.slr,newdata=test)
mse.mlr <- mean((preds.mlr - test$y)^2)


# Principal Component Analysis
pc <- prcomp(data)
pc
# most of the variance is explained in the first component
# the components come from projecting the points on the the
# direction along which most of the variation in the data lies 
train.first.pc <- pc$x[idx,1]
test.first.pc <- pc$x[-idx,1]
fit.pc <- lm(train$y~train.first.pc)
preds.pc <- coef(fit.pc)[1] + (coef(fit.pc)[2]) *  test.first.pc
mse.pc <- mean((preds.pc - test$y)^2)  

barplot(c(mse.mlr, mse.pc), col = c("blue","red"), names.arg = c("mlr","pc"))
