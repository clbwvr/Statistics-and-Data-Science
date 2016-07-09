# Lasso
# OLS just minimizes RSS and ridge regression minimizes RSS + lambda * sum(coef^2)
# Lasso minimizes RSS + lambda * sum(abs(coef))
# and thus can make a better fit when we have a large p. Lasso will make coefficients zero, thus
# dropping them from the model, while ridge won't. It will just shrink the coefficients.

# Let's compare lasso, ridge regression and plain old least squares to fit "score" using pscl.admit (phd admitance data)
set.seed(123)
install.packages("pscl")
library(MASS)
library(pscl)
data <- admit
data[,1] <- as.numeric(data[,1])

# Let's also add a bunch of junk variables to mess with our linear fit.
X <- matrix(runif(nrow(data)*30),ncol=30)
colnames(X) <- colnames(X,do.NULL=F,prefix="junk.")
data <- cbind(data,X)
idx <- sample(1:nrow(data), (2/3) * nrow(data),replace=F)
train <- data[idx,] 
test <- data[-idx,]

# simple linear fit
pairs(train)
fit.slr <- lm(score~.,data=train)
summary(fit.slr)
# we can see all those junk variables we all insignificant.
# But they're inflating the variance of our estimates
preds <- predict(fit.slr,newdata=test)
fit.slr.testmse <- mean((preds-test$score)^2)

# ridge regression
# we hope to shrink the junk variables effects towards zero.
lambda.grid <- seq(50,0,length.out = 1000)
library(glmnet)
x <- model.matrix(score~.,data=data)[,-1]
y <- data[,1]
fit.ridge <- glmnet(x[idx,],y[idx],lambda = lambda.grid,alpha=0)
plot(range(lambda.grid),range(fit.ridge$beta),type="n")
for(i in 1:nrow(fit.ridge$beta)){lines(lambda.grid,fit.ridge$beta[i,],col=i)}
# the coefficients at lambda = 0 are equal to the ones from the slr (after standardization)
# but as lambda grows, they shrink towards 0

# Let's use test set validation to find tphe best lambda, then compare that ridge regression fit 
# to the slr's fit's test mse.
mse <- rep(NA,length(lambda.grid))
for(i in 1:length(mse)){
  fit.r <- glmnet(x[idx,],y[idx],lambda = lambda.grid[i],alpha=0)
  pred <- predict(fit.r, newx=x[-idx,])
  mse[i] <- mean((pred - test$score)^2)
}
lambda.grid[which.min(mse)]
plot(lambda.grid,mse,type="l")
plot(range(lambda.grid),range(fit.ridge$beta),type="n")
points(lambda.grid[which.min(mse)],min(mse),col="red",pch=19)

# lambda = .55 is gives the best mse for us with an mse of 1.64


# lasso
# we hope to shrink the junk variables effects towards zero and drop some variables
lambda.grid <- seq(.5,0,length.out = 1000)
library(glmnet)
x <- model.matrix(score~.,data=data)[,-1]
y <- data[,1]
fit.lasso <- glmnet(x[idx,],y[idx],lambda = lambda.grid,alpha=1)
plot(range(lambda.grid),range(fit.lasso$beta),type="n")
for(i in 1:nrow(fit.lasso$beta)){lines(lambda.grid,fit.lasso$beta[i,],col=i)}
# the coefficients at lambda = 0 are equal to the ones from the slr (after standardization)
# but as lambda grows, they shrink towards 0

# Let's use test set validation to find tphe best lambda, then compare that ridge regression fit 
# to the slr's fit's test mse.
mse <- rep(NA,length(lambda.grid))
for(i in 1:length(mse)){
  fit.l <- glmnet(x[idx,],y[idx],lambda = lambda.grid[i],alpha=1)
  pred <- predict(fit.l, newx=x[-idx,])
  mse[i] <- mean((pred - test$score)^2)
}
lambda.grid[which.min(mse)]
# lambda = .07 gives the lowest mse of 1.17
plot(lambda.grid,mse,type="l")
points(lambda.grid[which.min(mse)],min(mse),col="red",pch=19)
fit.lasso.testmse <- min(mse)

fit.slr.testmse
fit.ridge.testmse
fit.lasso.testmse

# SLR has the worst mse, ridge has lower mse than lsr, lasso has the lower mse.
