# Ridge Regression
# OLS just minimizes RSS
# Ridge regression minimizes RSS + lambda * sum(coef^2)
# Ridge improves on ols because it decreases variance in the model with shrinkage
# and thus can make a better fit when we have a large p.

# Let's compare ridge regression and plain old least squares to fit "score" using pscl.admit (phd admitance data)
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

# Let's use test set validation to find the best lambda, then compare that ridge regression fit 
# to the slr's fit's test mse.
mse <- rep(NA,length(lambda.grid))
for(i in 1:length(mse)){
  fit.r <- glmnet(x[idx,],y[idx],lambda = lambda.grid[i],alpha=0)
  pred <- predict(fit.r, newx=x[-idx,])
  mse[i] <- mean((pred - test$score)^2)
}
lambda.grid[which.min(mse)]
plot(lambda.grid,mse,type="l")
points(lambda.grid[which.min(mse)],min(mse),col="red",pch=19)

# lambda = .55 is gives the best mse for us with an mse of 0.55
# which is better than the slr mse, .92.
# Great improvement when the resonse is 1 to 5 That's a half a score of improvement.

