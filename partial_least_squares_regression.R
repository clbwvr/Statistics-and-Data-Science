# Partial Least Squares 

# PLS is a supervised alternative to PCR
# Like PCR, PLS identifies M linear combinations of X's named Z1,...,ZM 
# to use is least squares regression. PCR sets these as the 1,...,M principal components,
# but PLS attempts to find directions that help explain both the response and the predictors.

# After standardizing the p predictors, PLS computes the first direction Z1 by setting
# the jth coefficient in the linear combination equal to the coefficient from the simple 
# linear regression of Y onto Xj. 

# To identify the second PLS direction we first adjust each of the variables
# for Z1, by regressing each variable on Z1 and taking residuals. These residuals
# can be interpreted as the remaining information that has not been
# explained by the first PLS direction. We then compute Z2 using this orthogonalized
# data in exactly the same fashion as Z1 was computed based
# on the original data. This iterative approach can be repeated M times to
# identify multiple PLS components Z1, . . ., ZM.

# Simulate data
set.seed(123)
y <- 1:200 + 40*rnorm(200)
x1 <- 1:200 + 20*rnorm(200)
x2 <- 1:200 + 20*rnorm(200)
x3 <- 1:200 + 20*rnorm(200)
x4 <- 1:200 + 20*rnorm(200)

# MLR 
fit.mlr <- lm(y~x1+x2+x3+x4)
summary(fit.mlr)

# PLS 
x1 <- x1 - mean(x1)
x2 <- x2 - mean(x2)
x3 <- x3 - mean(x3)
x4 <- x4 - mean(x4)
z1 <- x1 * coef(lm(y~x1+x2+x3+x4))[2] +
  x2 * coef(lm(y~x1+x2+x3+x4))[3] + 
  x3 * coef(lm(y~x1+x2+x3+x4))[4] +
  x4 * coef(lm(y~x1+x2+x3+x4))[5]
fit.pls1 <- lm(y~z1)
summary(fit.pls1)

# There's very little variation left after the first component, but 
# we'll do it anyway and get a worthless component (pvalue = 1 haha)
z2 <- x1 * coef(lm(fit.pls1$residuals ~ x1+x2+x3+x4))[2] +
  x2 * coef(lm(fit.pls1$residuals ~ x1+x2+x3+x4))[3] +
  x3 * coef(lm(fit.pls1$residuals ~ x1+x2+x3+x4))[4] +
  x4 * coef(lm(fit.pls1$residuals ~ x1+x2+x3+x4))[5]
fit.pls2 <- lm(y~z1+z2)
summary(fit.pls2)

# The pls does better than the MLR in terms of RSS and MSE