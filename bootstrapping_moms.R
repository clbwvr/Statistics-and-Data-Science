##### Lecture 7

### MOM for gamma(alpha,lambda)
# Find estimates of alpha and lambda
# E(Y) = alpha/lambda
# E(Y^2) = Var(Y) + E(Y)^2 = alpha/(lambda^2) + ((alpha/lambda)^2
#                                                
# Equate the sample and true moments
# 1/n * sum(Y) = alpha/lambda
# 1/n * sum(Y^2) = alpha/(lambda^2) + ((alpha/lambda)^2
#                         
# Solve the system to get 
# lambda.hat = Ybar / s2_b (biased variance)
# alpha.hat = Ybar^2 / s2_b (biased variance)

lambda.hat.fcn <- function(Y){return(mean(Y) / ((1/length(Y)) * sum((Y - mean(Y))^2)))}
alpha.hat.fcn <- function(Y){return(mean(Y)^2 / ((1/length(Y)) * sum((Y - mean(Y))^2)))}

# Test estimates
g <- rgamma(10000,2,4)
lambda.hat.fcn(g)
alpha.hat.fcn(g)
# Not bad!

# We could SIMULATE an approximation of our estimators using the true parameters
nsims <- 10000
n <- 300
alpha <- 4
lambda <- 3
lambda.hat <- rep(NA,nsims)
alpha.hat <- rep(NA,nsims)
for (i in 1:nsims){
  g <- rgamma(4,shape = alpha, rate = lambda)
  lambda.hat[i] <- lambda.hat.fcn(g)
  alpha.hat[i] <- alpha.hat.fcn(g)
}

plot.new()
par(mfrow=c(1,2))

hist(lambda.hat,xlim = c(0,50), breaks=2000, probability = T, main = paste("Distribution of lambda. True:", lambda))
abline(v = lambda, col = "blue", lwd=3)
hist(alpha.hat,xlim = c(0,50), breaks=2000, probability = T,main = paste("Distribution of alpha. True:", alpha))
abline(v = alpha, col = "blue", lwd=3)

# Of course, we don't know the true parameters in real life.
# So how can we consider the estimators' distributions to make CI's and HT's?

# Bootstrap

# We can fit the model with an estimate made form a sample, then sample
# from a fitted model and consider the estimator distribution
# From our example (trying to estimate alpha and lambda)

# Suppose we came up with these estimates through mom or mle or something
alpha.hat <- 3.85
lambda.hat <- 2.98

# Then we sample from the fitted distribution and create a bunch of estimates
nsims <- 10000
n <- 200
alpha.hats <- lambda.hats <- rep(NA,nsims)
for (i in 1:nsims){
  fitted <- rgamma(n,alpha.hat,lambda.hat)
  alpha.hats[i] <- alpha.hat.fcn(fitted)
  lambda.hats[i] <- lambda.hat.fcn(fitted)
}

plot.new()
par(mfrow=c(1,2))

hist(lambda.hats, breaks=2000, probability = T, main = paste("Distribution of lambda hats. fitted:", lambda.hat))
abline(v = lambda, col = "blue", lwd=3)
hist(alpha.hats, breaks=2000, probability = T,main = paste("Distribution of alpha hats. fitted:", alpha.hat))
abline(v = alpha, col = "blue", lwd=3)

# What are the standard errors?
alpha.hat.se <- sqrt(var(alpha.hats))
lambda.hat.se <- sqrt(var(lambda.hats))

# We can also use bootstrap approximation to the sampling distributions of our estimators
# to create CI's. One estimate is called the Bootstrap Percentile Method".

# Just use emperical quantiles of the bootstrap distributions
alpha.hat.ci <- c(quantile(alpha.hats, .025), quantile(alpha.hats, .975))
lambda.hat.ci <- c(quantile(lambda.hats, .025), quantile(lambda.hats, .975))

# Plot the CI's
plot.new()
par(mfrow=c(1,2))

hist(lambda.hats, breaks=2000, probability = T, main = paste("Distribution of lambda hats. fitted:", lambda.hat))
abline(v = lambda.hat.ci, col = "blue", lwd=1)
hist(alpha.hats, breaks=2000, probability = T,main = paste("Distribution of alpha hats. fitted:", alpha.hat))
abline(v = alpha.hat.ci, col = "blue", lwd=1)
