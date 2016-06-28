### Lecture 2

# In this module, I will consider four properties of an estimator, bias, variance: mse, and consistency.

# Data for Creating Estimators
shape <- 4
scale <- 1
samplesize <- 10
samplenum <- 1000
population <- rgamma(2000, scale=scale, shape=shape)
mu <- mean(population)
sigma2 <- var(population)
hist(population, probability = T)
samples <- matrix(replicate(samplenum, sample(population, samplesize)), nrow=samplenum, ncol=samplesize)

# Create Sample Mean
ybar <- apply(samples,1,mean)


### Bias of Estimator = E(estimator - true) = E(estimator) - true
# Should be pretty low, because smaple mean is unbiased for true mean
ybar.bias <- mean(ybar) - mu 
ybar.bias

# Is  1/n * sum((y - ybar)^2)  a biased estimator for variance?
# Create biased sample variance
s2.b <- rep(NA,samplenum)
for (i in 1:samplenum){
  #find and save variance for each sample
  s2.b[i] <-(1/(samplesize))*sum((samples[i,]-mean(samples[i,]))^2)
}

# Create nonbiased sample variance
s2 <- rep(NA,samplenum)
for (i in 1:samplenum){
  #find and save variance for each sample
  s2[i] <-(1/(samplesize-1))*sum((samples[i,]-mean(samples[i,]))^2)
}

### Bias of estimators = E(estimator - true) = E(estimator) - true
s2.b.bias <- mean(s2.b) - sigma2
s2.bias <- mean(s2) - sigma2
paste("The biased variance estimate with 1/n,",s2.b.bias, ", in this case, will almost always be more biased than nonbiased (with 1/n-1)", s2.bias, "in this case.")


### Variance of Estimates = E((estimate - mean(estimate)^2) = E(estimate^2) - E(estimate)^2
ybar.variance <- mean(ybar^2) - mean(ybar)^2
ybar.sd <- sqrt(ybar.variance)
s2.variance <- mean(s2^2) - mean(s2)^2
s2.sd <- sqrt(s2.variance)


### Mean Squared Error MSE = E((estimate - true)^2) = Var(estimate) + Bias(estimate)^2
ybar.mse <- mean((ybar-mu)^2)
ybar.mse <- ybar.variance + ybar.bias^2 #same result
s2.mse <- mean((s2 - sigma2)^2)
s2.mse <- s2.variance + s2.bias^2 #same result


### Consistency of Estimate
# Estimate is consistent if estimate -p> mu as n -> inf
# If MSE(estimate) -> 0 as n -> inf, then estimate is consistent for true (estimate -p> true)
# We can show ybar is consistent for mu
#      Bias(ybar) = E(ybar) - mu = mu - mu = 0, so
#      MSE(ybar) = var(ybar) + bias(ybar)^2
#                = sigma2 / n
#                -> 0 as n -> inf
# Thus ybar is consitent for 

