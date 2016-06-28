##### Lecture 4

### Is sample mean a good estimator of population mean in finite population?

# Bias(Xbar) 
# = E(Xbar - mu)
# = E(Xbar) - mu
# = mu - mu
# = 0

# So sample mean is unbiased. What about variance?
# It's different than in a nonfinite population because we have to consider the covariance
# We also must consider the marginal probabilities of RV's, which depend on sampling method
# When SRS, P(X=zeta) = n_zeta / N.

# Var(Xbar) (in a nonfinite population)
# = (sigma2 / n) * ((N-n) / (n-1))
# -> 0 as n->N


# So the sample mean variance approaces 0 as n->N. Thus sample mean is a good estimator.
grades <- rnorm(15000,75,15)

sample2mean <- rep(NA, 2)
sample5mean <- rep(NA, 5)
sample100mean <- rep(NA, 100)
sample1000mean <- rep(NA, 1000)
# min() to max out at 100
for(i in 1:10000){
  sample2mean[i] <- min(100,mean(sample(x = grades,size = 2,replace = F)))
  sample5mean[i] <- min(100,mean(sample(x = grades,size = 5,replace = F)))
  sample100mean[i] <- min(100,mean(sample(x = grades,size = 100,replace = F)))
  sample1000mean[i] <- min(100,mean(sample(x = grades,size = 1000,replace = F)))
}

# Plot 
# Notice that all are nonbiased and have increasingly low variances
par(mfrow=c(2,2))
hist(sample2mean, xlim = c(0,100))
hist(sample5mean, xlim = c(0,100))
hist(sample100mean, xlim = c(0,100))
hist(sample1000mean, xlim = c(0,100))

# Variances
# Notice that the finite population correction is not significant with small ample size(2)
# But with larger sample size(1000), it is significant (~20% better than without correction)
sample2.meanvar <- var(sample2mean)
sample2.finite.estimate <- (15**2)/2 * (15000 - 2) / (15000 - 1)
sample2.infinite.estimate <- (15**2)/2

sample1000.meanvar <- var(sample100mean)
sample1000.finite.estimate <- (15**2)/100 * (15000 - 100) / (15000 - 1)
sample1000.infinite.estimate <- (15**2)/100

                      