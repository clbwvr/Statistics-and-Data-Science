set.seed(123)
p <- .2
n <- 30

# +1 because of how R defines geometric
data <- rgeom(n,p) + 1

p.hat.mle <- 1 / mean(data)
p.hat.mle.se <- sqrt(p.hat.mle**2 * (1-p.hat.mle) / n)

# Nonparametric bootstrap
B <- 5000
p.hat.mle.bs <- rep(NA,B)
p.hat.mle.bs.tstats <- rep(NA,B)
for(i in 1:B){
  tempdata <- sample(x = data,size = n, replace=T)
  p.hat.mle.bs[i] <- 1 / mean(tempdata)
  p.hat.mle.bs.se <- sqrt((p.hat.mle.bs[i]**2 * (1-p.hat.mle)) / n)
  p.hat.mle.bs.tstats[i] <- (p.hat.mle.bs[i] - p.hat.mle) / p.hat.mle.bs.se
}

# Percentile CI's
quantile(p.hat.mle.bs, c(.025, .975))

# Now look at the distribution of the p hat mle (from true distribution)
p.hat.mle.true <- rep(NA,5000)
for(i in 1:5000){
  p.hat.mle.true[i] <- 1 / mean(rgeom(n,p) + 1)
}

# Compare distribution of nonparametric bootstrap and true mle distribution
par(mfrow=c(1,2))
hist(p.hat.mle.bs)
abline(v=quantile(p.hat.mle.bs, c(.025, .975)), col="orange", lwd=3)
abline(v=quantile(p.hat.mle.bs, .5), col="green", lwd=3)
hist(p.hat.mle.true)
abline(v=quantile(p.hat.mle.true, c(.025, .975)), col="orange", lwd=3)

# Bootstrapping is good for estimating spread and shape. Not center.
# Because the bootstrap distribution will always be centered at the mle.
abline(v=quantile(p.hat.mle.true, .5), col="green", lwd=3)

# Bootstrap Relected Percentile Method
reflected.distribution <- p.hat.mle.bs - p.hat.mle
delta <- quantile(reflected.distribution, c(.975,.025))
reflected.ci <- p.hat.mle - delta
reflected.ci

# Bootstrap t Interval Method
bootse <- sd(p.hat.mle.bs)
delta <- quantile(p.hat.mle.bs.tstats, c(.975,.025)) * bootse
t.interval.ci <- p.hat.mle - delta
t.interval.ci


mean(rexp(1000,3))
