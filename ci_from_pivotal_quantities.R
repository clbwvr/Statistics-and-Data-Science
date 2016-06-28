require(ggplot2)

#how to plot at end

# The simplest pivot is the normal one
# Since the n sample mean has distribution N(mu,sigma2/n),
# The z score of the mean, xbar - mu / (sigma/n) ~ N(0,1).
# Notice this is a pivot because the distribution doesn't depend on the parameters.
# But, of course, we don't know population sd, so we can use sample sd and the distribution will be t_n-1.
# xbar - mu / (s / n) ~ t_n-1
# so (1-alpha)% CI for mu is xbar+-(s/n)*(t_n-1,1-alpha/2)

# True for any distribution asymptotically, but exactly for normal distribition. 
mu <- 75
sigma <- 15
n <- 200

# Do repeatedly
x <- seq(1:100)
upper <- lower <- rep(NA,100)
for(i in 1:100){
  samp <- rnorm(n,mu,sigma)
  xbar <- mean(samp)
  s <- sd(samp)
  alpha <- (100-95)/100
  z <- c(qnorm(1-alpha/2))
  xbar.se <- s/sqrt(n)
  lower[i] <- xbar-xbar.se*z
  upper[i] <- xbar+xbar.se*z
}

# What if f(y) = theta * y^(theta-1)?
# Then we can find thetahat_mle = -n/sum(ln(y))
# We know that -sum(ln(y)) ~ ga(n,theta)
# Thus theta * (-sum(ln(y))) ~ ga(n,1) (properties of gamma)
# So theta*n / thetahat_mle ~ ga(n,1) is a pivot
#     because theta*n / thetahat_mle = theta * -sum(ln(y))
# Let g1 = g(n,1)_1-alpha/2 and g2 = g(n,1)_alpha/2
# Then P(g1 < n*theta/thetahat_mle < g2) = 1-alpha
# Algebra...
# P((thetahat_mle / n)*g1 < theta < (thetahat_mle / n)*g2) = 1-alpha
n <- 200
theta <- 3

possibles <-seq(0,1,length.out = 1000)
pmf <- theta*(possibles)^(theta-1)
g1 <- qgamma(.025, shape = n, rate = 1)
g2 <- qgamma((1-alpha)/2, shape = n, rate = 1)

upper <- lower <- rep(NA,100)

for(i in 1:100){
  y <- sample(possibles, n, prob = pmf,replace = T)
  thetahat_mle <- -n/(sum(log(y)))
  upper[i] <- (thetahat_mle / n)*g1
  lower[i] <- (thetahat_mle / n)*g2
}
