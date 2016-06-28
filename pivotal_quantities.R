# Lecture 9 

# We can find that the mle estimates of mu and sigma2 from a normal distribution
# are ybar and Sb^2 respectively.
# We can form confidence intervals for the mu and sigma based on pivotal quantities we
# can get for those estimates.

# First, mu. If we knew sigma2, we could use (ybar-mu)/(sqrt(sigma2/n)) ~ N(0,1). But we don't, so this
# isn't a pivotal quantity (has an unknown other than the parameter).
# But we *can* use (ybar-mu)/(sqrt(s2/n)) ~ T_n-1.
# Solving for mu, we get ybar +- (t_n-1)(s/sqrt(n)) as our (1-alpha)100% CI

#Pretend we don't know these
mu <- 75
sigma2 <- 15^2
n <- 200
y <- rnorm(n,mu,sqrt(sigma2))

ybar <- mean(y)
t.alpha <- qt(p=0.975,df=n-1)
ci <- c(ybar - t.alpha * sqrt(sigma2/n), ybar + t.alpha * sqrt(sigma2/n))

# Second, sigma2. What pivotal quantity do we know with s2 and sigma2?
# We know that (n-1)*s^2 / sigma^2 ~ chisq(n-1)
# But our estimate for mle is s2b (the biased sample variance)
# We can change the numerator to n*sb2
# Then n * sb2 / sigma2 ~ chisq(n-1)
# Then we can say P(chisq_critval_left < n * sb2 / sigma2 < chisq_critval_right)
# Then P(sb2 * n / chisq_critval_right < sigma2 < sb2 * n / chisq_critval_left)
sb2 <- (1/n) * sum((y - ybar)^2)
chisq.critval.left <- qchisq(p = .025, df = n-1)
chisq.critval.right <- qchisq(p = .975, df = n-1)
ci <- c(sb2 * n / chisq.critval.right, sb2 * n / chisq.critval.left)
