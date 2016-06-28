# Lecture 10

# Properties of mles:
# estimator is consistent for it's parameter

# g(mle) is an estimator for g(parameter)
# ex) 1/ybar is the mle estimator for lambda in exp(lambda)
lambda <- 3
y <- rexp(200,lambda)
ybar <- mean(y)
lambda.mle <- 1/ybar
# we can show that g(mle) estimates g(parameter)
# let g(x) = x^2
lambda.g <- lambda^2
lambda.mle.g <- (1/ybar)^2

# most importantly, mle's are asymptotically normally distributed
# specifically theta.mle ~ N(theta, 1/(n*I(theta)))
# where I(theta) is the density information
# n*I(theta) = I_n(theta) and I_n(theta) is easier to calculate, so we will usually use it
# so theta.mle ~ N(theta, 1/(I_n(theta)))
# calculate I_n(theta) as -E((d2/dtheta2) * loglikelihood(theta))
N <- 10000
n <- 200
lambda.mle <- rep(NA,N)
for(i in 1:N){
  y <- rexp(n,lambda)
  ybar <- mean(y)
  lambda.mle[i] <- 1/ybar
}
I.n.lambda <- n/(lambda^2)
hist(lambda.mle, probability = T)
x <- seq(2,4,.01)
lines(x=x, y=dnorm(x,lambda, sqrt(1/I.n.lambda)))

# Let's make a confidence interval for lambda
# We know sqrt(I_n(lambda)) * (lambda.mle - lambda) -> N(0,1)
# Then manipulating the pivotal quantity, we can get
# lambda.mle +- z_a/2 * 1/sqrt(I_n(lambda))
lambda.mle <- 1/ybar
zcritval <- qnorm(.975, 0, 1)
ci <- c(lambda.mle - zcritval * 1/sqrt(I.n.lambda)
        , lambda.mle + zcritval * 1/sqrt(I.n.lambda))
# Awesome! We can analytically make a CI!
