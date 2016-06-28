##### Lecture 6

### Let Y ~ exp(lambda)
lambda <- 1/5

# How can we estimate lambda (E(X) = 1/lambda)?
# Method of Moments, because E(X) is a moment.
# LLN says that sample moment -> true moment
# That is, 1/n * sum(Y) -> E(X)
#          Ybar -> 1/lambda
#          1/Ybar -> lambda
# Thus we can use 1/Ybar to estimate lambda.

# Let's look at how it fairs in multiple sample sizes.

Y <- rexp(n = 10, rate = 1/lambda)
lambda.est.mom10 <- (1/10) * sum(Y)
paste("True lambda: ",lambda," n=10 MOM Estimate: ", lambda.est.mom10)

Y <- rexp(n = 100, rate = 1/lambda)
lambda.est.mom100 <- (1/100) * sum(Y)
paste("True lambda: ",lambda," n=100 MOM Estimate: ", lambda.est.mom100)

Y <- rexp(n = 1000, rate = 1/lambda)
lambda.est.mom1000 <- (1/1000) * sum(Y)
paste("True lambda: ",lambda," n=1000 MOM Estimate: ", lambda.est.mom1000)


# Another example

# Let Y with E(Y) = mu and Var(Y) = sigma2
# What are the MOM estimates of mu and sigma2?
# First true moment: E(X) = mu
# Second true moment: E(X^2) = sigma2 + mu^2

# Equating the samples and the trues:
# 1/n * sum(Y) -> mu
# 1/n * sum(Y^2) -> sigma2 + mu^2

# Solving for the parameters:
# mu.est = 1/n * sum(Y)
# sigma2.est = 1/n * sum(Y^2) - Ybar^2
#            = 1/n * sum((Y - Ybar)^2)


# Let's look at how they fairs in multiple sample sizes.

mu <- 70;
sigma2 <- 15^2;

# Doesn't have to be normal, just easier to input
Y <- rnorm(n = 10, mean = mu, sd = sqrt(sigma2))
mu.est.mom10 <- (1/10) * sum(Y)
sigma2.est.mom10 <- (1/10) * sum((Y - mean(Y))^2)
paste("True mu: ",mu," n=10 MOM Estimate: ", mu.est.mom10)
paste("True sigma2: ",sigma2," n=10 MOM Estimate: ", sigma2.est.mom10)

Y <- rnorm(n = 100, mean = mu, sd = sqrt(sigma2))
mu.est.mom100 <- (1/100) * sum(Y)
sigma2.est.mom100 <- (1/100) * sum((Y - mean(Y))^2)
paste("True mu: ",mu," n=100 MOM Estimate: ", mu.est.mom100)
paste("True sigma2: ",sigma2," n=100 MOM Estimate: ", sigma2.est.mom100)

Y <- rnorm(n = 1000, mean = mu, sd = sqrt(sigma2))
mu.est.mom1000 <- (1/1000) * sum(Y)
sigma2.est.mom1000 <- (1/1000) * sum((Y - mean(Y))^2)
paste("True mu: ",mu," n=1000 MOM Estimate: ", mu.est.mom1000)
paste("True sigma2: ",sigma2," n=1000 MOM Estimate: ", sigma2.est.mom1000)