# GOF and PD of Poisson Fit

# Get data
lambda <- runif(n=1,min=.5,max=.7)
data <- rpois(n=200, lambda=lambda)
mle <- mean(data)

# GOF
sum <- 0
for(i in unique(data)){
  sum <- sum + sum(((sum(data==i) - 200*dpois(i,mle))^2) / 200*dpois(i,mle)) 
}
neg2lr <- 2*sum
df <- (length(unique(data))-1)-1
pvalue <- 1 - pchisq(sum,df)
# If high, then we'll keep the model

# PD 
neg2lr <- 2 * sum(subset(data,data != 0) * log(subset(data,data != 0)/mle)) 
df <- length(data)
pvalue <- 1 - pchisq(sum,df)
# If high, then we'll keep the model


# QQ Plots


# Good uniform match
data <- runif(1000)
sorted <- sort(data)
x <- 1:length(data)
theoretical.quantiles <- qunif((x-.5)/1000)
plot(theoretical.quantiles,sorted)

# Good normal match
data <- rnorm(100,85,15)
sorted <- sort(data)
x <- 1:length(data)
theoretical.quantiles <- qnorm(((x-.5)/100), 85, 15)
plot(theoretical.quantiles,sorted)

# Good Beta  match
data <- rbeta(100,2,3)
sorted <- sort(data)
x <- 1:length(data)
theoretical.quantiles <- qbeta(((x-.5)/100), 2, 3)
plot(theoretical.quantiles,sorted)

# Bad Beta Match
data <- runif(100)
sorted <- sort(data)
x <- 1:length(data)
theoretical.quantiles <- qbeta(((x-.5)/100), 8, 81)
plot(theoretical.quantiles,sorted)

# Bad Normal Match
data <- rnorm(100)
sorted <- sort(data)
x <- 1:length(data)
theoretical.quantiles <- qnorm(((x-.5)/100), 0, 1)
plot(theoretical.quantiles,sorted)
