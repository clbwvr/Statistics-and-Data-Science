# Use bootstrap techniques to consider the standard error of a sample correlation coefficient

# Create data
x <- seq(0,1,length.out = 100)
# True values
a <- 5
b <- 10
# Normally distributed errors
y <- a + b*x + rnorm(length(x), 0, 2)

plot(x,y)

rhat <- cor(x,y)

B <- 1000
n <- 20
rhat.boot <- rep(NA,B)
for(i in 1:B){
  idx <- runif(n,0,100)
  rhat.boot[i] <- cor(x[idx],y[idx])
}
hist(rhat.boot,breaks=50)

rhat.se <- sd(rhat.boot)

