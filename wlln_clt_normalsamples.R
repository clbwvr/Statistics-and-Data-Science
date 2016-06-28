### Week 1 ###

### Weak Law of Large Numbers

# ybar converges in probability to mu of any parent distribution
# normal example

mu <- 70;
sd <- 15;
set.seed(1234);

y5 <- rnorm(5,mu,sd)
y20 <- rnorm(20,mu,sd)
y1000 <- rnorm(1000,mu,sd)

y5bar <- mean(y5)
y20bar <- mean(y20)
y1000bar <- mean(y1000)

y5se <- sd(y5) / sqrt(5)
y20se <- sd(y20) / sqrt(20)
y1000se <- sd(y20) / sqrt(1000)

paste("n=5 xbar =", y5bar, "+-", y5se)
paste("n=20 xbar =", y20bar, "+-", y20se)
paste("n=1000 xbar =", y1000bar, "+-", y1000se)

### Distribution of Functions of RV's
set.seed(1234);
samplenum <- 10000
samplesize <- 200
shape = 1
scale = 1

# Parent distribution ga(shape,scale)
y <- matrix(data=rgamma(n = samplenum*samplesize, shape = shape, scale = scale), nrow = samplenum, ncol = samplesize)
hist(y, probability = T, main = "Parent Distn (gamma(shape,scale))")
lines(x=seq(0,20,.01), dgamma(x=seq(0,20,.01), shape = shape, scale = scale), col="blue")

# Mean distribution ga(n*shape, n*scale)
ybar <- apply(y,1,mean)
hist(ybar, probability = T, main = "Distribution of Y bar (gamma(n*shape, n*scale))")
lines(x=seq(0,2,.01), y=dgamma(seq(0,2,.01), samplesize, samplesize), col="blue")

# Sum distribution ga(n*shape, scale)
ysum <- apply(y,1,sum)
hist(ysum, probability = T, main = "Distribution of sum of Ys (gamma(n*shape, scale))")
lines(x=seq(150,250,.01), y=dgamma(seq(150,250,.01), shape = samplesize, scale = scale), col="blue")



### Central Limit Theorem

# "Distribution of mean converges to N(mu_y, var_y / n)

# I'll use gamma(2,1)
# Parent Distribution (ga(2,1))
x <- seq(0,5,.01)
alpha <- 2
lambda <- 1
plot(x, y=dgamma(x=x,shape=alpha,rate=lambda),type="l", main="Parent Distn (ga(2,1))")

# Mean Distribution
# n - sample size
# N - number of samples


# USING HISTOGRAMS (Actual Samples)
# Not bad, but not great
n <- 2
N <- 1000
y.i <- replicate(N,rgamma(n,shape=alpha,rate=lambda))
y.bar <- colSums(y.i) / (n)
hist(y.bar)
n_mean = alpha/lambda
n_sd = sqrt((alpha/(lambda^2)) / n)
# Scaled becaues histogram y is frequency, normal line is a pdf
lines(x=x, y=500*dnorm(x, mean=n_mean , sd=n_sd ))

# Very good
n <- 30
N <- 1000
y.i <- replicate(N,rgamma(n,shape=alpha,rate=lambda))
y.bar <- colSums(y.i) / (n)
hist(y.bar)
n_mean = alpha/lambda
n_sd = sqrt((alpha/(lambda^2)) / n)
# Scaled becaues histogram y is frequency, pdf is probability
lines(x=x, y=200*dnorm(x, mean=n_mean , sd=n_sd ))


##### Sampling from normal distribution
mu <- 0
sigma <- 1
v1 <- 5
v2 <- 6
samplesize <- 2000
z <- rnorm(samplesize, mu, sigma)
chisq_1 <- z*z
w1 <- rchisq(samplesize, v1)
w2 <- rchisq(samplesize, v2)
t <- z / sqrt(w1/v1)
f <- (w1/v1) / (w2/v2)

# Standard Normal
x <- seq(-3,3,.01)
hist(z,breaks=50,probability = T, main="Standard Normal")
lines(x=x, y=dnorm(x=x, mean=0, sd=1), col="blue")

# Chisq by Squaring Standard Normal
x <- seq(0,10,.01)
hist(chisq_1, breaks=50, probability = T, main="Chi Squared by Squaring Standard Normal")
lines(x=x, y=dchisq(x=x, df=1),col="blue")

# T distribution by Z / sqrt(w_v/v)
x <- seq(-5,5,.01)
hist(t,breaks=50,probability = T, main="T distribution by Z/sqrt(w_v / v)")
lines(x=x, y=dt(x=x, df = v1), col="blue")

# F distribution by w1/v1 / w2/v2
x <- seq(0,20,.01)
hist(f, breaks=50, probability = T, main="F distribution by w1/v1 / w2/v2")
lines(x=x, y=df(x = x, df1 = v1, df2 = v2), col="blue")
