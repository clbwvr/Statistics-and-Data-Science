# What is standard error?
#
# The standard error of the mean (SE of the mean) estimates the variability between sample means 
# that you would obtain if you took multiple samples from the same population. The standard error 
# of the mean estimates the variability between samples whereas the standard deviation measures the
# variability within a single sample.
# 
# For example, you have a mean delivery time of 3.80 days with a standard deviation of 1.43 days 
# based on a random sample of 312 delivery times. These numbers yield a standard error of the mean 
# of 0.08 days (1.43 divided by the square root of 312). Had you taken multiple random samples of the
# same size and from the same population the standard deviation of those different sample means would
# be around 0.08 days.
# 
# Use the standard error of the mean to determine how precisely the mean of the sample 
# estimates the population mean. Lower values of the standard error of the mean indicate
# more precise estimates of the population mean. Usually, a larger standard deviation will
# result in a larger standard error of the mean and a less precise estimate. 
# A larger sample size will result in a smaller standard error of the mean and a more precise estimate.

x <- c(9,15)
y <- c(10.9,11.9,12.2,12.2,12.9,12.6,12.3,12.3,12.5,10.2)

mean(x)
mean(y)

# Their means are the same!
# But y is more precise... how can we quantify this?

# Standard Error is s/sqrt(n)
x.n <- length(x)
y.n <- length(y)

x.sd <- sd(x)
y.sd <- sd(y)

x.se <- x.sd / sqrt(x.n)
x.se
y.se <- y.sd/ sqrt(y.n)
y.se

# Notice that if n is huge (sample size approaches population size),
# then the standard error will be nearly 0.
# This means that your sample will be very close to the population mean.


# weight of male ncsu students 
pop.weight <- rnorm(20000, 180, 25)
hist(pop.weight,breaks=100)

sample20.weight <- sample(pop.weight, size = 20, replace = F)
sample20.weight.se <- sd(sample20.weight) / sqrt(20)
sample20.weight.se

sample2000.weight <- sample(pop.weight, size = 2000, replace = F)
sample2000.weight.se <- sd(sample2000.weight) / sqrt(2000)
sample2000.weight.se

sample20000.weight <- sample(pop.weight, size = 20000, replace = F)
sample20000.weight.se <- sd(sample20000.weight) / sqrt(20000)
sample20000.weight.se

# Let's show sample2000.weight.se ~= the standard deviation of the means of 10000 simulations of n=2000

mean2000 <- replicate(10000,mean(sample(pop.weight, size = 2000, replace = F)))

# Obviously the hist of the mean will be more tightly distributed 
hist(mean2000)
hist(sample2000.weight)

sd(mean2000)
sample2000.weight.se

# They're approx the same!