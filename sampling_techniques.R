### Lecture 3 ###

# Create population (Males in different age groups 's number of sexual partners)
N <- 1500
age <- c(rep('20s', N/3), rep('30s', N/3), rep('40s', N/3))
partners <- abs(c(round(rnorm(N/3,3,2)), round(rnorm(N/3,5,3)), round(rnorm(N/3,6,3))))
location <- rep(c('A','B','C','D','E'), N/5)
data <- data.frame(age,partners,location)
attach(data)

# pop mean, total, and variance
pop.mean <- (1/N) * sum(partners)
pop.total <- sum(data$partners)
pop.variance <- (1/N) * sum((partners - pop.mean)^2)
pop.sd <- sqrt(pop.variance)

# Simple Random Sample
n <- 300
sample.srs <- sample(x = data$partners, size = n, replace = F)

# Stratified Random Sample - Partitioned units by characteristics
sample.strat1 <- sample(x = (subset(x = data$partners, data$age == '20s')), size = n/3)
sample.strat2 <- sample(x = (subset(x = data$partners, data$age == '30s')), size = n/3)
sample.strat3 <- sample(x = (subset(x = data$partners, data$age == '40s')), size = n/3)
sample.strat <- c(sample.strat1, sample.strat2, sample.strat3)

# Cluster Sampling
locations <- unique(location)
samplelocations <- sample(locations, 3, replace = F)
sample.cluster1 <- sample(x=subset(x = data$partners, data$location == samplelocations[[1]]), size= n/3)
sample.cluster2 <- sample(x=subset(x = data$partners, data$location == samplelocations[[2]]), size= n/3)
sample.cluster3 <- sample(x=subset(x = data$partners, data$location == samplelocations[[3]]), size= n/3)

# Systematic Sampling
point <- 3
sectionsize <- N/n
sections <- N
indices <- seq(point, N, by=sectionsize)
sample.systematic <- data$partners[indices]


### SRS Notation
N
n
x <- data$partners
X <- sample.srs
zeta <- unique(X)
count <- rep(NA,length(zeta))
for(i in 1:length(zeta)){
  count[i] <- sum(zeta[i] == sample.srs)
}
zeta.pmf <- count/n
plot(x=zeta, pmf)

# Expected Value of X
# = mu
X.exp <- sum(zeta * zeta.pmf)
X.exp
mean(partners)

# Variance of X
# = sigma2
X.var <- sum((zeta^2) * zeta.pmf) - sum((zeta) * zeta.pmf)^2 # = E(X^2) - mu^2
X.var
var(partners)


par(mfrow=c(1,2))
hist(partners, main="Population")
hist(sample.srs, main="Sample")