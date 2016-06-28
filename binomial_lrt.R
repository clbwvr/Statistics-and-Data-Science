# Binomial Large Sample LRT
# Notice that our power gets better and better the farther from .5 we get.
# It's equal to significance level at .5 and very high at 0 and 100
# Power gets higher and higher with higher n. At n = very large, power is very large.
# I found the lrt through dividing the likelihoods at null and mle and algebra
siglevel<-.05
neg2ln <- rep(NA,10000)
n<-20
p.alt<-.75
for(i in 1:10000){
  y <- rbinom(n = 1, size=n, p=p.alt)
  neg2ln[i] <- -2 * log((.5^n) / (((y/n)^y) * ((1-y/n)^(n-y))))
}
power <- mean(neg2ln > qchisq(1-siglevel,1))
power



# Generate data under null assumption (the model)
# 10000 samples of multinomial(1000,1/10)
data <- rmultinom(n=10000,size=1000,prob = rep(1/10,10))
neg2lrt <- rep(NA,10000)
for(i in 1:10000){
  neg2lrt[i] <- 2 * sum(data[,i] * log(data[,i] / (1000/10)))
}
cutoff <- quantile(neg2lrt, .95)
#thus if we observe an neg2lrt greater than our cutoff, we'd reject the null (model is no good)

# If we have a large sample, then we would know the distribution of -2ln(lrt) ~ chisq(m-1-k)
neg2lrt <- rep(NA,10000)
for(i in 1:10000){
  neg2lrt[i] <- 2 * sum(data[,i] * log(data[,i] / (1000/10)))
}
hist(neg2lrt,probability = T,breaks=100)
lines(x=seq(0,30,.01),y=dchisq(seq(0,30,.01),df =9),col="red")
cutoff <- qchisq(p = .95, df=9)
