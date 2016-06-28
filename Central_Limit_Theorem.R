# CLT Practice

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


##### USING HISTOGRAMS (Actual Samples) #####
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
slines(x=x, y=200*dnorm(x, mean=n_mean , sd=n_sd ))
#####

##### USING EXACT DISTRIBUTION #####
# Exact distribution of ybar from gamma yi's
# Can be found by mgf method. It's ga(n*alpha, n*lambda)
exact<-function(y,n,alpha,lambda){
  ((lambda^(alpha*n))/gamma(n*alpha))*(n*y)^(alpha*n-1)*exp(-lambda*n*y)*n
}

# Different values of sample size to compare
n<-c(1,2,5,10,30,50)
x<-seq(from=0,to=5,by=0.01)
# Make r "ask" to move to next plot
par(ask=TRUE)
for(i in 1:length(n)){
  
  # Exact distribution of ybar (ga(n*2, n*1)) 
  plot(x,exact(x,n=n[i],alpha=2,lambda=1),col="green",type="l",main=paste(
    "Approximate vs Exact Distr. of Y-bar from gamma(2,1), n=",n[i]),ylab="pdf")
  # Approximate distribution (N(2,2/n))
  lines(x,dnorm(x,mean=2,sd=sqrt(2/n[i])),type="l",col="blue")
  legend(x="topright",col=c("green","blue"),legend=c("Exact Distribution",
                                                   "Approximate Distribution"),pch=10)
}
#####

      