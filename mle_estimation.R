#load package to have interactive 3-d plot
library(rgl)

### Example) Given Y=2, what is the maximum likelihood estimator of lambda for Y ~ exp(lambda)
### (Note this is for sample size = 1. Usually we will have more and use the joint distribution)

#first static plot of log likelihood and likelihood to see max is same point

lambda<-seq(from=0.1,to=3,by=0.001)
#create plot of L and l
par(mfrow=c(1,2))

# Function for pdf of exponential (function of lambda, given Y=2)
plot(lambda,y=lambda*exp(-2*lambda),type="l",ylab="L(lambda)",main="Likelihood from an exponential given y=2")
plot(lambda,y=log(lambda*exp(-2*lambda)),type="l",ylab="l(lambda)",main="Log-Likelihood from an exponential given y=2")

#Now 3d plot of both y and L (exponential probability function with lambda and y unknown) at same time
#plotting sequences
lambda<-seq(from=0.1,to=3,by=0.1)
y<-seq(from=0.01,to=2.5,by=0.001)

#grid of points to plot Likelihood or density over
gridy<-rep(y,length(lambda))
gridlambda<-rep(lambda,each=length(y))

#create density function
fylambda<-function(y,lambda){lambda*exp(-lambda*y)}

#coloring rainbow for the height of the curve
cutseq<-seq(from=0,to=1.7,by=0.01)
cols<-rainbow(length(cutseq)+1)
groups<-cut(fylambda(y=gridy,lambda=gridlambda),c(-Inf,cutseq,Inf),labels=1:(length(cutseq)+1))

#Plot in 3d (This is if we only have one y (joint is just the exp pdf))
plot3d(y=gridy,x=gridlambda,z=fylambda(y=gridy,lambda=gridlambda),zlim=c(0,1.7),col=cols[groups])

#If we observe y=2, then lambdahat MLE = 1/2.  The curve is highest there.  
#Also, the probability of getting a value very near to y=2 is highest for this curve.
#P(2-epsilon < Y < 2+epsilon) is highest when lambda=1/2.

#If we observe y=0.5, then lambdahat MLE = 2.  The curve is highest there.  
#Also, the probability of getting a value very near to y=0.5 is highest for this curve.
#P(0.5-epsilon < Y < 0.5+epsilon) is highest when lambda=2.



# MLE estimator for lambda in pois (using plotting)
pois.pdf <- function(x,lambda){
  return((exp(-lambda)*(lambda^x))/(factorial(x)))
}

# Plot pdf
x <- seq(0,10,.01)
plot(x,pois.pdf(x=x,lambda=3), lwd=1, type="l")

# Plot likelihood 
plot(x,pois.pdf(x=3,lambda=x), lwd=1, type="l")

# Plot loglikelihood
plot(x,log(pois.pdf(x=3,lambda=x)), lwd=1, type="l")



# MLE estimator for p in binom (using plotting)
binom.pdf <- function(n,k,p){
  return(choose(n,k) * (p^k) * (1-p)^(n-k))
}

# Plot pdf
x <- seq(0,10,1)
plot(x=x, y=binom.pdf(n = 10, k = x, p = .75), type = "h")

# Plot Likelihood (max at .3)
x <- seq(0,1,.01)
plot(x=x, y=binom.pdf(n = 10, k = 3, p = x), type = "h")

# Plot Loglikelihood (max at .3)
x <- seq(0,1,.01)
plot(x=x, y=log(binom.pdf(n = 10, k = 3, p = x)), type = "h")

