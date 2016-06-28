# We can show the distribution of the biased variance.
# We will see that the center of the distribution is not the expected variance, 1
# Thus the 1/n version of variance is biased.

set.seed(121)

n<-10
N<-10000

meanx<-rep(NA,N)
bvarx<-rep(NA,N)
for (i in 1:N){
  x<-rnorm(n,0,1)
  meanx[i]<-mean(x)
  bvarx[i]<-(1/n) * sum((x - mean(x))^2)
}
par(mfrow=c(1,2))

#Plot normal sample mean values and sample variance values

#mean
hist(meanx,main=paste("Histogram of Normal(0,1) Sample Means\n"," of size n=",n,"N=",N,"total samples"))
text(x=0.75,y=2000,paste("True Mean is 0\n Observed mean of sample\n means is ",round(mean(meanx),3)))

#variance
hist(bvarx,main=paste("Histogram of Normals(0,1) (Biased) Sample Variances\n"," of size n=",n,"N=",N,"total samples"))
text(x=2,y=1500,paste("True Variance is 1\n Observed mean of sample \n variances is ",round(mean(bvarx),4)))

