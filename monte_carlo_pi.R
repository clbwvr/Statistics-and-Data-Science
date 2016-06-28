# phat has asymptotic distribution N(p,phat*(1-phat)/n)
# So we can create a CI for our monte carlo method for calculating pi
monte.carlo.pi <- function(n=10000){
x <- runif(n,0,1)
y <- runif(n,0,1)
in.circle <- x^2 + y^2 < 1^2
pi.hat <- 4*mean(in.circle)
# plot(x,y, col=ifelse(x^2 + y^2 < 1^2,'blue','green'), type="p")
}
pi.hat.vec <- rep(NA,5000)
abs.error <- rep(NA,5000)
for (i in 1:5000){
  pi.hat.vec[i] <- monte.carlo.pi(i)
  abs.error[i] <- abs(pi.hat.vec[i] - pi)
}
par(mfrow=c(1,2))
plot(1:5000, pi.hat.vec,type = "l", col="black")
abline(h=pi, col="green",lwd="2")
plot(1:5000, ylim = c(0,.5), abs.error,type = "l", col="black")

