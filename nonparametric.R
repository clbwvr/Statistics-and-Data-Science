require(ggplot2)

x <- runif(100,-5,5)
error <- rnorm(100,0,20)
y <- 100 + x^3 + error
g<-ggplot() + geom_point(aes(x,y))
g


simple_nonpar <- function(x,y,windownum){
  iter <- seq(min(x),max(x),length.out = windownum)
  # print(iter)
  a <- min(x)
  ybar <- iterbar <- rep(NA,length(iter)-1)
  for(i in 1:(length(iter)-1)){
    a <- iter[i]
    b <- iter[i+1]
    ybar[i] <- mean(y[which(x>=a & x<=b)])
    # No NaN's please!
    if(!is.finite(ybar[i])){
      if(i > 1) {ybar[i] <- ybar[i-1]}
      else ybar[i] <- mean(ybar,na.rm = T)
    }
    iterbar[i] <- (a+b)/2
  }
  return(data.frame(ybar,iterbar))
}

model<-simple_nonpar(x,y,50)
g + geom_line(aes(model$iterbar,model$ybar),col="blue")

