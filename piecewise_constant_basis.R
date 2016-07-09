require(ggplot2)

x <- runif(50,0,10)
y <- x + rnorm(50)
b1 <- as.numeric(x < 2)
b2 <- as.numeric(x > 2 & x < 4)
b3 <- as.numeric(x > 4 & x < 6)
b4 <- as.numeric(x > 6 & x < 8)
b5 <- as.numeric(x > 8)

fit <- lm(y~b1+b2+b3+b4)
summary(fit)

fcn <- function(x){
  return(9.2245 - 
           ifelse(x<2,9.1677,
           ifelse(x<4,6.8830,
           ifelse(x<6,4.1958,
           ifelse(x<8,1.9956,
           0)))))
}
pred <- fcn(x)

g <- ggplot() + geom_point(aes(x,y)) + geom_line(aes(sort(x),sort(fcn(y))),col="blue")
g
plot(x,y,pch=20)
lines(x = sort(x), y = sort(fcn(x)), col="red")

