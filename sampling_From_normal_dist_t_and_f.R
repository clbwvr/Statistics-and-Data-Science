### Important Results when Sampling from a Normal Distribution

### If Z ~ N(0,1), then Z^2 ~ chisq(1)
z <- rnorm(10000,0,1)
hist(z)
z2 <- z^2
hist(z2)
x <- seq(0,10,by=.001)
# Scaled because histogram is frequency, pdf is probability
lines(x=x, y=10000*dchisq(x,1))


### If Yi ~ind~ chisq(p[i]) then sum(Yi) ~ chisq(sum(p))
y1 <- rchisq(10000,1)
y2 <- rchisq(10000,2)
y3 <- rchisq(10000,3)
y <- y1+y2+y3
x <- seq(0,20,by=.001)
hist(y)
# Scaled because histogram is frequency, pmf is probability
lines(x=x, y=20000*dchisq(x,1+2+3))

### T distribution ( t = z / (sqrt(w/v)) where z ~ (0,1) and w ~ chisq(v) and t ind w)
# t looks like normal, but longer tails
z <- rnorm(10000,0,1)
v <- 20
w <- rchisq(10000, v)
t <- z / (sqrt(w/v))
hist(t)
x <- seq(-5,5,by=.001)
# Scaled because histogram is frequency, pmf is probability
lines(x=x, y=5000*dt(x,v))
      
### F distribution
v1 <- 10
v2 <- 20
w1 <- rchisq(10000,v1)
w2 <- rchisq(10000,v2)
f <- (w1/v1)/(w2/v2)
hist(f)
x <- seq(0,5,by=.001)
# Scaled because histogram is frequency, pmf is probability
lines(x=x, y=5000*df(x,v1,v2))

