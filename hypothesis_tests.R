# Z Test: Are the sugar amounts greater than 20g?
sugar <- rnorm(50,22,3)
xbar <- mean(sugar)
s <- sd(sugar)
n <- length(sugar)
se <- s/sqrt(n)
z <- (xbar - 20) / se
pvalue <- 1-pnorm(z)
z.test(z)

# T Test: Are the salmonella levels higher than .3?
levels <- c(0.593, 0.142, 0.329, 0.691, 0.231, 0.793, 0.519, 0.392, 0.418)
# More than .3?
s <- sd(levels)
xbar <- mean(levels)
n <- length(levels)
se <- s/sqrt(n)
t <- (xbar - .3) / se
pvalue <- 1-pt(t,df=n)
t.test(levels,alternative = "greater",mu=.3)

# Two sample t test (equal variance)
# Is there a difference between the two? (mux - muy = 0)
x <- rnorm(10,85,5)
y <- rnorm(10,95,5)
xbar <- mean(x)
ybar <- mean(y)
sx <- sd(x)
sy <- sd(y)
nx<-ny<-10
sp2 <- ((nx-1)*sx^2 + (ny-1)*sy^2) / ((nx-1) + (ny-1))
t <- ((xbar-ybar) - (0)) / (sqrt(sp2) * sqrt(1/nx + 1/ny))
pvalue <- 2*pt(q = t, df = nx+ny-2)
t.test(x,y,var.equal = T)

# Two sample t test (unequal variance)
# Is there a difference between the two? (mux - muy = 0)
x <- rnorm(10,85,3)
y <- rnorm(10,95,7)
xbar <- mean(x)
ybar <- mean(y)
sx <- sd(x)
sy <- sd(y)
nx<-ny<-10
t <- ((xbar-ybar) - (0)) / sqrt(sx^2/nx + sy^2/ny)
v <- (sx^2/nx + sy^2/ny)^2 / ((((sx^2/nx)^2)/(nx-1) + ((sy^2/ny)^2)/(ny-1))) 
v <- round(v)
pvalue <- 2*pt(q = t, df = v)
t.test(x,y,var.equal = F)
