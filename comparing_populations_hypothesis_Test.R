# treatment group
treat <- rbinom(1000, 1, .1)
placebo <- rbinom(1000, 1, .1)

ptreat <- mean(treat)
pplacebo <- mean(placebo)

ppooled <- mean(c(treat,placebo))
sepooled <- sqrt((ppooled * (1-ppooled)) * (1/1000 + 1/1000))

x <- seq(0,1,.0001)
plot(x=x, y=dnorm(x,ppooled,sepooled), type="l")

#normally would be (ptreat - pplacebo) - (trueptreat - truepplacebo)
#but h0 = trues are equal
teststat <- (ptreat - pplacebo) / (sepooled)
ptreat - pplacebo



#means 
salaryconsultants <- rnorm(1000,80,10)
salaryit <- rnorm(1000,75,10)

xbarconsultants <- mean(salaryconsultants)
xbarit <- mean(salaryit)
sconsultants <- sd(salaryconsultants)
sit <- sd(salaryit)

sepooled <- sqrt((sconsultants^2 / 1000) + (sit^2 / 100))
  
diffhat <- xbarconsultants - xbarit
diffh0 <- 0

teststat <- ((diffhat) - diffh0) / (sepooled)
1 - pnorm(teststat)



# small sample
# car repair costs
ford <- c(150,400,720,500,930)
nissan <- c(50,200,150,400,750,400,150)

xbar.ford <- mean(ford)
xbar.nissan <- mean(nissan)

s.ford <- sd(ford)
s.nissan <- sd(nissan)

n.ford <- length(ford)
n.nissan <- length(nissan)

# They have similar sd's so we can pool them. We have to do this for small sample t-dist hyp tests
s2.pool <- (((n.ford-1)*s.ford^2) + ((n.nissan-1)*s.nissan^2)) / (n.ford + n.nissan - 2)
s.pool <- sqrt(s2.pool)

# Now get standard error from the pooled variance over the n's
se.diff <- s.pool*(sqrt(1/n.nissan+1/n.ford))

t <- qt(p = .975, df = 10)
ci <- xbar.ford - xbar.nissan + c(t*se.diff, -t*se.diff)
x <- seq(0,1,.0001)
plot(x,dbeta(x,shape1 = 82, shape2 = 22),type="l")
