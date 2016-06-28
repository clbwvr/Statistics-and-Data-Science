# True proportion of people who like chocolate
phat <- .85

# We'll use an informative prior, we're pretty sure most people like chocolate.
prior <- function(p){
  return(dbeta(p,40,2))
}

# The likelihood of the data we get for proportion parameter is modeled by binomial
likelihood <- function(n,y,p){
  return(dbinom(size = n, x = y, prob = p))
}

# The posterior distribution is proportional to the product
posterior <- function(n,y,p){
  return(prior(p) * likelihood(n,y,p))
}

n<-100
y<-75
p<-seq(0,1,length.out = 100)

plot(p,prior(p),type="l",col="red")
plot(p,likelihood(n,y,p),type="l", col="black")
plot(p,posterior(n,y,p),type="l", col="purple")

# We can find the marginal that makes this distribution integrate to 1.
# It looks like about 0.1, just looking at the plot of 10*posterior



# C - Cancer; M - Mammogram
probs = matrix(c(11, 99, 3, 887), 2, 2, byrow = TRUE, dimnames = list(c("M-True", "M-False"), c("C-True", "C-False")))
print(probs)

# Marginal Probabilities
probs = cbind(probs, rowSums(probs))
probs = rbind(probs, colSums(probs))
colnames(probs)[3] = "M"
rownames(probs)[3] = "C"

library(ggplot2)
library(reshape2)
mdf = melt(probs[1:2, 1:2], varnames = c("Mammogram", "Cancer"))
mdf$Cancer = ifelse(mdf$Cancer == "C-True", "Yes", "zNo")
p = ggplot(mdf, aes(x = Cancer, y = Mammogram)) + geom_tile(aes(fill = value)) + 
  scale_fill_gradient(low = "black", high = "white")
p

# Reduce the false positives so that
# the positive predictive value is close to 80%
falsePositive = round(probs[1, 1]/0.8)
probs2 = probs
probs2[1, 2] = falsePositive
probs2[2, 2] = probs2[3, 2] - falsePositive
probs2[1, 3] = sum(probs2[1, 1:2])
probs2[2, 3] = sum(probs2[2, 1:2])
print(probs2)

mdf = melt(probs2[1:2, 1:2], varnames = c("Mammogram", "Cancer"))
mdf$Cancer = ifelse(mdf$Cancer == "C-True", "Yes", "zNo")
p = ggplot(mdf, aes(x = Cancer, y = Mammogram)) + geom_tile(aes(fill = value)) + 
  scale_fill_gradient(low = "black", high = "white")
p


# Increase likelihood of cancer
PPV = probs[1, 1]/probs[1, 3]
probs3 = probs
probs3[2, 1] = 250 - probs3[1, 1]
probs3[2, 2] = probs3[2, 3] - probs3[2, 1]
probs3[3, 1] = 250
probs3[3, 2] = 750

mdf = melt(probs3[1:2, 1:2], varnames = c("Mammogram", "Cancer"))
mdf$Cancer = ifelse(mdf$Cancer == "C-True", "Yes", "No")
p = ggplot(mdf, aes(x = Cancer, y = Mammogram)) + geom_tile(aes(fill = value)) + 
  scale_fill_gradient(low = "black", high = "white")
p
