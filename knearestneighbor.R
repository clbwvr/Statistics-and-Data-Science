install.packages("class")
library(ggplot2)
library(class)
n<-500
gender <- c(rep("M",n/2), rep("F",n/2))
height <- c(rnorm(n/2,71,5),rnorm(n/2,64,4))
weight <- c(rnorm(n/2,180,30),rnorm(n/2,130,20))

data <- data.frame(gender,height,weight)

randidx <- sample(0:n,n,replace=F)
train <- data[randidx[0:(.8*n)],]
test <- data[randidx[(.8*n):n],]

# K nearest neighbor. Look at the k closest points and get a majority vote from them.
model<-knn(train = train[-1] , test = test[-1], cl = train$gender, k = 5)


par(mfrow=c(1,2))

plot(test$height,test$weight,col=ifelse(test$gender=="M","green","orange"),pch=20)
title("Data")
plot(test$height,test$weight,col=ifelse(model==test$gender,"black","red"), pch=20)
title(paste("Model:",round(mean(model==test$gender),2), "Accuracy"))

mean(model==test$gender)