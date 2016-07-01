plot(faithful$waiting,faithful$eruptions,pch=20)


# Validation Set
idx <- sample(1:length(eruptions), .8*length(eruptions), replace = F)
train <- faithful[idx,]
test <- faithful[-idx,]
fit <- lm(eruptions~waiting,data=train)
eruptions.hat <- predict(fit,newdata=test)
plot(test$eruptions, eruptions.hat-test$eruptions)
mean((fit$residuals)^2)

# Leave One Out Cross Validation (LOOCV)
mse <- rep(NA,length(faithful$eruptions))
for(i in 1:length(faithful$eruptions)){
  train <- faithful[-i,]
  test <- faithful[i,]
  fit <- lm(train$eruptions~train$waiting)
  mse[i] <- mean((fit$residuals)^2)
}
mean(mse)

# K Fold Cross Validation
k <- 5
mse <- rep(NA,k)
split <- length(faithful$eruptions)/k

for(i in 1:k){
  idx <- ((i-1)*split+1):(i*split)
  train <- faithful[idx,]
  test <- faithful[-idx,]
  fit <- lm(train$eruptions~train$waiting)
  mse[i] <- mean((fit$residuals)^2)
}
mean(mse)
