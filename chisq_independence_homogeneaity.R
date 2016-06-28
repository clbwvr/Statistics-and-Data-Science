# Chi Square Test of Independence

male<-c(200,150,50)
female<-c(250,300,50)
data <- data.frame(male,female)

x2 <- 0
for(i in 1:2){
  for(j in 1:3){
    exp <- sum(data[j,]) * sum(data[,i]) / sum(data)
    x2 <- x2 + (data[j,i] - exp)^2 / exp
  }
}
x2
qchisq(.95, (length(male)-1) * (length(female)-1))
# X2 is greater than chisq, so we reject

# Chi Square Test of Homogeneaity

male <- c(50,30,20)
female <- c(50,80,70)
data <- data.frame(male,female)
x2 <- 0
for(i in 1:2){
  for(j in 1:3){
    exp <- sum(data[j,]) * sum(data[,i]) / sum(data)
    x2 <- x2 + (data[j,i] - exp)^2 / exp
  }
}
x2
qchisq(.95, (length(male)-1) * (length(female)-1))
#Thus we reject the null

# What are we doing? We know the lrt follows a chi square
# We know large values of lrt mean large deviation from what's randomly expected from the cells
# Thus if it's big, there's something keeping things from being randomly expected.
# This is an association