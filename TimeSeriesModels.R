# Practicing Simple Time Series Models


### AVERAGES ###
data <- c(356.0,371.6,373.7,380.4,364.8,373.1,367.4,373.4,374.1,380.1)
n <- length(data)


# Average Level Change
diffs <- rep(NA,n)
for(i in 2:n){
  diffs[i] <- data[i] - data[i-1]
}
avg_dif <- mean(diffs[-1])
forecast <- data[n] + avg_dif
plot(x=1:(n+1), y=(c(data,forecast)), type="b")


# Average Percentage Change
perc_diffs <- rep(NA,n)
for(i in 2:n){
  perc_diffs[i] <- (data[i] - data[i-1]) / data[i-1]
}
avg_perc_dif <- mean(perc_diffs[-1])
forecast <- data[n] + (data[n] * avg_perc_dif)
plot(x=1:(n+1), y=(c(data,forecast)), type="b")


# Weighted Average Percentage Change
perc_diffs <- rep(NA,n)
sum_weights <- 0
for(i in 2:n){
  perc_diffs[i] <- ((data[i] - data[i-1]) / data[i-1]) * (i-1)
  sum_weights = sum_weights + (i-1)
}
avg_perc_dif <- sum(perc_diffs[-1]) / sum_weights
forecast <- data[n] + (data[n] * avg_perc_dif)
plot(x=1:(n+1), y=(c(data,forecast)), type="b")
data[n]

### MOVING AVERAGES ###
data <- c(2009.3,2219.6,2598.9,2817.8,2981.4,3277.7,3516.4,3764.3,4267.9,5107.6)
n <- length(data)

# Single Moving Average Level Change
num_periods <- 3
diffs <- rep(NA,n)
for (i in 2:n){
  diffs[i] <- data[i] - data[i-1] 
}
ma <- rep(NA,n)
forecast <- rep(NA,n+1)
for (i in (num_periods+1):n){
  ma[i] <- sum(diffs[(i-num_periods+1):i]) / num_periods
  forecast[i+1] <- data[i] + ma[i]
}
plot(x=c(1:(n)),y=data,type="b")
lines(x=c(1:(n+1)),y=forecast, col="red")

# Single Moving Average Percentage Change
num_periods <- 3
perc_diffs <- rep(NA,n)
for (i in 2:n){
  perc_diffs[i] <- (data[i] - data[i-1]) / data[i-1]
}
ma <- rep(NA,n)
forecast <- rep(NA,n+1)
for (i in (num_periods+1):n){
  ma[i] <- sum(perc_diffs[(i - num_periods + 1) : i]) / num_periods
  forecast[i+1] <- data[i] + data[i]*ma[i]
}
plot(x=c(1:(n)),y=data,type="b")
lines(x=c(1:(n+1)),y=forecast, col="red")

# Double Moving Average Level Change
num_periods <- 3
diffs <- rep(NA,n)
for (i in 2:n){
  diffs[i] <- (data[i] - data[i-1])
}
ma <- rep(NA,n)
forecast <- rep(NA,n+1)
for (i in (num_periods+1):n){
  ma[i] <- sum(diffs[(i - num_periods + 1) : i]) / num_periods
}
dma <- rep(NA,n)
forecast <- rep(NA,n+1)
for(i in (2*num_periods):n){
  dma[i] <- sum(ma[(i - num_periods + 1):i]) / num_periods
  forecast[i+1] <- data[i] + dma[i]
}
plot(x=c(1:(n)),y=data,type="b")
lines(x=c(1:(n+1)),y=forecast, col="red")

# Double Moving Average Level Change
num_periods <- 3
perc_diffs <- rep(NA,n)
for (i in 2:n){
  perc_diffs[i] <- (data[i] - data[i-1]) / data[i-1]
}
for (i in (num_periods+1):n){
  ma[i] <- sum(perc_diffs[(i - num_periods + 1) : i]) / num_periods
}
dma <- rep(NA,n)
forecast <- rep(NA,n+1)
for(i in (2*num_periods):n){
  dma[i] <- sum(ma[(i - num_periods + 1):i]) / num_periods
  forecast[i+1] <- data[i] + dma[i] * data[i]
}
plot(x=c(1:(n)),y=data,type="b")
lines(x=c(1:(n+1)),y=forecast, col="red")



### EXPONENTIAL SMOOTHING ###

data <- c(2009.3,2219.6,2598.9,2817.8,2981.4,3277.7,3516.4,3764.3,4267.9,5107.6)
n <- length(data)


# Single Exponential Smoothing

alpha <- .9
forecast <- rep(NA,n+1)
forecast[1] <- data[1]
for (i in 2:(n+1)){
  forecast[i] <- (alpha * data[i-1]) + ((1 - alpha) * forecast[i-1])
}

# Plot
plot(x=c(1:(n)),y=data,type="b")
lines(x=c(1:(n+1)),y=forecast, col="red")
s


# Double Exponential Smoothing

alpha <- .9
s <- rep(NA,n)
d <- rep(NA,n)
s[1] <- d[1] <- data [1]
a <- rep(NA,n)
b <- rep(NA,n)
forecast <- rep(NA,n+1)
for (i in 2:n){
  s[i] <- alpha * data[i] + (1-alpha) * s[i-1]
}
for (i in 2:n){
  d[i] <- alpha * s[i] + (1-alpha) * d[i-1]
}
for (i in 1:n){
  a[i] <- (2*s[i]) - d[i]
  b[i] <- (alpha / (1-alpha)) * (s[i] - d[i])
}
# 1 because we are forecasting ahead one period at a time
# and only going until P11. If we kept going, we'd have to 
# start using 2 for P12, 3 for P13, etc.
for (i in 2:(n+1)){
  forecast[i] = a[i-1] + b[i-1] * (1) 
}
a

# Plot
plot(x=c(1:(n)),y=data,type="b")
lines(x=c(1:(n+1)),y=forecast, col="red")



### TREND LINE ###


# EVEN OBS
data <- c(17.99,16.6,15.13,17.55,19.96,18.77,16.81,18.97,22.31,20.77)
n <- length(data)
x <- rep(NA, n)
for (i in 1:n){
  x[i] <- 2* i - (n+1)
}
a <- (1/n) * sum(data)
b <- sum(data * x) / sum(x * x)
trend <- function(x) {
  return (a + b*x)
}
forecast <- trend(n+1)
plot(x=(1:n), y=data, type="b")
lines(x=(1:n+1),y=trend(x), col="red")
trend(x)
data

# ODD OBS
data <- c(17.99,16.6,15.13,17.55,19.96,18.77,16.81,18.97,22.31)
n <- length(data)
x <- rep(NA, n)
for (i in 1:n){
  x[i] <- i - ceiling(n/2)
}
a <- (1/n) * sum(data)
b <- sum(data * x) / sum(x * x)
trend <- function(x) {
  return (a + b*x)
}
forecast <- trend(n+1)
plot(x=(1:n), y=data, type="b")
lines(x=(1:n+1),y=trend(x), col="red")

### CLASSICAL DECOMPOSITION ###

# Data
sales <- c(18,16,15,15,16,13,12,15,17,19,22,18,25,23,22,23,18,16,15,21,23,26,30,26,32,29,27,27,23,20,20,23,25,26,35,31,38,32,30,31,28,27,28,31,33,37,42,39,48,45,41,41,39,35,38,41,43,48,52,46)
years <- c(rep("year1",12),rep("year2",12),rep("year3",12),rep("year4",12),rep("year5",12))
months <- rep(c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'), 5)
data <- data.frame(years,months,sales)


# Specific Indices (Sales * (1200)/ (sum of sales). This factor will make indices average to 100.)
# the total should be 1200. so how much does each month contribute?
expected_sum_indices <- 1200
specific_indices <- rep(NA,60)

specific_indices[1:12] <- sales[1:12] * expected_sum_indices / sum(sales[1:12])
specific_indices[13:24] <- sales[13:24] * expected_sum_indices / sum(sales[13:24])
specific_indices[25:36] <- sales[25:36] * expected_sum_indices / sum(sales[25:36])
specific_indices[37:48] <- sales[37:48] * expected_sum_indices/ sum(sales[37:48])
specific_indices[49:60] <- sales[49:60] * expected_sum_indices/ sum(sales[49:60])
data <- data.frame(data,specific_indices)
data

# Typical Indices (Average of middle three for each month)

# Modified Average  (Average the three middle values)
mod_average <- rep(NA,12)
for(i in 1:12){
  t <- subset(data,months==months[i])
  sorted <- sort(t$specific_indices)
  mod_average[i] <- (sorted[2]+sorted[3]+sorted[4]) / 3
}

# Multiple the indices by (1200/sum) so that we end up summing to 1200.
# Then divide by 100 to get the indices
expected_sum_indices / sum(mod_average) 
typical_indices <- mod_average * (expected_sum_indices / sum(mod_average)) 
seasonal_indices <- rep(typical_indices/100,5)
data <- data.frame(data,seasonal_indices)
data

# Trend
# Calculate Trend Line using coded X's and Deseasonalized (Y/S) values
# Apply each X to the trend line equation to get trend values
n <- length(sales)
x <- rep(NA, n)
for (i in 1:(n)){
  x[i] <- 2* i - ((n)+1)
}
data <- data.frame(data,x)

deseasonalized_values <- data$sales / data$seasonal_indices
data <- data.frame(data,deseasonalized_values)

a <- (1/n) * sum(data$deseasonalized_values)
b <- sum(data$deseasonalized_values * x) / sum(x^2)

trend_values <- (a + b * data$x)
data <- data.frame(data,trend_values)

# Cyclical
# Just take moving average of CR, which is deseasonalized_vales / trend_values
cr <- data$deseasonalized_values / data$trend_values
data <- data.frame(data,cr)
ma_cr <- rep(NA,61)
for (i in 4:61){
  ma_cr[i] <- mean(data$cr[(i-3):(i-1)])
}

# Apply F = S*T*C
fcst <- seasonal_indices[1] * (a + (b*(x[length(x)] +2))) * ma_cr[61]
fcst

### SALES RATIO



month <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
year1_sales <- c(194529,180053,193489,178690,175083,245968,203194,233556,252654,243747,295889,240746)
year2_sales <- c(204011,197708,186805,173225,183138,273495,186384,225785,259797,259425,265051,244524)
year3_sales <- c(198947,185557,177166,179221,168905,226617,231820,241445,214259,240701,256150,244156)

# Noncumulative Example. Predict year3 annual sales using monthly year3 data

year1_sales_ratio <- year1sales / sum(year1sales)
year2_sales_ratio <- year2sales / sum(year2sales)
avg_sales_ratio <- (year1_sales_ratio + year2_sales_ratio) / 2

year3_annual_prediction <- year3_sales / avg_sales_ratio
year3_annual_prediction_error <- 100 * (sum(year3_sales) - year3_annual_prediction) / sum(year3_sales)

plot(x=(1:(length(year3_annual_prediction))), y=year3_annual_prediction, type="b")
lines(x=(1:(length(year3_annual_prediction))), y=rep(sum(year3_sales),(length(year3_annual_prediction))), col="red")

# Cumulative Example. Predict year3 annual sales using monthly year3 data

year1_cumul_sales_ratio <- rep(NA,length(year1_sales))
year2_cumul_sales_ratio <- rep(NA,length(year2_sales))

for (i in 1:(length(year1_sales))){
  year1_cumul_sales_ratio[i] <- sum(year1_sales[1:i] / sum(year1_sales))
  year2_cumul_sales_ratio[i] <- sum(year2_sales[1:i] / sum(year2_sales))
  year3_cumul_sales[i] <- sum(year3_sales[1:i])
}

avg_cumul_sales_ratio <- (year1_cumul_sales_ratio + year2_cumul_sales_ratio) / 2
year3_annual_prediction <- year3_cumul_sales / avg_cumul_sales_ratio

plot(x=(1:(length(year3_annual_prediction))), y=year3_annual_prediction, type="b")
lines(x=(1:(length(year3_annual_prediction))), y=rep(sum(year3_sales),(length(year3_annual_prediction))), col="red")

# Quarterly Example.

# Noncumulative
year1_sales <- c(26.21, 23.45, 31.85, 25.28)
year2_sales <- c(25.76, 22.88, 34.02, 25.80)
year3_sales <- c(25.91, 24.07, 36.60, 26.43)
year4_sales <- c(27.08, 24.99, 41.29, 26.69)
sum(year4_sales)

year1_sales_ratio <- year1_sales / sum(year1_sales)
year2_sales_ratio <- year2_sales / sum(year2_sales)
year3_sales_ratio <- year3_sales / sum(year3_sales)

avg_sales_ratio <- (year1_sales_ratio + year2_sales_ratio + year3_sales_ratio) / 3

year4_annual_prediction <- year4_sales / avg_sales_ratio

# Cumulative

year1_cumul_sales_ratio <- rep(NA,length(year1_sales))
year2_cumul_sales_ratio <- rep(NA,length(year2_sales))
year3_cumul_sales_ratio <- rep(NA,length(year3_sales))
year4_cumul_sales <- rep(NA,length(year4_sales))

for (i in 1:length(year1_sales)){
  year1_cumul_sales_ratio[i] <- sum(year1_sales[1:i]) / sum(year1_sales)
  year2_cumul_sales_ratio[i] <- sum(year2_sales[1:i]) / sum(year2_sales)
  year3_cumul_sales_ratio[i] <- sum(year3_sales[1:i]) / sum(year3_sales)
  year4_cumul_sales[i] <- sum(year4_sales[i:1])
}

avg_cumul_sales_ratio <- (year1_cumul_sales_ratio + year2_cumul_sales_ratio + year3_cumul_sales_ratio) / 3

year4_annual_prediction <- year4_cumul_sales / avg_cumul_sales_ratio



### FAMILY MEMBER FORECASTING (DISAGGREGATION)

product_id <- c("S","M","L","XL", "XXL")
sku_sales_history <- c(35, 10, 9, 64, 21)
category_sales_history <- sum(sku_sales)
sku_ratio_history <- sku_sales / category_sales

category_forecast <- 25
sku_forecast <- category_forecast * sku_ratio_history
sku_forecast


### PERFORMANCE METRICS

sku <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
sales <- c(1500, 88, 240, 450, 700, 5000, 100, 70, 140, 230)
forecast <- c(1450, 92, 250, 480, 750, 5250, 95, 80, 132, 250)
N <- length(sales)
data <- data.frame(sku, sales, forecast)

# MPE = average error
mpe <- 100 * sum((sales - forecast) / sales) / N

# MAPE = average absolute error
mape <- 100 * sum((abs(sales - forecast) / sales)) / N

# WMAPE = average absolute error weighted by actual
wmape <- 100 * sum(sales * (abs(sales - forecast))/sales) / sum(sales)

# Range of Error
roe <- sd(100*(sales - forecast) / sales)
hist(100 * (sales - forecast) / sales)

