Dat <- read.csv('air_monthly.csv', header = T)
Num_data <- Dat[, -(1:3)]

ts_dat <- as.vector(t(as.matrix(Num_data))) ### we transpose the matrix 

ts_dat <- ts(ts_dat, start = c(2017, 1), frequency = 12)

plot.ts(ts_dat)


##### additive decomposition
add_decomp <- decompose(ts_dat, type = 'additive')
plot(add_decomp)

##### multiplicative decomposition
multi_decomp <- decompose(ts_dat, type = 'multiplicative')
plot(multi_decomp)


library(seasonal)
library(forecast)
s <- stl(ts_dat, t.window = 13, s.window = 'periodic')
plot(s)
trendcycle(s) ### trend
remainder(s) ### random
seasonal(s) ### seasonal
seasadj(s)



library(forecast)
library(dplyr) 

### automatically fit an ARIMA model
auto.arima(remainder(s))

### Ljung-Box Test
x <- lapply(1:25, function(i){  
  p <- Box.test(remainder(s), lag = i, type = "Ljung-Box")  
  output <- data.frame(lag = i, p_value = p$p.value) 
  return(output) }) %>% bind_rows 

plot(x = x$lag,  
     y = x$p_value, ylim = c(0,1), 
     main = "Series white_noise - Ljung-Box Test", 
     xlab = "Lag", ylab = "P-Value") 

abline(h = 0.05, col="red", lwd=3, lty=2) 



library(ggplot2)
forecast(s, h=5, method = 'naive')

### fforecast(s, h=5, method = 'ets')



s %>% forecast(method="naive", h=5) %>% 
  autoplot(ylab = 'Ozone') + ggtitle('Forecasting')



library(TSstudio)
data(USUnRate)  
ts_info(USUnRate) 


unemployment <- window(USUnRate, start = c(1990,1)) 
ts_plot(unemployment,  
        title = "US Monthly Unemployment Rate",  
        Ytitle = "Unemployment Rate (%)", 
        Xtitle = "Year", 
        Xgrid = TRUE, 
        Ygrid = TRUE) 

set.seed(2020)  
ts_non_trend <- ts(runif(200, 5,5.2),start = c(2000,1),frequency = 12) 

ts_linear_trend_p<-ts_non_trend+1:length(ts_non_trend)/
  (0.5*length(ts_non_trend)) 
ts_linear_trend_n<-ts_non_trend-1:length(ts_non_trend)/
  (0.5*length(ts_non_trend)) 
ts_exp_trend<-ts_non_trend+exp((1:length(ts_non_trend) -1 )/ (0.5* length(ts_non_trend)))-1 



if(!require(xts)) install.packages('xts')
library(xts)

merged_series <- merge(
  Baseline_No_Trend = as.xts(ts_non_trend),  
  Positive_Linear_Trend = as.xts(ts_linear_trend_p), 
  Negative_Linear_Trend = as.xts(ts_linear_trend_n), 
  Exponential_Trend = as.xts(ts_exp_trend))  

ts_plot(merged_series,  
        type = "single", 
        Xgrid = TRUE, 
        Ygrid = TRUE, 
        title = "Different Types of Trends", 
        Ytitle = "The Values of the Series", 
        Xtitle = "Year") 

seasonal_pattern <- sin(2*pi*(1:length(ts_non_trend))/ frequency(ts_non_trend)) 

ts_seasonal <- ts_non_trend + seasonal_pattern 

ts_plot(ts_seasonal,  
        title = "Seasonal Pattern without Trend", 
        Xgrid = TRUE, 
        Ygrid = TRUE, 
        Ytitle = "The Values of the Series", 
        Xtitle = "Year") 

seasonal_pattern <- sin(2*pi * (1:length(ts_non_trend)) / frequency(ts_non_trend)) 

ts_seasonal <- ts_non_trend + seasonal_pattern 

ts_plot(ts_seasonal,  
        title = "Seasonal Pattern without Trend", 
        Xgrid = TRUE, 
        Ygrid = TRUE, 
        Ytitle = "The Values of the Series", 
        Xtitle = "Year") 

seasonal_with_Ptrend <- ts_linear_trend_p + seasonal_pattern 
seasonal_with_Ntrend <- ts_linear_trend_n - seasonal_pattern 
seasonal_with_Etrend <- ts_exp_trend + seasonal_pattern 

merged_series_seasonal <- merge(
  Positive_Linear_Trend = as.xts(seasonal_with_Ptrend), 
  Negative_Linear_Trend = as.xts(seasonal_with_Ntrend), 
  Exponential_Trend = as.xts(seasonal_with_Etrend)) 

ts_plot(merged_series_seasonal,  
        type = "single", 
        Xgrid = TRUE, 
        Ygrid = TRUE, 
        title = "Seasonal Pattern with Trend", 
        Ytitle = "The Values of the Series", 
        Xtitle = "Year")

ts_heatmap(USgas,  
           title = "Heatmap - the US Natural Gas Consumption")

ts_heatmap(USUnRate, 
           title = "Heatmap - The US Unemployment Rate") 

ts_heatmap(USgas,  
           title = "Heatmap - the US Natural Gas Consumption")

ts_heatmap(USUnRate, 
           title = "Heatmap - The US Unemployment Rate", col = 'Reds') 

set.seed(1234) 

white_noise <- ts(rnorm(12*10, mean =  0, sd = 1),  
                  start = c(2008, 1),  
                  frequency = 12) 

ts_plot(white_noise, title = "White Noise ~ N(0, 1)",  
        line.mode = "lines+markers", 
        Xgrid = TRUE, 
        Ygrid = TRUE, 
        Ytitle = "The Values of the Series") 

acf(white_noise)

if(!require(dplyr)) install.packages("dplyr")
library(dplyr) 
x <- lapply(1:24, function(i){  
  p <- Box.test(white_noise, lag = i, type = "Ljung-Box")  
  output <- data.frame(lag = i, p_value = p$p.value) 
  return(output) }) %>% bind_rows 

plot(x = x$lag,  
     y = x$p_value, ylim = c(0,1), 
     main = "Series white_noise - Ljung-Box Test", 
     xlab = "Lag", ylab = "P-Value") 

abline(h = 0.05, col="red", lwd=3, lty=2) 

