setwd("/Users/Gitesh/Desktop/Term 2/ALY6015/Week 5 Project")
library(tidyverse)
require(rio)
library(zoo)
library("TTR")
library(tseries)
install.packages("forecast")
library(forecast)

data <- import("accidental.csv")
attach(data)

str(data)
colnames(data) <- c("Time","Deaths")

data$Time <- as.Date(as.yearmon(as.character(data$Time)), frac = 0)

data.ts <- ts(data$Deaths , start= c(1973,1), end = c(1978,12), frequency = 12)
str(data.ts)
#plotting time series
plot.ts(data.ts)



#Time series decomposition
data.ts.comp <- decompose(data.ts)
plot(data.ts.comp)

#seasonality adjusting
data.ts.adjusted <- data.ts - data.ts.comp$seasonal
plot(data.ts.adjusted, ylab="Death", xlab="year", main="Trend and Random")

#ARIMA model 

#DIckey-Fuller test for variable
adf.test(Deaths, alternative = "stationary", k=0)
#non stationary, as p-value is more than 0.05
adf.test(diff(Deaths), alternative = "stationary", k=0)


acf(data.ts, lag.max = length(data.ts))
pacf(data.ts, lag.max = 20)


data.ts.arima <- arima(data.ts, order = c(3,0,0))
plot(forecast(data.ts.arima))

auto.arima(data.ts)

autoplot(forecast(data.ts.arima))
