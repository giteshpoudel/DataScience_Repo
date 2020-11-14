setwd("/Users/Gitesh/Desktop/Term 2/ALY6015/Week 5 Discussion")

install.packages("fpp")
library(fpp)

data <- a10
data
str(data) 
data.ts <- ts(data, frequency = 12, start = c(1991,7))
plot(data.ts, ylab="In millions", xlab= "year", main="Antidiabetic drug Sales")

#Time-series Decomposition

data.ts.comp <- decompose(data.ts)

plot(data.ts.comp)


#Seasonally Adjusting 

data.ts.adjusted <- data.ts - data.ts.comp$seasonal

plot(data.ts.adjusted, ylab="In Millions", xlab="year", main="Trend and Random")

acf(data.ts, lag.max = 20)

pacf(data.ts)








