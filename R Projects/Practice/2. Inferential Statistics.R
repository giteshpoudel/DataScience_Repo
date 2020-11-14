setwd("C:/Users/Gitesh/Desktop/Term 2/ALY6015/Week 2 Project")
library(tidyverse)
install.packages("dslabs")
library(dslabs)
data(murders)

summary(murders)


#calculating murder rate in per hundred thousand people
murder_rate <-  murders$total/murders$population * 100000
murders <- mutate(murders, murder_rate)
head(murders)
average_murder_rate <- mean(murder_rate)    
average_murder_rate   #2.779125

# We will test if the murder rate of Northeast states is less than country average
#one-sample t-test 
#H0: Murder rate of Northeast is greater than average murder rate of country
#H1: Murder rate of Northeast is  lower than average murder rate of country 
#confidence level of 95%

northeast <- filter(murders, murders$region == "Northeast" )
northeast

plot(northeast$murder_rate, main = "Northeast states murder rate compared to Country wide average", col="blue")
abline(a=average_murder_rate, b=0, col="red")

t.test(northeast$murder_rate, mu=average_murder_rate, alternative = "less", conf.level = 0.95)




#For two sample T-test and F-test you will create a new sample vector
#we will create a data.frame with only Western states

west <- filter(murders, murders$region == "West" )
west

plot(west$murder_rate, main = "Western states murder rate compared to Country wide average", col="blue")
abline(a=average_murder_rate, b=0, col="red")

#Before t-test conduct f-test to identify if they have different variance 
boxplot(northeast$murder_rate , west$murder_rate, main="Murder Rate of Northeast and West")

var.test(northeast$murder_rate, west$murder_rate, mu=0, conf.level = 0.95, alt="two.sided", var.equal = F, paired = F)

#t.test(murders$murder_rate ~ murders$region %in% c("Northeast", "West"), var.equal = F)   #not sure why this didn't work

t.test(northeast$murder_rate, west$murder_rate, mu=0, conf.level = 0.95, alt="two.sided", var.equal = T, paired = F)

