setwd("C:/Users/Gitesh/Desktop/Term 2/ALY6015/Week 2 Project")
library(tidyverse)
library("MASS")
data(chem)
chem
summary(chem)
hist(chem)
boxplot(chem, main="Box plot of Chem")

#H0 : mu < 1
#H1 : mu is greater than 1
#one-sided 95% confidence interval 

t.test(chem, mu=1, alternative = "greater", conf.level = 0.95)

#Part C
data(cats)
cats
summary(cats)
hist(cats$Bwt)
boxplot(cats$Bwt ~ cats$Sex, main="Male and female cats bodyweight")

#H0 : mean of Male Bwt = mean of female Bwt
#H1 : mean of male bwt is not equal to mean of female bwt
#two-sides with confidence interval of 95%

t.test(cats$Bwt ~ cats$Sex, mu=0, alt="two.sided", conf=0.95, 
       var.eq= FALSE, paired=FALSE)    #variance is not equal based on boxplot


#Part D

data("shoes")
shoes
hist(shoes$A)
hist(shoes$B)
boxplot(shoes$A, shoes$B, main="Wear of Material A and B")
plot(shoes$A, shoes$B)
abline(a=0, b=1)
t.test(shoes$A, shoes$B, mu=0, alt="two.sided", conf.level = 0.95, paired = T)

#Part E

data(bacteria)
bacteria
x <- factor.(bacteria$trt)
sucess <- filter(bacteria, y=="n")
prop.test()









#Part F

data(cats)
#in F-test we test the difference in variance of two sample
#Box plot comparism of two sample gives good visualization of difference in variance

boxplot(cats$Bwt ~ cats$Sex, main="Male and female cats bodyweight")

var(cats$Bwt[cats$Sex=="M"])
var(cats$Bwt[cats$Sex=="F"])

#Ho: Variance of bodyweight of male and female cats are equal
#H1: variance of bodyweight of male and femlae cats are not equal

var.test(cats$Bwt ~ cats$Sex, alt="two.sided", conf =0.95)
