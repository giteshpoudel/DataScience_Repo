setwd("/Users/Gitesh/Desktop/Term 2/ALY6015/Week 3 Project")
install.packages("biglasso")
library(lars)
data(diabetes)
install.packages("glmnet")
library(glmnet)
head(diabetes)
diabetes

#Exercise 2

attach(diabetes)   #attaching dataset to make addressing easy

par(mfrow=c(2,5))
for(i in 1:10){
  plot(x[,i], y, main = substitute(paste('y vs ', x[i]))) 
  abline(lm(y~x[,i]))
}

#Exercise 3
class(x)
reg_x <- lm(y~x)
reg_x   #coeffiecent of model 
summary(reg_x) 

#Exercise 4

L1_model <- glmnet(x,y)

plot(L1_model,xvar="norm", label = T)

#Exercise 5
#cross-validation 
cv_fit <- cv.glmnet(x=x, y=y, alpha = 1, nlambda = 1000)

plot(cv_fit)    #cv-plot


#Exercise 6 
min <- cv_fit$lambda.min
fit <- glmnet(x=x, y=y, alpha = 1, lambda=min)
#beta matrix when lambda is minimum
fit$beta     # 3 coeff are Zero

#Exercise 7
SE1 <-cv_fit$lambda.1se
fit <- glmnet(x=x, y=y, alpha = 1, lambda=SE1)
fit$beta    # 6 coeff are now shrunk to zero

#Exercise 8
reg_x2 <- lm(y~x2)
summary(reg_x2)

#Exercise 9
LA1_model <- glmnet(x2,y)
plot(LA1_model,xvar="norm", label = T)

#Exercise 10
cv_fit1 <- cv.glmnet(x=x2, y=y, alpha = 1, nlambda = 1000)
plot(cv_fit1)
min1 <- cv_fit1$lambda.min
fit1 <- glmnet(x=x2, y=y, alpha = 1, lambda= min1)
fit1$beta
