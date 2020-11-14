setwd("/Users/Gitesh/Desktop/Term 2/ALY6015/Week 3 Project")
require(tidyverse)
require(biglasso)
install.packages("glmnet")
require(glmnet)
install.packages("rio")
require(rio)


data <- import("data.csv") 

attach(data)
str(data)

head(data)


sample_size <- floor(0.75* nrow(data))   #75% of data will be taken for training model

set.seed(150)       # seting the seed to make partition reproducible


train_index <- sample(seq_len(nrow(data)), size= sample_size)

train_set <- data[train_index, ]
test_set <- data[-train_index, ]

x_train <- subset(train_set , select = -price_range)
y_train <- train_set$price_range
x_test <- subset(test_set, select= -price_range)
y_test <- test_set$price_range

#Lasso 
class(y_train)
class(x_train)
x_train <- as.matrix(x_train)
y_train <- as.matrix(y_train)
x_test <- as.matrix(x_test)
y_test <- as.matrix(y_test)

lasso_fit <- cv.glmnet(x_train, y_train, type.measure = "mse",
                       alpha = 1, family="gaussian")
lasso_fit
?predict
lasso_predict <- predict(lasso_fit, s=lasso_fit$lambda.1se, newx=x_test)
lasso_predict<- round(lasso_predict) #we can use round the y value to closet integer as y is factor
# we calculate mean-square difference between test and predicted value
# to check how close our model is to predict dependent variable

mean((y_test-lasso_predict)^2)

