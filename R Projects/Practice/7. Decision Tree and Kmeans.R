setwd("/Users/Gitesh/Desktop/Term 2/ALY6015/Week 4")
install.packages("factoextra")
install.packages("NbClust")
install.packages("cluster")
install.packages("tidyverse")
library(tidyverse)
library(factoextra)
library(cluster)
library(NbClust)
install.packages("Mass")
install.packages("corrplot")
require(rio)    
require(corrplot)

wine <-import("winequality.csv")
head(wine)
str(wine)
attach(wine)

m<-cor(wine)
corrplot(m,method = "number", type="upper")

#dividing into training and test datasets
set.seed(1234)  #
ind <- sample(2, nrow(iris), replace=T, prob=c(0.7, 0.3))

wine.train <- wine[ind==1, ]
wine.test <- wine[ind==2, ]


#Decesion Trree
library(party)
wine.formula <- quality ~ .  # includes everything in wine dataset
wine.ctree <- ctree(wine.formula, data=wine.train)
wine.ctree
plot(wine.ctree)
pred<- round(predict(wine.ctree, newdata = wine.test))
table(pred, wine.test$quality)

install.packages("caret")
require(caret)
confusionMatrix(pred, wine.test$quality)  #test and train output has different level, cannot generate confusion matrix

#K-mean  

set.seed(1445)
wine2 <- wine

#clear class information
wine2$quality <- NULL

#Evaluate optimum number of cluster
# Using within group Sum of squares (WSS) plot
wssplot <- function(wine2, nc=11, seed=1234){
  wss <- (nrow(wine2)-1)*sum(apply(wine2,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(wine2, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
wssplot(wine2)

#Using Nbclust
library(NbClust)
set.seed(1440)
nc <- NbClust(wine2, min.nc = 2, max.nc = 15, method = "kmeans")

wine.kmeans <- kmeans(wine2, centers =3)

wine.kmeans$centers


plot(wine2 , col=wine.kmeans$cluster)
points(wine.kmeans$centers, col=1:3, pch="*", cex=15)

plot(wine2[c("alcohol", "total sulfur dioxide")] , col=wine.kmeans$cluster)
points(wine.kmeans$centers[,c("alcohol", "total sulfur dioxide")], col=1:3, pch="*", cex=5)

table(wine$quality, wine.kmeans$cluster)

install.packages("factoextra")
library(factoextra)
library(cluster)

sil <- silhouette(wine.kmeans$cluster, dist(wine2))
fviz_silhouette(sil)

#Density based clustering
install.packages("fpc")
library(fpc)

ds <- dbscan(wine2, eps = 0.1, MinPts = 8)
ds
table(wine$quality,ds$cluster )
plotcluster(wine2, ds$cluster) 


