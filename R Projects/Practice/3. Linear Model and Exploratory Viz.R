setwd("/Users/Gitesh/Desktop/Term 2/ALY6015/Week1 Project")
getwd()
library("tidyverse")
data(trees)
trees
summary(trees) 
str(trees)

h<- trees$Height
g<- trees$Girth 
#Transforming Grith from inches to ft 
g <- g/12
v <- trees$Volume


#Using Linear model
model1 <- lm(g~h, data=trees)
model2 <- lm(v~h, data=trees)

p <- plot(h, g, pch= 16, cex= 1.3, col= "red", main = "Grith vs Height",
          ylab ="Grith ofTree (ft)", xlab = "Height of Tree (Ft)")     #pch = 16 to create solid dots
abline(model1, col="blue") 
model1$coef


q <- plot(h, v, pch= 16, cex= 1.3, col= "red", main = "Volume vs Height",
          xlab ="Height of Tree (Ft)", ylab = "Volume of Tree (Cubic ft)") 
abline(model2, col="blue")
model2$


r <- plot(h, g, pch= 16, cex= 1.3, col= "red", main = "Grith vs Height",
          xlab ="Height of Tree (Ft)", ylab = "Diameter of Tree (Ft)")
abline(lm(g~h, data = trees), col="blue")     
model2$coef

#Histogram and Density Plot

#Histogram
hist(v,col = "red", border="black", main = "Frequency distribution of Volume of Trees")
hist(h,col= "blue", border = "black", main = "Frequency distribution of Height of Trees")
hist(g,col= "magenta", border="black", main= "Frequency distribution of Grith of Trees")

#Ading Density plot
d <- density(g) 
plot(d,main = "Density of Girth", col ="blue")
polygon(d, col= "red", border= "black")


#Box Plots

boxplot(v, main = "Box plot of Volume", col = "red", border="blue", ylab ="Volume of Tree (cubic ft)", las=1)   
# las=1 used to change orientation of number in y axis

boxplot(h, main = "Box plot of Height", col = "blue", border="red", ylab ="Height of Tree (ft)", las=1)   


boxplot(g, main = "Box plot of Grith", col = "magenta", border="black", ylab ="Grith of Tree (ft)", las=1)   

boxplot (h , g,  main="Box plot comparing Height and Grith of Tree", col= "yellow", ylab="Measurement in ft", las=1)

#Probability distribution 

#Probability distribution of Volume
v<- sort(v)  #sorting to plot correctly
mean(v)
sd(v)

densv <- dnorm(v, mean(v), sd(v))
plot(v, densv, type= "l", main = "Probability distribution of Volume", ylab = "Probabilty Density",
    xlab= "Volume of Tree", col="red", las=1)
abline(v=mean(v))

#Probability distribution of Height
h<- sort(h)  #sorting to plot correctly

densh <- dnorm(h, mean(h), sd(h))
mean(h)
plot(h, densh, type= "l", main = "Probability distribution of Height", ylab = "Probabilty Density",
     xlab= "Height of Tree", col="red", las=1)
abline(v=mean(h))

#Probability distribution of Grith
g<- sort(g)  #sorting to plot correctly

densg <- dnorm(g, mean(g), sd(g))
plot(g, densg, type= "l", main = "Probability distribution of Grith", ylab = "Probabilty Density",
     xlab= "Grith of Tree", col="red", las=1)
abline(v=mean(g))

