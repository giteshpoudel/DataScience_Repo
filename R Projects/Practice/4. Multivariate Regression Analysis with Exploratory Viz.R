library(tidyverse)
library(MASS)
data("Rubber")
Rubber
summary(Rubber)
plot(Rubber, col = "blue", pch=16)


# ploting hard vs loss
ggplot(data= Rubber, aes(hard, loss)) +  #ploting hard vs loss
  geom_point(aes(color = tens)) + #adding tens point as colored dots
  theme_gray() +    #one of the basic theme
  geom_smooth(method = 'lm', formula = y ~x, se = F) #adding line using linear function #se = display confidence interval


#ploting tens vs loss
ggplot(data = Rubber, aes(tens, loss)) +
  geom_point(aes(color = hard)) + 
  theme_bw() + 
  geom_smooth(method = "lm", formula = y~x, color="red", se= F) 
            
#we can observe negative correlation between both Loss and Hard, and Loss and Tens

#Linear model loss on Hard and tens
Rubber.lm1 <- lm(loss~hard+tens, data=Rubber)
summary(Rubber.lm1) 
plot(Rubber.lm1)

#termplot
par(mfrow=c(1,2))
termplot(Rubber.lm1, partial = TRUE, smooth = panel.smooth)
par(mfrow=c(1,1))


#Using Oddbooks 
#Loading DAAG library which contains Oddbooks dataset

install.packages("DAAG")     #install.packages downloads library not available in R 
data("oddbooks")
summary(oddbooks)

plot(oddbooks)
logbooks <- log(oddbooks)
plot(logbooks)
lmbooks <- lm(oddbooks)
plot(lmbooks)

#since weight is dependent upon all three dimensions, ie: thick*height*breadth 
logbooks.lm1<-lm(weight~thick,data=logbooks)
summary(logbooks.lm1)$coef 


logbooks.lm2<-lm(weight~thick+height,data=logbooks)
summary(logbooks.lm2)$coef

logbooks.lm3<-lm(weight~thick + height + breadth,data=logbooks)
summary(logbooks.lm3)$coef

#negative correlation can be observed 

#ploting linear models 

ggplot(oddbooks, aes(thick, weight)) +
  geom_point(aes(color = breadth)) + 
  theme_grey() +
  geom_smooth(method = 'lm', formula = y ~x, se = F) 


ggplot(oddbooks, aes(thick+ breadth, weight)) + 
  geom_point(aes(color = breadth)) + 
  theme_grey() +
  geom_smooth(method = 'lm', formula = y ~ x, se = F)

ggplot(oddbooks, aes(thick+height+breadth, weight)) + 
  geom_point(aes(color = thick)) +
  theme_grey() +
  geom_smooth(method = 'lm', formula = y ~x, se = F) 


install.packages("ggcorrplot")

MASS::Rubber
ggcorrplot::ggcorrplot(cor(Rubber),title="Rubber Corelation", method="circle")


