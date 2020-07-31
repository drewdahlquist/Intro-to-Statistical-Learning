library(MASS)
library(ISLR)

### Simple linear regression
names(Boston)
?Boston
plot(medv~lstat,Boston) #Plot medv vs lstat
lm.fit1=lm(medv~lstat,data=Boston) #Fit a model
lm.fit1
summary(lm.fit1)
abline(lm.fit1,col="red")
names(lm.fit1)
confint(lm.fit1)
predict(lm.fit1,data.frame(lstat=c(5,10,15)),interval="confidence") #Prediction for values of 5,10,15

### Multiple linear regression
lm.fit2=lm(medv~lstat+age,data=Boston) #Fit a model
summary(lm.fit2)
lm.fit3=lm(medv~.,data=Boston) #Fit a model
summary(lm.fit3)
par(mfrow=c(2,2)) #Create 2x2 matrix for tables in next command
plot(lm.fit3)
lm.fit4=update(lm.fit3,~.-age-indus) #Update fit3 model to no longer include age or indus

### Nonlinear terms and interactions
lm.fit5=lm(medv~lstat*age,data=Boston) #Fit a model with lstat,age,lstat:age
summary(lm.fit5)
lm.fit6=lm(medv~lstat+I(lstat^2),data=Boston) #Fit a model with lstat,lstat^2
summary(lm.fit6)
attach(Boston)
par(mfrow=c(1,1)) #Change back to 1x1 viewing
plot(medv~lstat)
points(lstat,fitted(lm.fit6),col="red",pch=20)
lm.fit7=lm(medv~poly(lstat,4))
points(lstat,fitted(lm.fit7),col="blue",pch=20)
plot(1:20,1:20,pch=1:20,cex=2) #View 20 available graph markers

### Qualitative predictors
names(Carseats)
summary(Carseats)
lm.fit1=lm(Sales~.+Income:Advertising+Age:Price,data=Carseats) #Fit a model
summary(lm.fit1)
contrasts(Carseats$ShelveLoc) #View contrast table

### Writing R functions
attach(Carseats)
regplot=function(x,y,...){ #... allows for extra various parameters
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)