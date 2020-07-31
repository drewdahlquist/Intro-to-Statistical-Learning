require(ISLR)
require(MASS)

### Linear Discriminant Analysis
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=Year<2005) #Fit model
lda.fit
plot(lda.fit)
Smarket.2005=subset(Smarket,Year==2005) #Create subset of data
lda.pred=predict(lda.fit,Smarket.2005) #Predict using subset as test data
class(lda.pred) #lda.pred type (it's a list)
data.frame(lda.pred)[1:5,] #view first 5 entries in lda.pred
table(lda.pred$class,Smarket.2005$Direction) #Confusion matrix
mean(lda.pred$class==Smarket.2005$Direction) #Correct classification rate

### KNN
library(class)
?knn
attach(Smarket)
Xlag=cbind(Lag1,Lag2)
train=Year<2005 #Boolean for training observation subset
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=1) #KNN prediciton
table(knn.pred,Direction[!train]) #Confusion matrix
mean(knn.pred==Direction[!train]) #Correct classification rate

### Logistic Regression
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket,col=Smarket$Direction) #Color relates to response
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial) #Fit model, "family=binomial" is for logistic regression
summary(glm.fit)
glm.probs=predict(glm.fit,type="response") #Returns fitted probabilities
glm.probs[1:5] #View example fitted probabilities
glm.pred=ifelse(glm.probs>.5,"Up","Down") #Classify our probability predictions at about 50%
attach(Smarket)
table(glm.pred,Direction) #Confusion matrix
mean(glm.pred==Direction) #Correct classification rate
# Training and test set
train=Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train) #Fit model only using train data
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") #Predict on test data
glm.pred=ifelse(glm.probs>.5,"Up","Down")
Direction.2005=Smarket$Direction[!train] #Test classifications
table(glm.pred,Direction.2005) #Confusion matrix
mean(glm.pred==Direction.2005) #Correct classification rate
# Fit smaller model
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train) #Fit model
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") #Predict on test data
glm.pred=ifelse(glm.probs>.5,"Up","Down")
table(glm.pred,Direction.2005) #Confusion matrix
mean(glm.pred==Direction.2005) #Correct classification rate
summary(glm.fit)