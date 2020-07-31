require(ISLR)
require(boot)
?cv.glm
plot(mpg~horsepower,data=Auto)

### Leave-one-out CV
glm.fit=glm(mpg~horsepower,data=Auto) #fit linear model
cv.glm(Auto,glm.fit)$delta #cross validation, no args for "K" so cv.glm does LOOCV
# Write optimized function for LOOCV, from ISL (5.2)
loocv=function(fit){
  h=lm.influence(fit)$h #extract hat matrix diagonal into vector h
  mean((residuals(fit)/(1-h))^2)
}
loocv(glm.fit) #test function

cv.error=rep(0,5) #vector for collecting errors
degree=1:5 #iterator for degree
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data=Auto) #fit model for given polynomial degree
  cv.error[d]=loocv(glm.fit) #record error
}
plot(degree,cv.error,type="b") #plot errors

### 10-fold CV
cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1] #record error
}
lines(degree,cv.error10,type="b",col="red")

### Bootstrap
alpha=function(x,y){ #function for obtaining alpha, vectors x & y
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X,Portfolio$Y)
# What is the standard error of alpha?
alpha.fn=function(data,index){
  with(data[index,],alpha(X,Y))
}
alpha.fn(Portfolio,1:100)

set.seed(1) #bootstrap involves randomness
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE)) #like a one-time bootstrap

boot.out=boot(Portfolio,alpha.fn,R=1000) #1,000 bootstraps
boot.out #summary
plot(boot.out)