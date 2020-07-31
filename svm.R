

### Support Vector Machines
set.seed(10111)
x=matrix(rnorm(40),20,2) #20 observations, 2 classes, 2 variable
y=rep(c(-1,1),c(10,10)) #y var = -1 or +1
x[y==1,]=x[y==1]+1 #move means from 0 to 1 when y=+1
plot(x,col=y+3,pch=19) #plot where points is blue/red for y=+1/-1

library(e1071) #import library for svm
dat=data.frame(x,y=as.factor(y)) #turn y into factor variable
svmfit=svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE) #fit support vector classifier
print(svmfit)
plot(svmfit,dat) #plot function for svm's, not very nice

# make our own svm plotting function
make.grid=function(x,n=75){
  grange=apply(x,2,range) #get range of each variable in x
  x1=seq(from=grange[1,1],to=grange[2,1],length=n) #values over range of x1
  x2=seq(from=grange[1,2],to=grange[2,2],length=n) #values over range of x2
  expand.grid(X1=x1,X2=x2) #makes lattice
}
xgrid=make.grid(x) #grid of predictor values
ygrid=predict(svmfit,xgrid) #make predictions on all grid values
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.5) #plot according to prediction
points(x,col=y+3,pch=19) #plot actual points
points(x[svmfit$index,],pch=5,cex=2) #support points

