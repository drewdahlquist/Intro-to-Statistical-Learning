library(ISLR)

summary(Hitters) #summary of our dataset
Hitters=na.omit(Hitters) #omit an NA data
with(Hitters,sum(is.na(Hitters))) #verify there is no NA data

### Best Subset Regression
library(leaps)
regfit.full=regsubsets(Salary~.,data=Hitters) #by default only gives up to subsets of size 8
summary(regfit.full) #view our best subsets
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19) #subsets up to 19 (i.e. all the variables)
reg.summary=summary(regfit.full) #create summary of our best subsets
names(reg.summary) #look at names on summary
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp") #plot Cp values
which.min(reg.summary$cp) #find out value for which Cp in minimized
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],pch=20,col="red") #highlight our min point on graph

plot(regfit.full,scale="Cp") #plot method for regsubset, plots Cp values & binary labels for which variables are included in each model
coef(regfit.full,10) #get coefficients for model with 10 variables

### Forward Stepwise Selection
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward") #still use regsubsets function, but select method="forward" to do forward stepwise selection
summary(regfit.fwd) #summary of our forward stepwise model
plot(regfit.fwd,scale="Cp") #same plotting format as above

### Model Selection using a Validation Set
dim(Hitters) #get dimensions of our data
set.seed(1)
train=sample(seq(263),180,replace=FALSE) #sample 180 observations from 1,2,3,...,263
train #look at our sample from above
regfit.fwd=regsubsets(Salary~.,data=Hitters[train,],nvmax=19,method="forward") #fit model on training set

val.errors=rep(NA,19) #setup vector with 19 slots
x.test=model.matrix(Salary~.,data=Hitters[-train,]) #matrix corresponding to validation set
for(i in 1:19){ #do this bc no predict method for reg subsets
  coefi=coef(regfit.fwd,id=i) #subset of variables used in i'th model
  pred=x.test[,names(coefi)]%*%coefi #to get predicitions we index names of coef vector to use correct variables & matrix multiply by coef vector
  val.errors[i]=mean((Hitters$Salary[-train]-pred)^2) #compute MSE for i'th model
}
plot(sqrt(val.errors),ylab="Root MSE",ylim=c(250,450),pch=19,type="b") #plot Root MSE
points(sqrt(regfit.fwd$rss[-1]/180),col="blue",pch=19,type="b") #plot RSS w/o NULL model
legend("topright",legend=c("Training","Validation"),col=c("blue","black"),pch=19) #put a legend on our plot
# write a "predict" function for regsubsets
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]]) #regsubsets has a "call" component, this line extracts its formula
  mat=model.matrix(form,newdata) #model.maatrix using formula from above
  coefi=coef(object,id=id) #extracts coefs
  mat[,names(coefi)]%*%coefi #do matrix multiplication
}

### Model Selection with Cross-Validation (10-fold)
set.seed(11)
folds=sample(rep(1:10,length=nrow(Hitters))) #create our 10-fold samples
folds #see 10-folds
table(folds) #see 10-folds tabulated
cv.errors=matrix(NA,10,19) #create empty CV error matrix w 10 rows & 9 cols
for(k in 1:10){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=k,],nvmax=19,method="forward") #fit regsubset model on 9 folds
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==k,],id=i) #use our predict method to predict on remaining fold
    cv.errors[k,i]=mean((Hitters$Salary[folds==k]-pred)^2) #compute MSE
  }
}
rmse.cv=sqrt(apply(cv.errors,2,mean)) #compute column Root MSE
plot(rmse.cv,pch=19,type="b") #plot Root MSE

### Ridge Regression & the Lasso
library(glmnet)
x=model.matrix(Salary~.-1,data=Hitters) #create matrix for glmnet
y=Hitters$Salary #create vector for glmnet

# Ridge
fit.ridge=glmnet(x,y,alpha=0) #fit a ridge regression model (alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE) #plot coefs vs log lambda
cv.ridge=cv.glmnet(x,y,alpha=0) #10-fold CV with ridge model
plot(cv.ridge) #plot CV MSE vs log lambda (vertical lines at min & 1-SE from min)

# Lasso
fit.lasso=glmnet(x,y) #fit a lasso model (alpha=1, by default)
plot(fit.lasso,xvar="lambda",label=TRUE) #plot coefs vs log lambda
plot(fit.lasso,xvar="dev",label=TRUE) #plot coefs vs deviance explained
cv.lasso=cv.glmnet(x,y) #10-fold CV with lasso model
plot(cv.lasso) #plot CV MSE vs log lambda, same as above
coef(cv.lasso) #returns coef vector for best model
sum(coef(cv.lasso)!=0) #returns no. of non-0 predictors used in best model (including intercept)

# fit lasso with train & validation set
lasso.tr=glmnet(x[train,],y[train]) #fit lasso with training set
lasso.tr #fit details for each model done
pred=predict(lasso.tr,x[-train,]) #predict with validation set
dim(pred) #observations, values of lambda
rmse=sqrt(apply((y[-train]-pred)^2,2,mean)) #get RMSE
plot(log(lasso.tr$lambda),rmse,type="b",xlab="Log(lambda)") #plot RMSE vs log lambda
lam.best=lasso.tr$lambda[order(rmse)[1]] #get best lambda value
lam.best #best lambda value
coef(lasso.tr,s=lam.best) #coefs that best lambda results in

### Principal Component Regression
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV") #fit pcr model after standardizing (scale=TRUE) & using CV
summary(pcr.fit) #summary of our model
validationplot(pcr.fit,val.type="MSEP") #MSE on validation set
set.seed(1)
pcr.fit=pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation ="CV")
# to be finished