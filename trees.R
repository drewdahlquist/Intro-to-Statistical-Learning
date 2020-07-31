require(ISLR)
require(tree)

attach(Carseats)
hist(Sales)
High=ifelse(Sales<=8,"No","Yes") #create binary variable
Carseats=data.frame(Carseats,High) #attach new variable to Carseats

### Classification Tree
tree.carseats=tree(High~.-Sales,data=Carseats) #create tree-based classification model
summary(tree.carseats)
plot(tree.carseats) #plot tree
text(tree.carseats,pretty=0) #annotate plotted tree w text
tree.carseats #view detailed decisions of tree

# create training/test sets
set.seed(1011) #set a seed
train=sample(1:nrow(Carseats),250) #sample 250 observations from Carseats
tree.carseats=tree(High~.-Sales,data=Carseats,subset=train) #train model on training data
plot(tree.carseats) #plot tree
text(tree.carseats,pretty=0) #label
tree.pred=predict(tree.carseats,Carseats[-train,],type="class") #predict class labels on test set
with(Carseats[-train,],table(tree.pred,High)) #confusion matrix
(58+45)/150 #error rate

# use CV on tree
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass) #use misclass errors as basis for determining where to prune tree to
cv.carseats #print results from above
plot(cv.carseats) #plot misclass error from CV
prune.carseats=prune.misclass(tree.carseats,best=13) #prune to size of 13
plot(prune.carseats) #plot pruned tree
text(prune.carseats,pretty=0) #label
tree.pred=predict(tree.carseats,Carseats[-train,],type="class") #predict class labels on test set
with(Carseats[-train,],table(tree.pred,High)) #confusion matrix
(60+47)/150 #error rate

### Random Forest
require(randomForest)
require(MASS)
set.seed(101)
dim(Boston)
train=sample(1:nrow(Boston),300) #sample a training set
?Boston

rf.boston=randomForest(medv~.,data=Boston,subset=train) #train random forest on train data
rf.boston #details about our random forest (OOB)

oob.err=double(13) #for recording
test.err=double(13) #for recording
for(mtry in 1:13){ #for each value of mtry
  fit=randomForest(medv~.,data=Boston,subset=train,mtry=mtry,ntree=400) #train random forest for each mtry
  oob.err[mtry]=fit$mse[400] #out of bag error is calcualted w model
  pred=predict(fit,Boston[-train,]) #predict on test data
  test.err[mtry]=with(Boston[-train,],mean((medv-pred)^2)) #test MSE based on predictions
  cat(mtry," ") #print val of mtry as loop runs
}
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="MSE") #plot OOB & Test error on single tree of our forest
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))

### Boosting
require(gbm) #boosting package
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4) #fit boosted model
summary(boost.boston) #gives variable importance plot
plot(boost.boston,i="lstat") #partial dependence plot for lstat
plot(boost.boston,i="rm") #partial dependence plot for rm

n.trees=seq(from=100,to=10000,by=100) #view test performance as # of trees
predmat=predict(boost.boston,newdata=Boston[-train,],n.trees=n.trees) #predict on boosted model
dim(predmat)
berr=with(Boston[-train,],apply((predmat-medv)^2,2,mean)) #compute test error on all trees
plot(n.trees,berr,pch=19,ylab="MSE",xlab="# of Trees",main="Boosting Test Error") #plot boosting test error, note boosting is reluctant to overfit