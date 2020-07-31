require(ISLR)
attach(Wage)

### Polynomial Regression
fit=lm(wage~poly(age,4),data=Wage) #fit a 4th degree polynomial regression
summary(fit)

agelims=range(age) #2x1 vector of the range of age
age.grid=seq(from=agelims[1],to=agelims[2]) #sequence of numebrs between min & max of age
preds=predict(fit,newdata=list(age=age.grid),se=TRUE) #predict w our fitted model, ages in the age range, and have SEs
se.bands=cbind(preds$fit+2*preds$se,preds$fit-2*preds$se) #create 2*SE bands
plot(age,wage,col="darkgrey") #plot underlying data
lines(age.grid,preds$fit,lwd=2,col="blue") #plot our fitted function
matlines(age.grid,se.bands,col="blue",lty=2) #plot +-2*SE bands

# Using ANOVA() for testing which predictors are significant
fita=lm(wage~education,data=Wage) #fitted model a
fitb=lm(wage~education+age,data=Wage) #fitted model b, superset of a
fitc=lm(wage~education+poly(age,2),data=Wage) #fitted model c, superset of b
fitd=lm(wage~education+poly(age,3),data=Wage) #fitted model d, superset of c
anova(fita,fitb,fitc,fitd) #anova for nested sequence of models

### Polynomial Logistic Regression
fit=glm(I(wage>250)~poly(age,3),data=Wage,family=binomial) #fit log regression model
summary(fit)
preds=predict(fit,list(age=age.grid),se=T) #predict within range of age
se.bands=preds$fit+cbind(fit=0,lower=-2*preds$se,upper=2*preds$se) #create 2*SE bands
se.bands[1:5,] #view first 5 rows of resulting se.bands matrix

prob.bands=exp(se.bands)/(1+exp(se.bands)) #inverse logit function
matplot(age.grid,prob.bands,col="blue",lwd=c(2,1,1),lty=c(1,2,2),type="l",ylim=c(0,.1)) #plot our model + 2*SE lines
points(jitter(age),I(wage>250)/10,pch="I",cex=.5) #plot data corresponding w model

### Splines
require(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage) #fit linear spline model with 3 knots using cubic polynomials
plot(age,wage,col="darkgrey") #plot data
lines(age.grid,predict(fit,list(age=age.grid)),col="darkgreen",lwd=2) #plot model
abline(v=c(25,40,60),lty=2,col="darkgreen") #plot knots

### Smoothing Splines
fit=smooth.spline(age,wage,df=16) #fit smoothing spline w 16 deg of freedom
lines(fit,col="red",lwd=2) #plot model

fit=smooth.spline(age,wage,cv=TRUE) #fit smoothing spline w CV
lines(fit,col="purple",lwd=2) #plot model
fit #fitted model info, look at eff deg of free

### Generalized Additive Models
require(gam) #for below, s() is smoothing function in gam pkg
gam1=gam(wage~s(age,df=4)+s(year,df=4)+education,data=Wage) #fit linear regression gam
par(mfrow=c(1,3))
plot(gam1,se=T) #plot each term + SEs

gam2=gam(I(wage>250)~s(age,df=4)+s(year,df=4)+education,data=Wage,family=binomial) #fit log regression gam
plot(gam2) #plot each term

gam2a=gam(I(wage>250)~s(age,df=4)+year+education,data=Wage,family=binomial) #fit another log regression gam w linear term for 'year'
anova(gam2a,gam2,test="Chisq") #Chi^2 ANOVA test

# use GAM plot method for non-GAM objects
par(mfrow=c(1,3))
lm1=lm(wage~ns(age,df=4)+ns(year,df=4)+education,data=Wage) #fit linear model w natural splines
plot.Gam(lm1,se=T) #plot linear model using gam plot function