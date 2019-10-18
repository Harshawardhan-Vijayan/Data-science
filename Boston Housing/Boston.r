setwd("H://Boston Housing")
train<-read.csv("train.csv",sep=',',na.strings=c(''))
test<-read.csv("test.csv",sep=',',na.strings=c('')) 
sum(is.na(test))
summary(train)
str(train)
#changing chas as categorical variable
train$chas<-as.factor(train$chas)
is.factor(train$chas)
attach(train)#Whatever changes we do is applicable to train not test
#we can now just use the variable
typeof(train$chas)
str(train)
summary(train)
#install.packages('e1071')
library(e1071)
boxplot(x=crim)
# to find skewness
skewness(x=~train$crim+train$zn)
skew(train)
skewness(x=crim)
skewness(x=crim)
skewness(x=indus)
skewness(x=nox)
skewness(x=rm)
skewness(x=age)
skewness(x=dis)
skewness(x=rad)
skewness(x=tax)
skewness(x=ptratio)
skewness(x=black)
skewness(x=lstat)
skewness(x=medv)
# install.packages('Hmisc')
library(Hmisc)
describe(train)

library(Hmisc)
range(train$crim)
sd(train$crim)#Standard deviation
mean(x=crim)
mean(x=zn)
mean(x=indus)
mean(x=nox)
mean(x=rm)
mean(x=age)
mean(x=dis)
mean(x=rad)
mean(x=tax)
mean(x=ptratio)
mean(x=black)
mean(x=lstat)
mean(x=medv)
sd(x=crim)
sd(x=zn)
sd(x=crim)
sd(x=zn)
sd(x=indus)
sd(x=nox)
sd(x=rm)
sd(x=age)
sd(x=dis)
sd(x=rad)
sd(x=tax)
sd(x=ptratio)
sd(x=black)
sd(x=lstat)
sd(x=medv)
cor(crim,medv)
cormatrix<-rcorr(as.matrix(train))
View(cormatrix)
cormat<-data.frame(cormatrix$r)
View(cormat)
# coefficient
x<-names(train)
model<-lm(medv~crim,data = train)
View(model)
# left hand side of ~ is dependent
# righthandside of is independent
summary(model)
boxplot(model$residuals)
# to check whether errors are normally distributed
# residuals is actual - predicted         
# residual standard error is standard deviation of residues
# r squared is a value between 0 to 1,value close to 1 is good one
pred_df<-data.frame(model$fitted.values)#predicted values
resid_df<-data.frame(model$residuals)
#just use train$fit to add a column
train$fit_ex<-predict(model,train)
train
model1<-lm(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,data=train)
model1
boxplot(model1$residuals)
#crime rate and other variables impact might change
#i.e p value increases and loses significance value
summary(model1)
#tvalue is coefficient divided by std error
summary(model)
# r square is explain the variations in predicted value
# when we  see rsquared value as 0.7331 , 73 % is guessed correctly
                                                                                                          
train$regpred<-predict(model1,train)
skewness(train)
View(train)
rmse_reg<-sqrt(mean((train$medv-train$regpred)^2))
print(rmse_reg)

str(train)
train$crim<-scale(train$crim,scale=TRUE)#z score
train$zn<-scale(train$zn,scale=TRUE)
train$black<-scale(train$black,scale=TRUE)

##################On Test data#######################3
sum(is.na(test))
test$chas<-as.factor(test$chas)
test$medv<-predict(model1,test)
View(test)
# name in train and test names should be same
write.csv(test,'Submit1.csv')
detach(train)