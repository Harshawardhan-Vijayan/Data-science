#import data
setwd("H:\\Titanic")

train1<-read.csv("train.csv",sep=',',na.strings=c(''))
 train_sub<-floor(0.75*nrow(train1))
 set.seed(123)
 t <- sample(seq_len(nrow(train1)), size = train_sub)
 train<-train1[t,]
test<-read.csv("test.csv",sep=',',na.strings=c(''))
sum(is.na(train))
str(train)
##Imputing values for na's

# train$Embarked<-as.character(train$Embarked)
# train$Embarked<-ifelse(is.na(train$Embarked),'S',train$Embarked)

train$Embarked<-as.factor(train$Embarked)
table(train$Embarked)
# train$Age<-ifelse(is.na(train$Age),median(train$Age,na.rm = TRUE),train$Age)

###Converting the values into factors

train$Survived<-as.factor(train$Survived)
train$Pclass<-as.factor(train$Pclass)
library(e1071)
skewness(train$Age)
skewness(train$Fare)
skewness(train$SibSp)
skewness(train$Parch)
numdf<-subset(train,select=c('Age','SibSp','Parch','Fare'))
library(Hmisc)
cormat<-rcorr(as.matrix(numdf))
cormat<-as.data.frame(cormat$r)
View(cormat)
gendersurv<-tapply(train$Sex,train$Survived,function(x)length(x))
gendersurv
embarksurv<-tapply(train$Survived,train$Embarked,function(x)length(x))
agesurv<-tapply(train$Age,train$Survived,mean)
farePclass<-tapply(train$Fare,train$Pclass,mean)
farePclass
table(train$Sex,train$Survived)
table(train$Survived,train$Pclass)
table(train$Embarked,train$Survived)



###Model building####

model<-#(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,family='binomial') 
summary(model)
boxplot(model$residuals)
#AIC = Akaike Information Criterion every iteration we do AIC value comes down
#The maximum iteration it did is the fisher iteration
#In some cases Fisher scoring data will not converge
# this might be due to some categorical variables have same class like only male in all data points
train$probpred<-predict(model,train)
train$pred_lr<-ifelse(train$probpred>=0.5,1,0)
train$pred_lr<-as.factor(train$pred_lr)
table(train$Survived,train$pred_lr)
x<-(512+212)/(512+212+37+130)
x
#Try scaling at home

quantile(train$Parch)
skewness(train$Parch)
train$Fare<-ifelse(train$Fare>=31+1.5*(31-7.92135),31+1.5*(31-7.92135),train$Fare)
train$SibSp<-ifelse(train$SibSp>=2.5,2.5,train$SibSp)


describe(train$Fare)
quantile(train$Fare)
train$Fare<-ifelse(train$Fare<=6.4375,7.9104,train$Fare)
train$Fare<-ifelse(train$Fare>=227.5250,31,train$Fare)
skewness(train$Fare)


####Submission##################3
sum(is.na(test))
summary(test)
test$Fare<-ifelse(is.na(test$Fare),median(test$Fare,na.rm=TRUE),test$Fare)
test$Age<-ifelse(is.na(test$Age),median(test$Age,na.rm=TRUE),test$Age)
str(test)
test$Pclass<-as.factor(test$Pclass)
sum(is.na(test))
describe(test$Fare)
test$Fare<-ifelse(test$Fare<=6.9500,7.89580,test$Fare)
test$Fare<-ifelse(test$Fare>=227.5250,31.47188,test$Fare)
quantile(test$Fare)
str(train)
test$Survived<-predict(model,test,type='response')
test$Survived<-ifelse(test$Survived>=0.5,1,0)
test$Survived<-as.factor(test$Survived)
write.csv(test,'Submit1.csv')


library(party)
names(train)
X<-c("Pclass","Name","Sex" ,"Age","SibSp","Parch","Fare","Embarked")
Y<-c("Survived")
png(file='decisiontree.png')
output_tree<-ctree(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train)
plot(output_tree)
dev.off()
train$pred<-predict(output_tree,train)
table(train$pred,train$Survived)
x<-(492+246)/(492+246+96+57)
x
library(randomForest)
#to get same output again and again
rf<-randomForest(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train)
rf
train$pred_rf<-predict(rf,train)
table(train$Survived,train$pred_rf)
# x<-(533+274)/(533+274+16+68)
 test$Survived_RF<-predict(rf,test)
 write.csv(test,'rf_submission.csv')
# 
