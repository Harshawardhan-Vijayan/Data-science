
setwd("H:\\Black Friday")
train<-read.csv("train.csv",sep=',',na.strings = c(''))
sum(is.na(train))

summary(train)
nrow(train)
x<-173638/550068
x<-383247/550068
drop(train$Product_Category_3)
train=subset(train,select=-Product_Category_3)
names(train)
library(Hmisc)
describe(train)
summary(train)
train$Marital_Status<-as.factor(train$Marital_Status)
train$Stay_In_Current_City_Years<-as.character(train$Stay_In_Current_City_Years)
train$Stay_In_Current_City_Years<-ifelse(train$Stay_In_Current_City_Years=='4+','4',train$Stay_In_Current_City_Years)
train$Stay_In_Current_City_Years<-as.factor(train$Stay_In_Current_City_Years)
train$Occupation<-as.factor(train$Occupation)
train$Product_Category_1<-as.factor(train$Product_Category_1)
train$Product_Category_2<-as.factor(train$Product_Category_2)

str(train)

library(Amelia)
train1<-amelia(train,m=5,idvars=c("User_ID","Product_ID"),noms=c("Gender","Age","City_Category",
                                                                 "Stay_In_Current_City_Years","Marital_Status"),ords=c("Occupation","Product_Category_1","Product_Category_2"))
write.amelia(train1,file.stem="I")
train1<-read.csv("I1.csv",sep=',',na.strings = c(''))
train_sub<-floor(0.75*nrow(train1))
set.seed(123)
t <- sample(seq_len(nrow(train1)), size = train_sub)
train<-train1[t,]
test<-read.csv("test.csv",sep=',',na.strings=c(''))

xgboost()
sum(is.na(train))

skewness(train$Product_Category_2)

str(train)

numdf<-subset(train,select = c("Product_Category_1","Product_Category_2"))
numdf<-rcorr(as.matrix(numdf))
numdf<-as.data.frame(numdf$r)
View(numdf)

library(party)

names(train1)
train1<-subset(train,select=-c(X,User_ID,Product_ID))
library(randomForest)
  
x<-ctree(Purchase~.-X-User_ID-Product_ID,data=train)

train$lm_pred<-predict(x,train)
x<-sqrt(sum((train$lm_pred-train$Purchase)^2)/nrow(train))
x


