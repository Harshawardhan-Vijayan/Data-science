setwd("H:\\Bigmart")
#bigmart data
#1. data prep and data transformation

train <- read.csv('train.csv', header = TRUE, sep = ',', na.strings = c(''))

test <- read.csv('test.csv', header = TRUE, sep = ',', na.strings = c(''))

str(train)

sum(is.na(train))
#import missing values
train$Item_Weight <- ifelse(is.na(train$Item_Weight),median(train$Item_Weight,
                            na.rm = TRUE),train$Item_Weight)
sum(is.na(train$Item_Weight))
#handle inconsistency for item fat content
table(train$Item_Fat_Content)
train$Item_Fat_Content <- as.character(train$Item_Fat_Content)
train$Item_Fat_Content <- ifelse(train$Item_Fat_Content == 'LF',
                                 'Low Fat', train$Item_Fat_Content)


train$Item_Fat_Content <- ifelse(train$Item_Fat_Content == 'low fat',
                                 'Low Fat', train$Item_Fat_Content)


train$Item_Fat_Content <- ifelse(train$Item_Fat_Content == 'reg',
                                 'Regular', train$Item_Fat_Content)
train$Item_Fat_Content <- as.factor(train$Item_Fat_Content)
table(train$Outlet_Size)
sum(is.na(train$Outlet_Size))
train$Outlet_Size <- as.character(train$Outlet_Size)
train$Outlet_Size <- ifelse(is.na(train$Outlet_Size), 'Medium', 
                            train$Outlet_Size)
train$Outlet_Size <- as.factor(train$Outlet_Size)

#create a new variable

#2. data visual
str(train)
train$YOB <- 2019 - train$Outlet_Establishment_Year


library(e1071)
names(train)
skewness(train$Item_Weight)
boxplot(train$Item_Weight)
skewness(train$Item_Visibility)
boxplot(train$Item_Visibility)
skewness(train$Item_MRP)
boxplot(train$Item_MRP)
skewness(train$Item_Outlet_Sales)
boxplot(train$Item_Outlet_Sales)
boxplot(train$YOB)

#3.ANalysis of data#############


library(Hmisc)
describe(train)
summary(train$Item_Visibility)
# move n/a values to ql and Q3
ifelse(train$Item_Visibility <= 0.003599378, 0.02699,train$Item_Visibility)
############Bi-varient
names(train)
# only the numeric values
numdf <- subset(train, select = c('Item_Outlet_Sales','YOB', 'Item_MRP','Item_Visibility',
                                  'Item_Weight'))
cormat <- rcorr(as.matrix(numdf))
# good corelatio 
cormat <- as.data.frame(cormat$r)

# describe(train$Item_Visibility)
# quantile(train$Item_Visibility)
# train$Item_Visibility<-ifelse(train$Item_Visibility<=0.003599378
#                               ,0.02698948,train$Item_Visibility)
# train$Item_Visibility<-ifelse(train$Item_Visibility>=0.309390255
#                               ,0.09458529,train$Item_Visibility)
# 
# train$Item_Visibility<-scale(train$Item_Visibility,scale=TRUE)  
  
# multi corleation  ---- ingnore identifier since its no use & year rename as YOB
Model <- lm(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+
              Item_MRP+Outlet_Size+Outlet_Location_Type+Outlet_Type+YOB, data = train)
summary(Model)


train$regpred<-predict(Model,train)
rmse_reg<-sqrt(mean((train$Item_Outlet_Sales-train$regpred)^2))
rmse_reg
###########################TEST##############################33

# 
# x <- train$Item_Visibility
# qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
# caps <- quantile(x, probs=c(.05, .95), na.rm = T)
# H <- 1.5 * IQR(x, na.rm = T)
# x[x < (qnt[1] - H)] <- caps[1]
# x[x > (qnt[2] + H)] <- caps[2]
# 
# 
# names(train)
# describe(train$Item_MRP)
# train$Item_MRP<-scale(train$Item_MRP,scale=TRUE)
# 
# # 
# 
#  train$Item_Visibility<-ifelse()
  train$Item_Visibility<-ifelse(train$Item_Visibility>0.09458529+1.5*0.06759581
                                ,0.09458529+1.5*0.06759521,train$Item_Visibility)
  train$Item_Visibility<-ifelse(train$Item_Visibility<0.02698948-1.5*0.06759581
                                ,0.02698948-1.5*0.06759521,train$Item_Visibility)
 train$Item_Visibility<-scale(train$Item_Visibility,scale=TRUE)

train$Item_Visibility<-(train$Item_Visibility-min(train$Item_Visibility))/
  (max(train$Item_Visibility)-min(train$Item_Visibility))
   boxplot(train$Item_Visibility)
sum(is.na(test))
summary(test)
test$Item_Weight<-ifelse(is.na(test$Item_Weight),median(test$Item_Weight,na.rm=TRUE)
                         ,test$Item_Weight)
test$Outlet_Size<-as.character(test$Outlet_Size)
test$Outlet_Size<-ifelse(is.na(test$Outlet_Size),'Medium',test$Outlet_Size)
test$Outlet_Size<-as.factor(test$Outlet_Size)
table(test$Outlet_Size)
sum(is.na(test))
quantile(test$Item_Visibility)
x<-0.09346262-0.02704688
test$Item_Visibility<-ifelse(test$Item_Visibility>0.09346262+1.5*x
                              ,0.09346262+1.5*x,test$Item_Visibility)
test$Item_Visibility<-ifelse(test$Item_Visibility<0.02704688-1.5*x
                              ,0.02704688-1.5*x,test$Item_Visibility)
test$YOB<-2019-test$Outlet_Establishment_Year
test$Item_Fat_Content <- as.character(test$Item_Fat_Content)
test$Item_Fat_Content <- ifelse(test$Item_Fat_Content == 'LF',
                                 'Low Fat', test$Item_Fat_Content)


test$Item_Fat_Content <- ifelse(test$Item_Fat_Content == 'low fat',
                                 'Low Fat', test$Item_Fat_Content)


test$Item_Fat_Content <- ifelse(test$Item_Fat_Content == 'reg',
                                 'Regular', test$Item_Fat_Content)
test$Item_Fat_Content <- as.factor(test$Item_Fat_Content)

test$Item_Outlet_Sales<-predict(Model,test)
write.csv(test,'Submit1.csv')
library(randomForest)
sum(is.na(bm_train))
rf <- randomForest(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Size+Outlet_Location_Type+Outlet_Type+YOB,data =train)
test$Item_Outlet_Sales<-predict(rf,test)
write.csv(test,'Submit1.csv')

















































