setwd("H:\\Bigmart")
bm_train<-read.csv("Train.csv",TRUE,',',na.strings=c(""))
test<-read.csv("Test.csv",TRUE,',',na.strings=c(""))
summary(bm_train$Item_Identifier)
str(bm_train)
sum(is.na(bm_train))#for knowing no.of na's
summary(bm_train)
sum(is.na(bm_train$Item_Weight))
bm_train$Item_Weight<-ifelse(is.na(bm_train$Item_Weight),median(bm_train$Item_Weight,na.rm=TRUE),bm_train$Item_Weight)
if(is.na(bm_train$Item_Weight))
{
  bm_train$Item_Weight<-median(bm_train$Item_Weight,na.rm=TRUE)
}
bm_train$Item_Weight<-ifelse(is.na(bm_train$Out))
#na.rm is for removing na
median(bm_train$Item_Weight)
table(bm_train$Outlet_Size)
sum(is.na(bm_train$Outlet_Size))
bm_train$Outlet_Size<-as.character(bm_train$Outlet_Size)
bm_train$Outlet_Size<-ifelse(is.na(bm_train$Outlet_Size),'Medium',bm_train$Outlet_Size)
bm_train$Outlet_Size<-as.factor(bm_train$Outlet_Size)
sum(is.na(bm_train))
summary(bm_train)
library(e1071)
skewness(x=Item_Weight)
skewness(x=Item_MRP)
skewness(x=Item_Visibility)
skewness(x=Item_Outlet_Sales)
Item_Visibility<-scale(Item_Visibility,scale=TRUE)
Item_Outlet_Sales<-scale(Item_Outlet_Sales,scale=TRUE)
bm_train$Item_Fat_Content<-as.character(bm_train$Item_Fat_Content)
bm_train$Item_Fat_Content<-ifelse(bm_train$Item_Fat_Content=='LF' ,'Low Fat',bm_train$Item_Fat_Content)
bm_train$Item_Fat_Content<-ifelse(bm_train$Item_Fat_Content=='reg','Regular',bm_train$Item_Fat_Content)
bm_train$Item_Fat_Content<-ifelse(bm_train$Item_Fat_Content=='low fat' ,'Low Fat',bm_train$Item_Fat_Content)

bm_train$Item_Fat_Content<-as.factor(bm_train$Item_Fat_Content)
model<-lm()
table(bm_train$Outlet_Size)
View(bm_train)

bm_train$YOB<-2019 - bm_train$Outlet_Establishment_Year



###################Analysis#########################
library(Hmisc)
describe(bm_train)
x<-quantile(bm_train$Item_Visibility,0.25)
bm_train$Item_Visibility<-ifelse(bm_train$Item_Visibility<=0.003599378,
                                 0.02698948,bm_train$Item_Identifier)


numdf<-subset(bm_train,select=c('Item_Outlet_Sales'
                             ,'YOB','Item_Visibility','Item_MRP','Item_Weight'))
cormat<-rcorr(as.matrix(numdf))
cormat<-as.data.frame(cormat$r)
#######Multi variate##########3
#(bm_train)
names(bm_train)
model<-lm(Item_Outlet_Sales~Item_MRP+Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Outlet_Type+Outlet_Size+Outlet_Location_Type+YOB,data=train)
View(summary(model))
library(randomForest)
sum(is.na(bm_train))
rf <- randomForest(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+Item_MRP+Outlet_Size+Outlet_Location_Type+Outlet_Type+YOB,data =bm_train)
bm_train$pred1<-predict(rf,bm_train)
rmse_reg<-sqrt(mean((bm_train$Item_Outlet_Sales-bm_train$pred1)^2))
rmse_reg
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

test$Item_Outlet_Sales<-predict(rf,test)
write.csv(test,'Submit1.csv')



