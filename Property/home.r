setwd("H:\\Property")
train<-read.csv("train.csv",sep=',',na.strings=c(''))
sum(is.na(train))
summary(train)
str(train)
library(Amelia)

ni<-amelia(train,m=5,idvars = c("Id","Building_Class","Zoning_Class","Lot_Size","Property_Shape","Land_Outline"),
noms=c("Road_Type","Lane_Type","Utility_Type",""))
summary(train)
