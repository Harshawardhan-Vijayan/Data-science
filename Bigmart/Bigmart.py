

####Bigmart data set
import os
import numpy as np
import pandas as pd
from sklearn.preprocessing import Imputer
import matplotlib.pyplot as plt

os.chdir("H:\\Bigmart")##Set directory

train=pd.read_csv("Train.csv")
test=pd.read_csv("Test_1.csv")
#sum(train.isna())s


#train.dtypes()#for data types
#c=0
#for i in pd.isna(train):
#    if i:
#        c+=1
#    else:
#        continue
#print(c)

summary_train=train.describe()# for summary of the data

#train.info()      

train.skew()
train['Outlet_Size'].isnull().value_counts()# missing values in Outlet size

train['Outlet_Size'].isnull().value_counts()

train.isnull().sum()# for getting all the missing values in the dataset
test.isnull().sum()
pd.crosstab(train["Outlet_Size"],train["Item_Fat_Content"])# table function in R
    
example=pd.Series(train['Item_Identifier'])#Store Item_Identifier as seperating variable

train["Outlet_Size"].mode()#to find mode of Outlet_Size
train["Item_Identifier"].isnull().sum()# to find missing values in Item_Identifier


####Imputing median to missing values
train['Item_Weight'] = np.where(train["Item_Weight"].isnull(),      
     np.nanmedian(train['Item_Weight']),train["Item_Weight"])###just like ifelse in R


train['Item_Weight'].isnull().sum()

x=pd.DataFrame(np.where(train["Outlet_Size"].isnull(),train['Outlet_Size'].mode(),train['Outlet_Size']))

x.isnull().sum()

train["Outlet_Size"]=np.where(train["Outlet_Size"].isnull(),train["Outlet_Size"].mode(),train["Outlet_Size"])
train["Item_Fat_Content"].value_counts()

j=0
x=pd.DataFrame({"Item_Fat_Content":[]})
print(j)
print(2)

test["Outlet_Size"]=np.where(test["Outlet_Size"].isnull(),test["Outlet_Size"].mode(),test["Outlet_Size"])


train.columns
train["YOB"] = 2019 - train["Outlet_Establishment_Year"]

test["YOB"] = 2019 - test["Outlet_Establishment_Year"]




j=0
#j=int(train["Item_Identifier"].count())
j

train["Item_Fat_Content"]=pd.DataFrame(np.where(train["Item_Fat_Content"]=="LF","Low Fat"
           ,train["Item_Fat_Content"]))

train["Item_Fat_Content"]=pd.DataFrame(np.where(train["Item_Fat_Content"]=="low fat","Low Fat"
           ,train["Item_Fat_Content"]))

train["Item_Fat_Content"]=pd.DataFrame(np.where(train["Item_Fat_Content"]=="reg","Regular"
           ,train["Item_Fat_Content"]))

test["Item_Fat_Content"]=pd.DataFrame(np.where(test["Item_Fat_Content"]=="LF","Low Fat"
           ,test["Item_Fat_Content"]))

test["Item_Fat_Content"]=pd.DataFrame(np.where(test["Item_Fat_Content"]=="low fat","Low Fat"
           ,test["Item_Fat_Content"]))

test["Item_Fat_Content"]=pd.DataFrame(np.where(test["Item_Fat_Content"]=="reg","Regular"
           ,test["Item_Fat_Content"]))




train["Item_Fat_Content"].value_counts()

skewness = train.skew()
x["Item_Fat_Content"].value_counts()

#if (train['Outlet_Size'].dtypes=="dtype(\'O\')"):
#    print(1)


#for i in train.columns:
#    if(train[i])
    
train['YOB']=2019-train['Outlet_Establishment_Year']

from sklearn import preprocessing
max_abs_scaler = preprocessing.MaxAbsScaler()
X_train_maxabs = max_abs_scaler.fit_transform(train["Item_Visiblity"])
X_train_maxabs


train.columns


plt.boxplot(train['YOB'])

z=train.corr()

plt.scatter(train['Item_MRP'],train['Item_Outlet_Sales'])

train["Item_Visiblity"].summary()
train.dtypes
#tr