# -*- coding: utf-8 -*-
"""
Created on Sat Aug 31 14:15:52 2019

@author: Harshwardhan
"""
#abcdefghijklmnop
import pandas as pd
import numpy as np
import os

os.chdir("H:\\Boston Housing")
train=pd.read_csv("train_Boston.csv")
train.isnull().sum()
train.dtypes
train.describe()
train["crim"].describe()

summ=train.describe()

train.describe()
for i in train.columns:
   print(train[i].describe())
   
for i in train.columns:
    if(i!="chas"):
        print(i,":",train[i].skew())
train.skew()

x=train.boxplot(column='crim')

import matplotlib.pyplot as plt

plt.boxplot(train['crim'])

x=train.corr()

from sklearn import preprocessing

min_max=preprocessing.MinMaxScaler().fit(train[['crim']])

df=pd.DataFrame(min_max.transform(train[['crim']]))

df.skew()

#i=train.columns
#
#print(i[j])
plt.boxplot(train[i[15]])

train['ln_crim']=np.log(train['crim'])

train['ln_crim'].skew() 

train.columns

plt.boxplot(train['ln_crim'])

train.columns
train.skew()
i='1'
train['ln_zn']=np.where(train['zn']==0,train['zn'],np.log(train['zn'],where=train['zn']!=0))
    
train['ln_zn'].isnull().value_counts()

train['ln_dis']=np.log(train['dis'],where=train['dis']!=0)

train['ln_rad']=np.log(train['rad'],where=train['rad']!=0)

#train['ptratio']=np.log(train['rad'],where=train['rad']!=0)

train['ln_black']=np.log(train['black'],where=train['black']!=0)

train['ln_lstat']=np.log(train['lstat'],where=train['lstat']!=0)

train['ln_zn'].skew()

train['ln_zn'].isna().sum() 

train['1']=1

train=train.drop(['1'],axis=1)


from sklearn.preprocessing import StandardScaler

scaler=StandardScaler()

train.skew()

train['chas_c']=train['chas'].astype('category')
train.skew()
train['crim_scale'] = scaler.fit_transform(train['crim'].values.reshape(-1,1))

# -1 is  for original shape 1 

train['zn'].dtype

from sklearn.preprocessing import MinMaxScaler

train['zn_scale']=scaler.fit_transform(train['zn'].values.reshape(-1,1))
train['zn_scale'].skew()

i=train.columns
i
for j in i:
    print(j)


train1=train
train=train.drop(['ln_crim','ln_zn','ln_dis','ln_rad','ln_black','ln_lstat','zn_scale'],axis=1)

trian

train.columns

x=train.corr()

y=pd.DataFrame(x['medv'])
y.sort_values(by=['medv'])

plt.scatter(train['rm'],train['medv'])

plt.scatter(train['lstat'],train['medv'])


y=train['medv']
train1=train
x=train  
y=train['medv']
x=x.drop(['ID','medv','zn','crim','chas'],axis=1)
x.columns

#del train1['zn']

import seaborn as sns

import statsmodels.api as sm

model = sm.OLS(y,x)#linear regression
            

results = model.fit()
#fit is for alpha and beta

results.summary()


from sklearn.linear_model import LinearRegression

LM=LinearRegression()

r=LM.fit(x,y)



LM.coef_
LM.intercept_


preds_y=LM.predict(x)

train.columns
train.shape[0]
rmse=(sum((preds_y-y)**2)/train.shape[0])**0.5


from sklearn.metrics import mean_squared_error

mse = mean_squared_error(y,preds_y)

np.sqrt(mse)

#glimpse(train)

results.summary()

with open("Sub_train",'a') as f:
    train.to_csv(f,header=False)