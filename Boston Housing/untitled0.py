#seaborn
import seaborn as sns
import pandas as pd
tips = sns.load_dataset('tips')
#evaluate two variables
sns.lmplot(x = 'total_bill', y = 'tip', data=tips, hue= 'sex', palette = 'Set1')
sns.lmplot(x = 'total_bill', y = 'tip', data=tips, col = 'sex')
#Strip plots jitter false all i/p to single row
sns.stripplot(y='tip', data=tips, jitter= False)
plt.ylabel('tip($)')
plt.savefig('example_2')
#mulitple stripplots
sns.stripplot(x='day',y='total_bill', data=tips, jitter = False)
#size
sns.stripplot(x='day',y='total_bill', data=tips,size=10, jitter = False)
#joinplot
sns.jointplot(x='total_bill', y='tip', data=tips)
