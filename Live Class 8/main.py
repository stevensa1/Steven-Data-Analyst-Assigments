import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mlb
import seaborn as sns

# Read data
df1 = pd.read_csv('netflix_titles.csv')
df2 = pd.read_csv('population_total_long.csv')
df3 = pd.read_csv('world-happiness-report-2021.csv')
df4 = pd.read_csv('world-happiness-report.csv')

# Bar plot
sns.set_style('darkgrid')
plt.figure(figsize = (10, 5)) # p, l
sns.countplot(x='type', data=df1)
plt.show()

df1.groupby('type')[['show_id']].count()

# Bar Plot Top 10
plt.figure(figsize = (12, 5))
sns.countplot(data = df1, x = 'rating', order = df1['rating'].value_counts().index[0:5])

df3[['Country name', 'Explained by: Log GDP per capita']].sort_values(by = 'Explained by: Log GDP per capita')
