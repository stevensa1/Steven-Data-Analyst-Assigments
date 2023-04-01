import warnings
warnings.filterwarnings("ignore")

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
%matplotlib inline

## Membuat Dataframe
list_a = [1, 2, 3, 4, 5]
df = pd.DataFrame({
    'a' : list_a,
    'b' : [10, 20, 30, 40, 50],
    'c' : [100, 200, 300, 400, 500],
    'd' : [1000, 2000, 3000, 4000, 5000],
})
df

## Export Data to Files
df.to_csv('output/test.csv', index=False)
df.to_excel('output/test.xlsx', sheet_name="Sheet1")

## Import Data dari Seaborn untuk Pandas Dataframe
df = sns.load_dataset('titanic')
df
df.info()

## Data Type untuk untuk tiap variabel
# int64 = integer
# float64 = float
# object = string
df.dtypes

# Exercise 5.1
dfa = sns.load_dataset('diamonds')
# 1. Ada berapa recors dan variabel?
dfa.shape # Melihat dimensi
## 53940 recors
## 10 variabel

# 2. Kolom apa aja?
dfa.columns

# 3. Jenis data dari variabel tersebut apa aja?
dfa.dtypes

# Data Frame Methods
dfa.describe() # Statistik Deskriptif
dfa.sample(5) # Random Sampling sejumlah 5 data
dfa.head(5) # Melihat 5 data pertama
dfa.tail(5) # Melihat 5 data terakhir
dfa.isnull().sum() # Melihat jumlah missing value
dfa.dropna(inplace=True) # Drop null value
dfa.dropna(subset = ['price'], inplace=True) # Drop kolom yang memiliki null value
dfa.drop_duplicates(inplace=True) # Drop duplicate value
dfa.drop(['price'], axis=1, inplace=True) # Drop kolom price
dfa.max() # Melihat nilai maksimum
dfa.min() # Melihat nilai minimum
dfa.mean() # Melihat nilai rata-rata
dfa.median() # Melihat nilai median
dfa.std() # Melihat nilai standar deviasi   
dfa.corr() # Melihat korelasi antar variabel  
dfa.describe(percentiles=[0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99]) # Melihat statistik deskriptif dengan percentiles
dfa.distplot(dfa['price']) # Melihat distribusi data

dfa[:50].mean() # Melihat nilai rata-rata dari data pertama 50 data

# DataFrame Operations

## Sorting
df.sort_values(by='fare', ascending=False) # Mengurutkan data berdasarkan kolom price secara descending
df.sort_values(by=['fare', 'age'], ascending=False) # Mengurutkan data berdasarkan kolom price dan age secara descending
dfa.sort_values(by=['carat', 'price'], ascending=False) # Mengurutkan data berdasarkan kolom carat dan price secara descending
dfa.sort_values(by=['carat', 'price'], ascending=False).reset_index(drop=True) # Mengurutkan data berdasarkan kolom carat dan price secara descending dan mengurutkan index dari 0

# Data Cleansing I
