import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

# Read data
df = pd.read_csv('train.csv')
# Melihat isi dari dataframe
print(df)

# Pembagian data categorical dan numerical
df_cat = df.select_dtypes(include='object').copy()
print(df_cat)

df_num = df.select_dtypes(include='number').copy()
print(df_num)

# Pembuatan barplot (horizontal dan vertikal)
# Bar plot Embarked
## Menggunakan Matplotlib
### Count Category Value
embarked_count = df_cat["Embarked"].value_counts()

### Get labels and values
embarked_label = embarked_count.index.tolist()
embarked_value = embarked_count.tolist()

### Draw vertical bar plot
fig, ax = plt.subplots()
ax.barh(embarked_label, embarked_value)
ax.set_title("Brand Counts")
plt.show()

### Draw horizontal bar plot
fig, ax = plt.subplots()
ax.bar(embarked_label, embarked_count)
ax.set_title("Brand Counts")
plt.show()

# Bar plot Sex

## Menggunakan Matplotlib
### Count Category Value
sex_count = df_cat["Sex"].value_counts()

### Get labels and values
sex_label = sex_count.index.tolist()
sex_value = sex_count.tolist()

### Draw vertical bar plot
fig, ax = plt.subplots()
ax.barh(sex_label, sex_value)
ax.set_title("Brand Counts")
plt.show()

### Draw horizontal bar plot
fig, ax = plt.subplots()
ax.bar(sex_label, sex_count)
ax.set_title("Brand Counts")
plt.show()

## Menggunakan Seaborn
### Draw vertical bar plot
sns.countplot(x="Sex", data = df_cat)

### Draw horizontal bar plot
sns.countplot(y="Sex", data = df_cat)

# Pembuatan distplot
## Dist plot Age
sns.displot(df_num["Age"], kde = True)
sns.displot(df_num["Fare"], kde = True)


# Pembuatan boxplot
sns.boxplot(df_num["Fare"])
sns.boxplot(df_num["Age"])
sns.boxplot(df_num["Pclass"])

# Pembuatan scatterplot
sns.scatterplot(x = df_num["Age"], y = df_num["Fare"])
sns.scatterplot(x = df_num["Pclass"], y = df_num["Fare"])
sns.scatterplot(x = df_num["Pclass"], y = df_num["Parch"])


# Pembuatan pie chart
survived_count = df_num["Survived"].value_counts()
survived_label = survived_count.index.tolist()
survived_value = survived_count.tolist()

fig, ax = plt.subplots()
ax.pie(survived_value, labels=survived_label, autopct='%1.1f%%')
ax.set_title("Survived")
plt.show()