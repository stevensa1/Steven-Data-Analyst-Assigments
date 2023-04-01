import os;
import pandas as pd;
import matplotlib.pyplot as plt;


data = open("assets/data/Fitness_trackers_updated.csv")
df = pd.read_csv(data)

# Count Plot for top ten sold brand
countsbar = df["Brand Name"].value_counts()[:10]

## Get labels and values
labels = countsbar.index.tolist()
values = countsbar.tolist()

## Draw bar plot
fig, ax = plt.subplots()
ax.barh(labels, values)
ax.set_title("Brand Counts")
plt.show()

# Pie Chart for Top 5 Brands
## Get Value of total Brand Counts
countspie = df["Brand Name"].value_counts()[:5]

## Get labels and values
labels = countspie.index.tolist()
values = countspie.tolist()

## Add other values in pie chart
total_count = df["Brand Name"].count()
other_count = total_count - sum(values)

## Add other values in labels and values
labels.append("Lainnya")
values.append(other_count)

## Draw pie chart
fig, ax = plt.subplots()
ax.pie(values, labels=labels, autopct='%1.1f%%')
ax.set_title("Top 5 Brands")
plt.show()
