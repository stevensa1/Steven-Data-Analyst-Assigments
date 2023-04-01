import os;
import pandas as pd;
import numpy as np;
import matplotlib.pyplot as plt;
import seaborn as sns;
import plotly.express as px;


data = open("assets/data/Fitness_trackers_updated.csv")
df = pd.read_csv(data)

#Pie Chart

# Count Plot for top ten sold brand
counts = df["Brand Name"].value_counts()[:10]

## Get labels and values
labels = counts.index.tolist()
values = counts.tolist()

## Draw bar plot
fig, ax = plt.subplots()
ax.barh(labels, values)
ax.set_title("Brand Counts")
plt.show()