import os;
import pandas as pd;
import numpy as np;
import matplotlib.pyplot as plt;
import seaborn as sns;
import plotly.express as px;


data = open("assets/data/Fitness_trackers_updated.csv")
df = pd.read_csv(data)

#Pie Chart

#Count Plot
sns.countplot(x = 'Brand Name', data = df)

#Histogram Plot (Price DIstribution)
sns.histplot(x = "Selling Price", data = df, kde = True, shrink = 1000, hue = "Brand Name", multiple = "stack")
sns.histplot(x = "Original Price", data = df, kde = True, shrink = 1000, hue = "Brand Name", multiple = "stack")

# Price vs Rating replot
sns.relplot(x = "Selling Price", y = "Rating (Out of 5)", data = df, hue = "Brand Name")
sns.relplot(y = "Selling Price", x = "Average Battery Life (in days)", data = df, hue = "Brand Name")

sns.relplot(y = "Selling Price", x = "Average Battery Life (in days)", data = df, col = "Brand Name")