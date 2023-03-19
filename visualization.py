import os;
import pandas as pd;
import numpy as np;
import matplotlib.pyplot as plt;
import seaborn as sns;

data = open("assets/data/Fitness_trackers_updated.csv")
df = pd.read_csv(data)
dataBrand = np.array(df['Brand Name'])
uniqueBrand = np.unique(dataBrand)
brandCount = []
for item in uniqueBrand:
  brandCount.append(len(dataBrand[dataBrand == item]))

#Pie Chart
plt.pie(brandCount, labels = uniqueBrand)