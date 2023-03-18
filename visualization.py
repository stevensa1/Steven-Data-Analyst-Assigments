import panda as pd;
import numpy as np;
import matplotlib.pyplot as plt;

df = pd.read_csv(r'/content/Fitness_trackers_updated.csv')
dataBrand = np.array(df['Brand Name'])
uniqueBrand = np.unique(dataBrand)
brandCount = []
for item in uniqueBrand:
  brandCount.append(len(dataBrand[dataBrand == item]))