import os
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib as plt

# Get current dir
cwd = os.getcwd()
print("Current working directory:", cwd)

data_dir = cwd + '\Repeatability\Data.csv'

data = pd.read_csv(data_dir)
print(data)

# Create boxplot

sns.boxplot(data, x = "Components", y = "Result")