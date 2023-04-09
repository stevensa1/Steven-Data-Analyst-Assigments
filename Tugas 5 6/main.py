import pandas as pd
import numpy as np
import seaborn as sns

# Read data
df = pd.read_csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')
# Melihat isi dari dataframe
print(df)
# Mengecek nilai Null / NA pada dataframe
df.isnull().sum()
## Berdasarkan hasil output tersebut, diperoleh bahwa tidak terdapat nilai NULL
## pada dataframe tersebut

# Categorical data encoding
# Cek tipe data
df.dtypes
# Ambil data yang bertipe object
obj_df = df.select_dtypes(include='object').copy()

# Encoding Variable Gender dengan One Hot Encoding Encoding
obj_df["gender"].value_counts()
obj_df = pd.get_dummies(obj_df, columns = ["gender"])
obj_df.head()

# Encoding Variable Partner dengan Binary Encoding
obj_df["Partner"].value_counts()
obj_df["Partner"] = np.where(obj_df["Partner"] == "Yes", 1, 0)
obj_df.head()
# 0 > No
# 1 > Yes

# Encoding Variable Dependents dengan Binary Encoding
obj_df["Dependents"].value_counts()
obj_df["Dependents"] = np.where(obj_df["Dependents"] == "Yes", 1, 0)
obj_df.head()
# 0 > No
# 1 > Yes

# Encoding Variable PhoneService dengan Binary Encoding
# Melihat jumlah data pada tiap kategori
obj_df["PhoneService"].value_counts()
obj_df["PhoneService"] = np.where(obj_df["PhoneService"] == "Yes", 1, 0)
obj_df.head()
# 0 > No
# 1 > Yes

# Encoding Variable MultipleLines dengan Ordinal Encoding
# Melihat jumlah data pada tiap kategori
obj_df["MultipleLines"].value_counts()
multipledict = {
    "MultipleLines" : {
        "No phone service" : 0,
        "No" : 1,
        "Yes" : 2
    }
}
obj_df = obj_df.replace(multipledict)
obj_df.head()

# Encoding Variable InternetService dengan One Hot Encoding
# Melihat jumlah data pada tiap kategori
obj_df["InternetService"].value_counts()
obj_df = pd.get_dummies(obj_df, columns = ["InternetService"])
obj_df.head()
obj_df.dtypes

# Encoding Variable OnlineSecurity dengan Ordinal Encoding
# Melihat jumlah data pada tiap kategori
obj_df["OnlineSecurity"].value_counts()
online_dict = {
    "OnlineSecurity" : {
        "No internet service" : 0,
        "No" : 1,
        "Yes" : 2
    }
}
obj_df = obj_df.replace(online_dict)
obj_df.head()

# Encoding Variable OnlineBackup dengan Ordinal Encoding
# Melihat jumlah data pada tiap kategori
obj_df["OnlineBackup"].value_counts()
backup_dict = {
    "OnlineBackup" : {
        "No internet service" : 0,
        "No" : 1,
        "Yes" : 2
    }
}
obj_df = obj_df.replace(backup_dict)
obj_df.head()
obj_df.dtypes

# Encoding Variable DeviceProtection dengan Ordinal Encoding
# Melihat jumlah data pada tiap kategori
obj_df["DeviceProtection"].value_counts()
device_protection_dict = {
    "DeviceProtection" : {
        "No internet service" : 0,
        "No" : 1,
        "Yes" : 2
    }
}
obj_df = obj_df.replace(device_protection_dict)
obj_df.head()

# Encoding Variable TechSupport dengan Ordinal Encoding
# Melihat jumlah data pada tiap kategori
obj_df["TechSupport"].value_counts()
tech_support_dict = {
    "TechSupport" : {
        "No internet service" : 0,
        "No" : 1,
        "Yes" : 2
    }
}
obj_df = obj_df.replace(tech_support_dict)
obj_df.head()

# Encoding Variable StreamingTV dengan Ordinal Encoding
# Melihat jumlah data pada tiap kategori
obj_df["StreamingTV"].value_counts()
streaming_tv_dict = {
    "StreamingTV" : {
        "No internet service" : 0,
        "No" : 1,
        "Yes" : 2
    }
}
obj_df = obj_df.replace(streaming_tv_dict)
obj_df.head()
obj_df.dtypes

# Encoding Variable StreamingMovies dengan Ordinal Encoding
# Melihat jumlah data pada tiap kategori
obj_df["StreamingMovies"].value_counts()
streaming_movies_dict = {
    "StreamingMovies" : {
        "No internet service" : 0,
        "No" : 1,
        "Yes" : 2
    }
}
obj_df = obj_df.replace(streaming_movies_dict)
obj_df.head()
obj_df.dtypes

# Encoding Varibale Contract dengan One Hot Encoding
# Melihat jumlah data pada tiap kategori
obj_df["Contract"].value_counts()
obj_df = pd.get_dummies(obj_df, columns = ["Contract"])
obj_df.head()
obj_df.dtypes

# Encoding Variable PaperlessBilling dengan Binary Encoding
# Melihat jumlah data pada tiap kategori
obj_df["PaperlessBilling"].value_counts()
obj_df["PaperlessBilling"] = np.where(obj_df["PaperlessBilling"] == "Yes", 1, 0)
obj_df.head()
obj_df.dtypes

# Encoding Variable PaymentMethod dengan One Hot Encoding
# Melihat jumlah data pada tiap kategori
obj_df["PaymentMethod"].value_counts()
obj_df = pd.get_dummies(obj_df, columns = ["PaymentMethod"])
obj_df.head()
obj_df.dtypes

# Encoding Variable Churn dengan Binary Encoding
# Melihat jumlah data pada tiap kategori
obj_df["Churn"].value_counts()
obj_df["Churn"] = np.where(obj_df["Churn"] == "Yes", 1, 0)
obj_df.head()
obj_df.dtypes

# Menggabungkan dataframe obj_df dengan dataframe df
obj_col = df.select_dtypes(include='object').columns
df = df.drop(obj_col, axis=1)
df = pd.concat([df, obj_df], axis=1)
df.head()
df.dtypes

# Anomalies and Outlier Handling
## Melihat data yang duplikat
df.duplicated().sum()
## Tidak terdapat data yang duplikat
df.dtypes()
## Variable TotalCharges seharusnya bertipe numerik
## Namun, terdapat data yang bernilai spasi
## Maka, kita akan mengubah tipe data tersebut menjadi numerik
df['TotalCharges'] = pd.to_numeric(df['TotalCharges'], errors='coerce')
df.dtypes()

## Cek outliers pada data int
sns.boxplot(data=df['tenure'])
sns.boxplot(data=df['MonthlyCharges'])
sns.boxplot(data=df['TotalCharges'])

## Berdasarkan Boxplot, tidak terdapat data outliers pada data
