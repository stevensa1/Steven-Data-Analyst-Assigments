import pandas as pd
import numpy as np
import sklearn
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score, accuracy_score
from mlxtend.feature_selection import SequentialFeatureSelector
import matplotlib.pyplot as plt

# 1. Load dataset
df = pd.read_csv('train.csv')

# 2. Cleaning dataset
df = df.dropna(axis=1)
df = df.select_dtypes(include=[np.number])

# 3. Split into input features and target variable
X = df.drop('SalePrice', axis=1)
X = X.drop(['Id', 'MSSubClass', 'MoSold', 'YrSold', 'MiscVal'], axis=1)

# Define target variable
y = df['SalePrice']

# Splitting train (20%) and test (80%)
# Splitting train and test
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# 4. Create model
# Feature selection based on r2 score with backwise elimination
# Define the range of k_features
k_features_range = range(1, len(X.columns) + 1)

# Initialize lists to store R-squared values
r_squared_values = []
max_r_squared = -np.inf
best_k_features = []

# Iterate over different values of k_features
for k in k_features_range:
    # Create the SequentialFeatureSelector
    sfs = SequentialFeatureSelector(LinearRegression(), k_features=k, forward=False, scoring='r2', cv=None)
    
    # Fit the selector and extract selected features
    selected_features = sfs.fit(X, y)
    
    # Redefine X with only selected features
    X_selected = X[list(selected_features.k_feature_names_)]
    
    # Train the linear regression model
    model = LinearRegression()
    model.fit(X_selected, y)
    
    # Calculate R-squared
    r_sq = model.score(X_selected, y)
    if r_sq > max_r_squared:
        max_r_squared = r_sq
        best_k_features = list(selected_features.k_feature_names_)
    
    # Store the R-squared value
    r_squared_values.append(r_sq)

# Plot the chart
plt.plot(k_features_range, r_squared_values)
plt.xlabel('k_features')
plt.ylabel('R-squared')
plt.title('R-squared vs. k_features')
plt.show()

print(f"Max R-squared: {max_r_squared}")
print(f"Corresponding k_features: {best_k_features}")

# Redefine X with only selected features
X_selected = X[best_k_features]

# 5. Create model
model = LinearRegression()
model.fit(X_selected, y)

r_sq = model.score(X_selected, y)
print(f"coefficient of determination: {r_sq}")
print(f"intercept: {model.intercept_}")
print(f"coefficients: {model.coef_}")

# Predict test data
df_test = pd.read_csv('test.csv')

# Select only selected features
df_test = df_test[best_k_features]

# Check for missing values
print(df_test.isnull().sum())

# Fill missing values with mean
df_test = df_test.fillna(df_test.mean())

# Predict test data
y_pred = model.predict(df_test)

print(f"predicted response:\n{y_pred}")

# Save predicted response to csv with the column Id from 1461 to 2919 and column SalePrice with the predicted value
df_pred = pd.DataFrame({'Id': range(1461, 2920), 'SalePrice': y_pred})
df_pred.to_csv('submission.csv', index=False)