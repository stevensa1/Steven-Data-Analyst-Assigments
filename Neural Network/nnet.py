from visualization_engine import *
from optimizer_engine import *
from tensorflow.keras import models, layers, utils, backend as K
import pandas as pd
# import shap

'''
Fungsi Aktivasi:
https://www.tensorflow.org/api_docs/python/tf/keras/activations
Cheatsheet:
https://en.wikipedia.org/wiki/Activation_function 
'''

# Menggunakan fungsi aktivasi ReLU (rectified linear unit) untuk Hidden Layers
# Fungsi aktivasi Sigmaoid untuk klasifikasi
# Menggunakan fungsi aktivasi linear untuk memprediksi kejadian bencana.

n_features = 2

# Build Model with Keras Sequential
model = models.Sequential(name="DeepNN", layers=[
    ### hidden layer 1
    layers.Dense(name="h1", input_dim=n_features,
                 units=int(round((n_features+1)/2)), 
                 activation='relu'),
    layers.Dropout(name="drop1", rate=0.2),
    
    ### hidden layer 2
    layers.Dense(name="h2", units=int(round((n_features+1)/4)), 
                 activation='relu'),
    layers.Dropout(name="drop2", rate=0.2),
    
    ### layer output
    layers.Dense(name="output", units=1, activation='linear')
])
model.summary()

visualize_nn(model, description=True, figsize=(10,8))

# Plotting with TensorFlow
# utils.plot_model(model, to_file='model.png', show_shapes=True, show_layer_names=True)

# Training Model
model.compile(optimizer='adam', loss='mean_absolute_error', 
              metrics=[R2])

banjir_d = open("...\assets\data\Data Banjir 2019-2023.csv")
longsor_d = open("...\assets\data\Data Tanah Longsor 2019-2023.csv")

banjir = pd.read_csv(banjir_d)
longsor = pd.read_csv(longsor_d)

y = banjir['Kejadian Banjir']
x1 = banjir['Kejadian Banjir'].shift(1)
x2 = longsor['Kejadian Longsor'].shift(1)

# train/validation
training = model.fit(x=x1 + x2, y=y, batch_size=32, epochs=100, shuffle=True, verbose=0, validation_split=0.3)

# plot
metrics = [k for k in training.history.keys() if ("loss" not in k) and ("val" not in k)]    
fig, ax = plt.subplots(nrows=1, ncols=2, sharey=True, figsize=(15,3))
       
