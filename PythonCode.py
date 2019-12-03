import os
os.chdir('C:/Users/Mr.Vyndictive/Dropbox/Machine Learning/Project 3')
os.chdir('/home/lu/Dropbox/Machine Learning/Project 3')
import matplotlib
import matplotlib.pyplot as plt
matplotlib.style.use('seaborn')
import numpy as np
import pandas as pd
from sklearn.cluster import KMeans

data = pd.read_csv('ToyData.csv')

data['type'] = data['type'].astype('category')

model = KMeans(n_clusters = 2, n_init = 20)


np.random.seed(0)
coordinates = data[['x','y']]
model.fit(coordinates)

labels = model.predict(coordinates)
data.insert((data.shape[1]),'label',labels)
data['label'] = data['label'].astype('category')


color = np.repeat('g',data.shape[0])
color[data['label'] == 0] = 'r'
data.insert((data.shape[1]),'color',color)
data['color'] = data['color'].astype('category')


plt.scatter(data['x'],data['y'], c=data['color'])


coordinates