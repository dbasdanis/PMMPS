#PCA analysis in file movie_metadata.csv.

import numpy as np  #linear algebra
import pandas as pd  #data processing,CSV file I/O 
from sklearn.decomposition import PCA #Principal Component Analysis module
from sklearn.cluster import KMeans #KMeans clustering
import matplotlib.pyplot as plt # python defacto plotting library
import seaborn as sns  #more snuzzy plotting library

movie = pd.read_csv('movie_metadata.csv')
print(movie.head())