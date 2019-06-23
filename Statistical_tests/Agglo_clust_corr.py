from sklearn import metrics
from sklearn.metrics import pairwise_distances
from sklearn import datasets
from sklearn.cluster import AgglomerativeClustering
import pandas as pd
from scipy.stats import pearsonr
import numpy as np
from sklearn.metrics import davies_bouldin_score

def pearson_affinity(M):
   return 1 - np.array([[pearsonr(a,b)[0] for a in M] for b in M])

df = pd.read_csv("/home/pennyworth/Seg_all.csv")
X = df.to_numpy()
nrows=df.count()[1]
mini=100
l=[]
for i in range(2,nrows):
    cluster = AgglomerativeClustering(n_clusters=i, linkage='average',
                           affinity=pearson_affinity)
    cluster.fit(X)
    labels = cluster.labels_
    db=davies_bouldin_score(X, labels)
    l.append(db)
    if db<mini:
        mini_c=i
        mini=db
    print(" No of clusters:", i,"DB Score : ", db)
print("\n Best is ",mini_c,"clusters; "," Score = ",mini)
print(l)
