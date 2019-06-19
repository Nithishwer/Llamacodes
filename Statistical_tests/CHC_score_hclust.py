from sklearn import metrics
from sklearn.metrics import pairwise_distances
from sklearn import datasets
df = pd.read_csv("somefile.csv")
X = df.to_numpy()
import numpy as np
from sklearn.cluster import KMeans
maxi=0
# Max number of clusters.
Maxno=50
for i in range(2,Maxno):
    kmeans_model = KMeans(n_clusters=i, random_state=1).fit(X)
    labels = kmeans_model.labels_
    chc = metrics.calinski_harabasz_score(X, labels)
    if chc>maxi:
        max_c=i
        maxi=chc
    print(" No of clusters:", i,"CHC Score : ", chc)
print("\n Best is ",max_c,"clusters; "," Score = ",maxi)
