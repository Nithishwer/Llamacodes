from sklearn import datasets
df = pd.read_csv("Somefile.csv",header=None)
X = df.to_numpy()
from sklearn.cluster import KMeans
from sklearn.metrics import davies_bouldin_score
mini=100
# Max number of clusters.
Maxno=147
for i in range(2,Maxno):
    kmeans = KMeans(n_clusters=i, random_state=1).fit(X)
    labels = kmeans.labels_
    db=davies_bouldin_score(X, labels)
    if db<mini:
        mini_c=i
        mini=db
    print(" No of clusters:", i,"DB Score : ", db)
print("\n Best is ",mini_c,"clusters; "," Score = ",mini)
