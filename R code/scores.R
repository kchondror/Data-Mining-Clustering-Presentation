library(cluster)
library(fpc)
library(clusterSim)



data = read.csv("Datasets/synthetic_dataset_with_noise.csv")

x = data[,1]
y = data[,2]

# Create a dataframe with the x and y coordinates
df = data.frame(x = x, y = y)

centers = df[sample(nrow(df), 3),]




# Apply K-means clustering to dataset
km.result <- kmeans(df, centers = centers)
km.clusters <- km.result$cluster

# First, compute the silhouette scores for each sample in the data
scores <- silhouette(km.clusters, dist(df))

# Next, compute the mean silhouette score for each cluster
mean_scores <- aggregate(scores, by=list(km.clusters), mean)
print(mean_scores)

# Create the barplot
barplot(mean_scores[,4], names.arg=c("Cluster 1" ,"Cluster 2" ,"Cluster 3 "),col="#69b3a0", las=1)
title("Mean Silhouette Scores for Each Cluster")



#----------------------------------------------------------------------------------------------
  

db <- dbscan(data, eps = 0.6 ,MinPts = 6)

hc.result <- hclust(dist(df),method = 'single')
hc.clusters <- cutree(hc.result, k = 3)

km.result <- kmeans(df, centers = centers)
km.clusters <- km.result$cluster

CH_scores <- c(calinhara(data,km.clusters) , calinhara(data,hc.clusters) , calinhara(data,db$cluster))

barplot(CH_scores, names.arg=c("K-means" ,"Single Link" ,"db scan"),col="#69b3a0", las=1)
title("Calinski-Harabasz Index")


DB_scores <- c(index.DB(x = data, cl = km.clusters)[[1]], index.DB(x = data, cl = hc.clusters)[[1]] , index.DB(x = data, cl = db$cluster)[[1]] )

barplot(DB_scores, names.arg=c("K-means" ,"Single Link" ,"db scan"),col="#69b3a0", las=1)
title("Davies Bouldin Index")
