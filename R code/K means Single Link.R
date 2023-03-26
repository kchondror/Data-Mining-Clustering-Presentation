library(utils)
library(cluster)


#//K-means - Single Link//

#----------------------------------------------------------------------------------------------
data = read.csv("Datasets/synthetic_dataset.csv")

x = data[,1]
y = data[,2]

# Create a dataframe with the x and y coordinates
df1 = data.frame(x = x, y = y)

# Plot the data
plot(df1, main = "Synthetic Dataset", xlab = "X", ylab = "Y")

centers = df1[sample(nrow(df1), 3),]

# Apply K-means clustering to dataset
km.result = kmeans(df1, centers = centers)
km.clusters = km.result$cluster

# Plot K-means clusters for dataset
plot(df1, col = km.clusters, main = "K-means Clustering ", xlab = "X", ylab = "Y")
legend("topright", legend = unique(km.clusters), col = unique(km.clusters), pch = 20)


hc.result = hclust(dist(df1),method = 'single')
hc.clusters = cutree(hc.result, k = 3)


plot(df1, col =hc.clusters, main = "Single link clustering ", xlab = "X", ylab = "Y")
legend("topright", legend = unique(hc.clusters), col = unique(hc.clusters), pch = 20)



#----------------------------------------------------------------------------------------------
data = read.csv("Datasets/circle_dataset.csv")

x = data[,1]
y = data[,2]

# Create a dataframe with the x and y coordinates
df1 = data.frame(x = x, y = y)

# Plot the data
plot(df1, main = "Synthetic Dataset", xlab = "X", ylab = "Y")

centers = df1[sample(nrow(df1), 2),]

# Apply K-means clustering to dataset
km.result = kmeans(df1, centers = centers)
km.clusters = km.result$cluster

# Plot K-means clusters for dataset
plot(df1, col = km.clusters, main = "K-means Clustering ", xlab = "X", ylab = "Y")
legend("topright", legend = unique(km.clusters), col = unique(km.clusters), pch = 20)

hc.result = hclust(dist(df1),method = 'single')
hc.clusters = cutree(hc.result, k = 2)


plot(df1, col =hc.clusters, main = "Single link Clustering ", xlab = "X", ylab = "Y")
legend("topright", legend = unique(hc.clusters), col = unique(hc.clusters), pch = 20)




#----------------------------------------------------------------------------------------------
data = read.csv("Datasets/synthetic_dataset_with_noise.csv")

x = data[,1]
y = data[,2]

# Create a dataframe with the x and y coordinates
df1 = data.frame(x = x, y = y)

# Plot the data
plot(df1, main = "Synthetic Dataset", xlab = "X", ylab = "Y")

centers = df1[sample(nrow(df1), 3),]

# Apply K-means clustering to dataset
km.result = kmeans(df1, centers = centers)
km.clusters = km.result$cluster

# Plot K-means clusters for dataset
plot(df1, col = km.clusters, main = "K-means Clustering ", xlab = "X", ylab = "Y")
legend("topright", legend = unique(km.clusters), col = unique(km.clusters), pch = 20)

hc.result = hclust(dist(df1),method = 'single')
hc.clusters = cutree(hc.result, k = 3)


plot(df1, col =hc.clusters, main = "Single link Clustering ", xlab = "X", ylab = "Y")
legend("topright", legend = unique(hc.clusters), col = unique(hc.clusters), pch = 20)





