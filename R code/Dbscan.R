library(cluster)
library(fpc)


data = read.csv("Datasets/synthetic_dataset.csv")

db <- dbscan(data, eps = 0.6 ,MinPts = 6)
plot(data, col=db$cluster+1, main="DBSCAN")


data = read.csv("Datasets/synthetic_dataset_with_noise.csv")

db <- dbscan(data, eps = 0.6 ,MinPts = 6)
plot(data, col=db$cluster+1, main="DBSCAN")


data = read.csv("Datasets/circle_dataset.csv")

db <- dbscan(data, eps=0.08,MinPts=4)
plot(data$X ,data$Y, col=db$cluster, main="DBSCAN" , xlab = "X", ylab = "Y")


