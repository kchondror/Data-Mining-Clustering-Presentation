library(cluster)
library(factoextra)
library(ggplot2)


df = read.csv("synthetic_dataset.csv")

#create plot of number of clusters vs total within sum of squares
fviz_nbclust(df, kmeans, method = "wss")


df = read.csv("circle_dataset.csv")

fviz_nbclust(df, hcut, method = "silhouette")
fviz_nbclust(df, hcut, method = "gap_stat")