library(base)


# Function that calculates the euclidean distance
euclidean <- function(a, b) sqrt(sum((a - b)^2))



# Read data.
M = read.csv("synthetic_dataset.csv")

#// Maximin algorithm //
# Set the number of clusters, a counter for them and list for the vectors. 
k <- 4
numOfCenters <- 1
centroids <- list()

# Set the initial centroid to a random vector from the dataset
random <- M[sample(nrow(M), 1), ]
centroids[[numOfCenters]] <- c(random$x , random$y)


# Find the vector that is furthest away from the previous centroid
maxDist <- -1
for (i in 1:200) {
  j <- unlist(centroids[numOfCenters])
  i <- as.numeric(M[i,])
  dist <- euclidean(i,j)
  if (dist > maxDist) {
    maxDist <- dist
    maxDistVector <- c(i)
  }
}

# Add the vector with the maximum distance to the list of centroids
numOfCenters <- numOfCenters + 1
centroids[[numOfCenters]] <- maxDistVector


# Find the maximum of the minimum distances of the remaining vectors 
# to the previous two centroids
while (numOfCenters < k) {
  
  x=0;y=0;dist=as.numeric(0);
  vector<-as.data.frame(cbind(x,y)) 
  dist = as.numeric(dist)  
  minDist<-as.data.frame(cbind((vector),dist))   
  
  for (i in 1:200) {
    mins <- list()
    
    for (j in centroids) {
      mins <- c(mins, euclidean(M[i,],j))
    }
    
    minDist[nrow(minDist) + 1,] = c(M[i,], Reduce(min, mins))
  }
  
  maxDistVector <- c(minDist[which.max(minDist$dist),]$x , minDist[which.max(minDist$dist),]$y )
  
  numOfCenters <- numOfCenters + 1
  centroids[[numOfCenters]] <- maxDistVector
}
print(centroids)




