#My email: l1medranosoto@ucsd.edu

library(cluster)
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(ape))

#==================================================================
#Generate random data to cluster

data <- rbind(matrix(rnorm(50000, mean =  5, sd = 1), ncol = 2),
              matrix(rnorm(300, mean = 10,  sd = 1), ncol = 2),
              matrix(rnorm(300, mean = 15,  sd = 1), ncol = 2))
colnames(data) <- c("x", "y")


#==================================================================
#Run kmeans and generate scatter plot

#centroids <- rbind(c(4.347548, 4.716311),c(6.067601, 4.498569),c(4.829717, 6.285432), c(27.462508, 27.563207))
#centroids <- rbind(c(4.347548, 4.716311),c(6.067601, 4.498569),c(27.462508, 27.563207))
km <- kmeans(data, centers=3, nstart = 25)
plot(data, col = km$cluster)
points(km$centers, col = 2:4, pch = 8, cex=3)


#==================================================================
#compare with hclust


hc  <- hclust(dist(data), method = "complete")
coeff <- coef(hc)
cls3 <- cutree(hc, k=3)
plot(data, col = cls3)

plot (hc, hang=-1)
rect.hclust(hc, k=3, border=2:4)




#==================================================================
# Determine the number of clusters

#Methods: Total Within Sum of Squares (wss), silhouette, gap_stat
fviz_nbclust(data, FUN = hcut, hc_method = "ward.D2", method = "wss", k.max = 5) +
  labs(subtitle = "The Elbow Method")

#The silhouette method applied to hclust
fviz_nbclust(data, FUN = hcut, hc_func = "hclust", hc_method = "ward.D2", method = "silhouette", k.max = 7) +
  labs(subtitle = "Silhouette method")

#The gap statistic method applied to k-means
fviz_nbclust(data, FUN = kmeans, k.max = 10, nstart = 20,  method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap statistic method")
