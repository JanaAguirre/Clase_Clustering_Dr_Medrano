#My email: l1medranosoto@ucsd.edu

library(cluster)
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(ape))

#read the data
InputData  <- read.table("datos_lcg.txt", header = TRUE, sep = ",", quote="", row.names = 1)


#set a plotting area of three horizontally contiguous panels
par(mfrow = c(1, 4))


#set margins for the plotting area
par(mar = c(2, 2, 2, 1) + 0.1)


#build the dendogram
csin <- hclust(dist(InputData, method = "euclidean"), method = "single")
cave <- hclust(dist(InputData, method = "euclidean"), method = "average")
ccom <- hclust(dist(InputData, method = "euclidean"), method = "complete")
cwar <- hclust(dist(InputData, method = "euclidean"), method = "ward.D2")



#plot the dendogram, with labels hanging down from zero
plot (csin, hang = -1, main = "Single")
rect.hclust(csin, k=20,  border=1:16)
csin20 <- cutree(csin, k=20)

plot (cave, hang = -1, main = "Average")
rect.hclust(cave, k=20,  border=1:16)
cave20 <- cutree(cave, k=20)

plot (ccom, hang = -1, main = "Complete")
rect.hclust(ccom, k=20,  border=1:16)
ccom20 <- cutree(ccom, k=20)

plot (cwar, hang = -1, main = "Ward.D")
rect.hclust(cwar, k=20,  border=1:16)
cwar20 <- cutree(cwar, k=20)


#Initialize random number generator
set.seed(123)


#Run K-means and save clusters to a file
km_Clusters <- kmeans(InputData, centers=20, nstart=25)
write.table(km_Clusters$cluster, file = "kmeans20.txt", quote = FALSE, row.names = TRUE, col.names = FALSE)



#THe elbow method applied to k-means
fviz_nbclust(InputData, FUN = kmeans, method = "wss", k.max = 20, nstart = 25) +
  labs(subtitle = "Elbow method")

#The silhouette method applied to k-means
fviz_nbclust(InputData, FUN = kmeans, method = "silhouette", k.max = 20, nstart = 25) +
  labs(subtitle = "Silhouette method")

#The gap statistic method applied to k-means
fviz_nbclust(InputData, FUN = kmeans, k.max = 20, nstart = 25,  method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap statistic method")

#The silhouette method applied to hclust
fviz_nbclust(InputData, FUN = hcut, hc_func = "hclust", hc_method = "ward.D2", method = "silhouette", k.max = 20) +
  labs(subtitle = "Silhouette method")

#The silhouette method applied to agnes
fviz_nbclust(InputData, FUN = hcut, hc_func = "agnes", hc_method = "ward.D2", method = "silhouette", k.max = 20) +
  labs(subtitle = "Silhouette method")

#THe Elbow method applied to diana
fviz_nbclust(InputData, FUN = hcut, hc_func = "diana", hc_method = "ward.D2", method = "wss", k.max = 20) +
  labs(subtitle = "Elbow method")


