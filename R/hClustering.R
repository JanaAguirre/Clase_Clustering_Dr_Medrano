#My email: l1medranosoto@ucsd.edu

library(cluster)
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(ape))


#read the data
#InputData  <- read.table("3_clear_clusters_2vars.csv", header = TRUE, sep = ",", quote="", row.names = 1)
#InputData  <- read.table("3_ovlp_clusters_2vars.csv", header = TRUE, sep = ",", quote="", row.names = 1)
InputData  <- read.table("no_clusters_2vars.csv", header = TRUE, sep = ",", quote="", row.names = 1)


#==================================================================
#build the dendogram whith hclust

hClusters <- hclust(dist(InputData), method = "complete")
coeff <- coef(hClusters)
plot (hClusters, hang = -1, main = "hclust Dendogram")
cls3 <- cutree(hClusters, k=3)

#cut the dendogram such that 3 clusters are produced
rect.hclust(hClusters, k=3, border=2:4)
fviz_cluster(list(data = InputData, cluster = cls3))


#==================================================================
#Build the dendogram with agnes

aClust <- agnes(InputData, method = "complete")
pltree(aClust, cex = 0.6, hang = -1, main = "agnes Dendrogram")
aCoeff <- aClust$ac
rect.hclust(as.hclust(aClust), k=3, border=2:4)
aCls3 <- cutree(as.hclust(aClust), k = 3)
fviz_cluster(list(data = InputData, cluster = aCls3)

#==================================================================
#Build the dendogram with Diana

dClust <- diana(table)
pltree(dClust, cex = 0.6, hang = -1, main = "Diana Dendrogram")
dCoeff <- dClust$dc
rect.hclust(as.hclust(dClust), k=3, border=2:4)
dCls3 <- cutree(as.hclust(dClust), k = 3)

fviz_cluster(list(data = InputData, cluster = aCls3))


#==================================================================
#Compare two dendograms (slow method)

#dend1 <- as.dendrogram (hClusters)
#dend2 <- as.dendrogram (as.hclust(aClust))

#dend_list <- dendlist(dend1, dend2)
#tanglegram(dend1, dend2, main = paste("Entanglement =", entanglement(dend_list)))

#==================================================================
# Determine the number of clusters

#Methods: Total Within Sum of Squares (wss), silhouette, gap_stat
fviz_nbclust(InputData, FUN = hcut, hc_method = "ward.D2", method = "wss", k.max = 10) +
  labs(subtitle = "The Elbow Method")

#The silhouette method applied to hclust
fviz_nbclust(InputData, FUN = hcut, hc_func = "hclust", hc_method = "ward.D2", method = "silhouette", k.max = 7) +
  labs(subtitle = "Silhouette method")
