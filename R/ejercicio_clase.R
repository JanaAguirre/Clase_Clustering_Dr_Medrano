#My email: l1medranosoto@ucsd.edu

library(cluster)
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(ape))

#read the data
InputData  <- read.table("ejemplo_clase", header = TRUE, sep = ",", quote="", row.names = 1)

#Run the hierarchical clustering and plot the dendogram
ccom <- hclust(dist(InputData), method = "complete")
plot (ccom, hang = -1)

#Save tree in Newick format
my_tree <- as.phylo(ccom)
write.tree(phy=my_tree, file="ejemplo_clase.tree")

#cut the dendogram such that 3 clusters can be visualized
rect.hclust(ccom, k=3, border=2:4)

#get the data points within each cluster
cls3 <- cutree(ccom, k=3)

#Scatter plot to visualize the data points in each cluster
plot(InputData, xlim=c(0,8), ylim=c(0,8), col=cls3)
fviz_cluster(list(data = InputData, cluster = cls3))

#==================================================================
#Now compare with method single linkage

csin <- hclust(dist(InputData), method = "single")
plot (csin, hang = -1)
rect.hclust(csin, k=3, border=2:4)

dend1 <- as.dendrogram (ccom)
dend2 <- as.dendrogram (csin)

dend_list <- dendlist(dend1, dend2)
tanglegram(dend1, dend2, main = paste("Entanglement =", entanglement(dend_list)))


#==================================================================
# Determine the number of clusters

#Methods: Total Within Sum of Squares (wss), silhouette, gap_stat
fviz_nbclust(InputData, FUN = hcut, hc_method = "ward.D2", method = "wss", k.max = 6) +
  labs(subtitle = "The Elbow Method")

#The silhouette method applied to hclust
fviz_nbclust(InputData, FUN = hcut, hc_func = "hclust", hc_method = "ward.D2", method = "silhouette", k.max = 6) +
  labs(subtitle = "Silhouette method")


