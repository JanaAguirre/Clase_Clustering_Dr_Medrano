library('ggplot2')
library('dplyr')
library('tidyr')

# identify clusters
clusters <- read.table('kmeans20.txt')[,2]

# read data
data <- read.table("datos_lcg.txt", header = TRUE, sep = ",", quote="")
clusters <- cutree(cwar, k=30)

clusters <- as.data.frame(clusters)
cluster_bnumbers <- rownames(clusters)
data$bnumber <- row.names(data)
data <- melt(data)
data <- merge (data, as.data.frame(clusters))
head(data)

cluster = 10
cluster_data <- select(data[data$cluster==cluster,],-cluster)
cluster_data <- gather(cluster_data[,-1], key="condition", value="values")
cluster_data$bnumber <- cluster_bnumbers
#head(cluster_data)

ggplot(cluster_data, aes(x=condition, y=values, color=bnumber, group=bnumber)) +
  geom_line()
)
cwar <- hclust(dist(InputData, method = "euclidean"), method = "ward.D2")
