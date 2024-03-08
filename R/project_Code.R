# Importando el dataframe del blastp
df <- read.delim("~/Clase_Clustering_Dr_Medrano/Project_Clustering/dataframe_h.txt", header=FALSE)

# Cambiando el nombre de las columnas
colnames(df) <- c( V1= "prot_origin", V2= "to_protein", V3= "bit_scores")

# Creando la matriz
library(tidyr)
library(textshape)
matx <- df
  matx <- spread(matx,key=to_protein, value=bit_scores)
  matx <- column_to_rownames(matx, "prot_origin")
  matx <- as.matrix(matx)
# Normalización de bitscores
  matx <- matx/max(matx)
  matx <- 1-matx
  matx <- as.dist(matx)
  matx_df <- as.matrix(matx)

# Visualización gráfica de matriz de distancias

  library("pheatmap")
  pheatmap(
    matx_df,
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    show_rownames = TRUE,
    show_colnames = TRUE,
    display_numbers = TRUE,
    fontsize_row = 6,
    fontsize_col = 6,
    fontsize_number = 6
  )
# Clustering jerárquico

  library(cluster)
  suppressPackageStartupMessages(library(factoextra))
  suppressPackageStartupMessages(library(dendextend))
  library(Rmisc)
  library(lattice)
  library(plyr)

# PCA
  pca_resultados <- prcomp(matx_df)
  biplot(pca_resultados, choices = c(1,18))
# Determinar el número de clusters con Elbow y Silhouette

# Elbow
  fviz_nbclust(matx_df, FUN = hcut, hc_method = "average", method = "wss", k.max = 5) +
    labs(subtitle = "The Elbow Method")
  fviz_nbclust(matx_df, FUN = hcut, hc_method = "complete", method = "wss", k.max = 5) +
    labs(subtitle = "The Elbow Method")
  fviz_nbclust(matx_df, FUN = hcut, hc_method = "single", method = "wss", k.max = 5) +
     labs(subtitle = "The Elbow Method")
  fviz_nbclust(matx_df, FUN = hcut, hc_method = "average", method = "wss", k.max = 5) +
     labs(subtitle = "The Elbow Method")
# Silhouette
   fviz_nbclust(matx_df, FUN = hcut, hc_func = "hclust", hc_method = "ward.D2", method = "silhouette", k.max = 7) +
    labs(subtitle = "Silhouette method")
   fviz_nbclust(matx_df, FUN = hcut, hc_func = "hclust", hc_method = "complete", method = "silhouette", k.max = 7) +
     labs(subtitle = "Silhouette method")
   fviz_nbclust(matx_df, FUN = hcut, hc_func = "hclust", hc_method = "average", method = "silhouette", k.max = 7) +
     labs(subtitle = "Silhouette method")
   fviz_nbclust(matx_df, FUN = hcut, hc_func = "hclust", hc_method = "single", method = "silhouette", k.max = 7) +
     labs(subtitle = "Silhouette method")

# Clustering jerarquico utilizando el método complete,
# single-linkage, average y ward. Elección de método

  ccom <- hclust(dist(matx), method = "complete")
  csing <- hclust(dist(matx), method = "single")
  cav <- hclust(dist(matx), method = "average")
  cward <- hclust(dist(matx), method = "ward.D2")

  par(mfrow = c(1,4))
  plot (ccom, hang = -1)
  plot (cward, hang = -1)
  plot (csing, hang = -1)
  plot (cav, hang = -1)

#Dendrogramas resultantes de cada clustering jerárquico

# Comparación de agnes y hclust
  #Método complete

  aClust <- agnes(matx, method = "complete")
  pltree(aClust, cex = 0.6, hang = -1, main = "agnes complete Dendrogram")

  plot (ccom, hang = -1)
  #Método ward

  dClust <- agnes(matx, method = "ward")
  pltree(dClust, cex = 0.6, hang = -1, main = "agnes ward.D2 Dendrogram2")

  plot (cward, hang = -1)

  #Scatter plot para visualizar los puntos en cada cluster

  cls3 <- cutree(ccom, k=3)
  fviz_cluster(list(data = matx_df, cluster = cls3))

  cls3_ward <- cutree(cward, k=3)
  fviz_cluster(list(data = matx_df, cluster = cls3_ward))

  # Corte de los dendogramas en el número de clusters identificados
  rect.hclust(ccom, k=2, border=2:4)
  rect.hclust(csing, k=3, border=2:4)
  rect.hclust(cav, k=3, border=2:4)
  rect.hclust(cward, k=3, border=2:4)

# Guardar los dendogramas en un formato Newick
  my_tree <- as.phylo(ccom)
  write.tree(phy=my_tree, file="ccom_hclust.tree")
  my_tree <- as.phylo(csing)
  write.tree(phy=my_tree, file="csing_hclust.tree")
  my_tree <- as.phylo(cav)
  write.tree(phy=my_tree, file="cav_hclust.tree")
  my_tree <- as.phylo(cward)
  write.tree(phy=my_tree, file="cward_hclust.tree")

# "Agglomerative coefficients"
  coeff_ccom <- coef(ccom)
  coeff_csing <- coef(csing)
  coeff_cav <- coef(cav)
  coeff_cward <- coef(cward)

dev.off()
