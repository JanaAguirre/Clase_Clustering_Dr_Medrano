# Importando el dataframe del blastp
df <- read.delim("~/Clase_Clustering_Dr_Medrano/Project_Clustering/dataframe_h.txt", header=FALSE)

# Cambiando el nombre de las columnas
colnames(df) <- c( V1= "prot_origin", V2= "to_protein", V3= "bit_scores")

# Creando la matriz
matx <- df
  matx <- spread(matx,key=to_protein, value=bit_scores)
  matx <- column_to_rownames(matx, "prot_origin")
  matx <- as.matrix(matx)
