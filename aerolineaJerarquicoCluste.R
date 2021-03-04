library(cluster)
library(ggplot2)
library(factoextra)
library(clustertend)
library(dendextend)
library(corrplot)
library(NbClust)

# B. Comercialización para viajeros frecuentes.

aerolinea<-read.csv("RFiles/EastWestAirlinesCluster.csv",header = TRUE, sep = ",")

#---Exploración inicial de los datos
str(aerolinea)
View(aerolinea)

set.seed(124)
hopkins(aerolinea,n=nrow(aerolinea)-1) #si es cercano a 0 el dataset es agrupable

#----Preprocesamiento de los datos
sum(is.na(aerolinea))   
aerolinea <- aerolinea[, -c(1)]  #Quitamos la columna id
View(aerolinea)

#---------Con datos normalizados
aerolinea <- scale(aerolinea)


#Determinamos el mejor k
nb<-NbClust(aerolinea,distance="euclidean",min.nc=2,max.nc=10,method="ward.D")

fviz_nbclust(nb)  #3

#Calculo distancia Euclidiana 
dist <- dist(aerolinea, method = "euclidean")

#Agrupamiento jerarquico
hc_ward <- hclust(dist, method = "ward.D")  

#Cortar el arbor 
grp_ward <- cutree(hc_ward, k = 3)
# Numero de observaciones en cada cluste
table(grp_ward)

fviz_dend(hc_ward, k=3, cex = 0.5, 
          k_colors = c("#D81159", "#8F2D56","#218380"),
          color_labels_by_k = TRUE, rect = TRUE)



#-------------Con datos sin normalizar 
#Calculo distancia Euclidiana 
dist <- dist(aerolinea, method = "euclidean")

#Agrupamiento jerarquico
hc_ward <- hclust(dist, method = "ward.D")  


#Cortar el arbor 
grp_ward <- cutree(hc_ward, k = 2)

fviz_dend(hc_ward, k=2, cex = 0.5, 
          k_colors = c("#D81159", "#8F2D56"),
          color_labels_by_k = TRUE, rect = TRUE)

#---------------------------

#--------Comparación de centroides y etiquetado

# Función para encontrar los centroides del grupo
centroid = function(i, dat, groups) 
{
  ind = (groups == i)
  colMeans(dat[ind,])
}

sapply(unique(grp_ward), centroid, aerolinea[,1:11], grp_ward)


table(grp_ward)

dataCluster = aggregate(aerolinea[,1:11],list(grp_ward),median)                      #Se elimina la columna id dataset normal
data.frame(Cluster=dataCluster[,1],Freq=as.vector(table(grp_ward)),dataCluster[,-1])

#Inciso d muestra del 95%
muestra = aerolinea[sample(nrow(aerolinea), nrow(aerolinea)*0.95,replace=FALSE),]

dist_muestra <- dist(muestra, method = "euclidean")
hc_ward_muestra <- hclust(dist_muestra, method = "ward.D")  
grp_ward_m <- cutree(hc_ward_muestra, k = 3)

fviz_dend(hc_ward_muestra, k=3, cex = 0.5, 
          k_colors = c("#D81159", "#8F2D56","#218380"),
          color_labels_by_k = TRUE, rect = TRUE)

#---------------------K-MEANS
aeroMeans <- kmeans(aerolinea, 3, nstart = 25)
print(aeroMeans)


# Cluster size
aeroMeans$size

# Cluster means
aeroMeans$centers

fviz_cluster(aeroMeans, data = aerolinea,
             palette = c("#D81159", "#8F2D56","#218380"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

kmeansAero <- eclust(aeroMeans, "kmeans", k = 3, nstart = 25, graph = TRUE)