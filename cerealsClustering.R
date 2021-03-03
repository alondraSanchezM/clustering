#install.packages("clustertend")
library(cluster)
library(ggplot2)
library(factoextra)


#A. Calificación de clientes de cereales para el desayuno

dataCereales<-read.csv("RFiles/Cereals.csv",header = TRUE, sep = ",")

#---Exploración inicial de los datos
str(dataCereales)
View(dataCereales)
summary(dataCereales)

#----Preprocesamiento de los datos
sum(is.na(dataCereales))   
dataCereales <- dataCereales[complete.cases(dataCereales),]
sum(is.na(dataCereales))   

rownames(dataCereales) <- dataCereales$name                               #Conversion de nombres de cereales a las 
dataCereales <- dataCereales[, -c(colnames(dataCereales) %in% ("name"))]  #filas para mejor visualización de clusters.
View(dataCereales)

dataCereales <- dataCereales[, -c(1:2)]  
View(dataCereales)
dataCereales <- scale(dataCereales)

#---Evaluación de tendencia
library(clustertend)
set.seed(124)
hopkins(dataCereales,n=nrow(dataCereales)-1) #si es cercano a 0 el dataset es agrupable

#---Calcular estabilidad de los datos
muestra<- dataCereales[sample(nrow(dataCereales), nrow(dataCereales)*0.95), ]
muestra
hopkins(muestra,n=nrow(muestra)-1)


#---Calculo distancia Euclidiana 
dist <- dist(dataCereales, method = "euclidean")

#---Agrupamiento jerarquico
hc_simple <- hclust(dist, method = "single")     #enlace unico    5
hc_simple
plot(hc_simple)
fviz_dend(hc_simple, cex  =0.5)
hc_complete <- hclust(dist, method = "complete") #enlace completo  6
hc_complete
plot(hc_complete)
fviz_dend(hc_complete, cex  =0.5)

#---Comparamos los dendogramas de ambos metodos
#install.packages("dendextend")
library(dendextend)
dend1<-as.dendrogram(hc_simple)
dend2<-as.dendrogram(hc_complete)
 
dend_list<-dendlist(dend1,dend2) #Se crea la lista de dendogramas.

#tanglegram(dend1,dend2)
tanglegram(dend1,dend2,
           highlight_distinct_edges=FALSE,
           common_subtrees_color_lines=FALSE,
           common_subtrees_color_branches=TRUE,
           main=paste("entanglement=",
                      round(entanglement(dend_list),2))
)


#---Determinamos si los dendogramas son similares o no
cor.dendlist(dend_list,method="cophenetic")

#Si su relación es más cercana a 0 que a 1  significa que no son estadisticamente similares.
#En este caso son medianamente similares

#---Comparamos los dos metodos
dend_list<-dendlist("Single"=dend1,"Complete"=dend2)
cors<-cor.dendlist(dend_list)
round(cors,2)

#install.packages("corrplot")
library(corrplot)
corrplot(cors,"pie","lower")

#---Calcular las distancias cofenéticas para checar que
#arbol es mejor >.75 son buenos
coph_simple <- cophenetic(hc_simple)
cor(dist,coph_simple)  #calcular corelación de distancias

coph_complete <- cophenetic(hc_complete)
cor(dist,coph_complete) 

hc_average <- hclust(dist, method = "average")   #cluster con enlcae average es el mejor en este caso
coph_ave <- cophenetic(hc_average)
cor(dist,coph_ave) 

#----Calcular k 
#install.packages("NbClust")
library("NbClust")
fviz_nbclust(dataCereales,hcut,method = "silhouette")
#NbClust(dataCereales,distance="euclidean",min.nc=2,max.nc=10, method="ward.D")

#--Cortar el arbor observar centroides
grp_simple <- cutree(hc_simple, k = 10)
table(grp_simple)
rownames(dataCereales)[grp_simple=1] #Obtener los nombres para los miembros del cluster 1

fviz_dend(hc_simple, k=10, cex = 0.5, 
          k_colors = c("#D81159", "#8F2D56","#218380","#FBB13C", "#73D2DE","#5F1F30", "#9C0D38","#BE7C4D","#82A7A6", "#F06543"),
          color_labels_by_k = TRUE, rect = TRUE)

grp_complete <- cutree(hc_average, k = 10)
table(grp_complete)
rownames(dataCereales)[grp_complete=1] 

fviz_dend(hc_average, k=10, cex = 0.5, 
          k_colors = c("#D81159", "#8F2D56","#218380","#FBB13C", "#73D2DE","#5F1F30", "#9C0D38","#BE7C4D","#82A7A6", "#F06543"),
          color_labels_by_k = TRUE, rect = TRUE)

#---observar centroides
cluster_Simple<-as.matrix(grp_simple)
aggregate(dataCereales,by=list(cluster_Simple),median)
dataCereales_simple <- cbind(dataCereales, cluster = cluster_Simple)
head(dataCereales_simple)

cluster_Complete<-as.matrix(grp_complete)
aggregate(dataCereales,by=list(cluster_Complete),median)
dataCereales_complete <- cbind(dataCereales, cluster = cluster_Complete)
head(dataCereales_complete)

fviz_cluster(list(data = dataCereales, cluster = grp_simple))
fviz_cluster(list(data = dataCereales, cluster = grp_complete))

#---Visualuzar estadisticas por grupo
View(dataCereales_simple)
dataCereales_simple <- as.data.frame(dataCereales_simple) 

summary(dataCereales_simple[dataCereales_simple$V14==1,]) 
summary(dataCereales_simple[dataCereales_simple$V14==2,]) 
summary(dataCereales_simple[dataCereales_simple$V14==3,]) 
summary(dataCereales_simple[dataCereales_simple$V14==4,]) 
summary(dataCereales_simple[dataCereales_simple$V14==5,]) 
summary(dataCereales_simple[dataCereales_simple$V14==6,]) 
summary(dataCereales_simple[dataCereales_simple$V14==7,]) 
summary(dataCereales_simple[dataCereales_simple$V14==8,]) 
summary(dataCereales_simple[dataCereales_simple$V14==9,]) 
summary(dataCereales_simple[dataCereales_simple$V14==10,])

#---Visualizar conjunto 
dataCereales_simple[dataCereales_simple$V14==1,]
dataCereales_simple[dataCereales_simple$V14==2,]
