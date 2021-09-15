#cargarpaquetes
install.packages(c("tidyverse","rpart","rpart.plot","caret","e1071","RWekajars","RWeka","FSelector","kknn","naivebayes","lubridate","sqldf","car","readr"))

library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(RWekajars)
library(RWeka)
library(FSelector)
library(kknn)
library(naivebayes)
library(lubridate)
library(sqldf)
library(car)
library(readr)


#Especificamos la ruta donde se encuentra cargado el archivo y donde guardara mis desarrollos.
#Cargamos el archivo como dataframe
setwd("/Users/jbern/Documents/ESTUDIOS/UNAB/DATA MINING/dbs")

#leer el archivo excel
datos <- read.table("alumnosstgo.csv", header=T, sep=",")
attach(datos)

#ver tabla de datos
view(datos)

#CLustering
D <- dist(datos,method = "euclidean", diag=TRUE, upper=FALSE)
d <- hclust(D,method="single")
plot(d)
grupos_clustering <- cutree(d, k = 4)
plot(grupos_clustering)

#K-Means
kmedias <- kmeans(datos[,4:length(datos)], 4,iter.max = 1000)
grupo_kmedias <- kmedias$cluster
plot(grupo_kmedias)

#Comparacion de Clustering vs KMeans
grupos <- cbind(grupos_clustering, grupo_kmedias)
view(grupos)

