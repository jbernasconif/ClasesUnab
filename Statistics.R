-#cargarpaquetes
  install.packages(c("tidyverse","rpart","rpart.plot","caret","e1071","RWekajars","RWeka","FSelector","kknn","naivebayes"))

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

#Especificamos la ruta donde se encuentra cargado el archivo y donde guardaré mis desarrollos.
#Cargamos el archivo como dataframe
setwd("/Users/jbern/Documents/ESTUDIOS/UNAB/DATA MINING/dbs")

#leet el archivo excel
datos <- read.table("delident.csv", header=T, sep=",")

#ver tabla de datos
view(datos)

#mutar tipo de datos a factor
datos <- datos %>% 
  mutate_at("Delivery", factor)

#datos de entrenamiento
nrow(datos)
set.seed(nrow(datos))
datos_entrenamiento <- sample_frac(datos, .8)
datos_prueba <- setdiff(datos, datos_entrenamiento)

#entrenamiento
clasificadelivery <- rpart(Delivery~ ., datos_entrenamiento)

#plotear el arbol de clasificacion
rpart.plot(clasificadelivery, main="Arbol de clasificacion para deliverys en delident")

#prediccion
datos_prueba$pred <- predict(clasificadelivery, datos_prueba, type="class")

#generacion de la matriz de confusion
matrizconfusion <- confusionMatrix(datos_prueba$pred, datos_prueba[["Delivery"]])
#graficar matriz de confusion
plot(matrizconfusion[["table"]], main="Matriz de Confusionn")
#ganancia de informacion
ig <- information.gain(Delivery~., datos)

#modelo k_vecino mas cercano
modeloentrenamiento <- train.kknn(Delivery~ .,datos_entrenamiento, kmax=8)
#prediccion
datos_prueba$pred <- predict(modeloentrenamiento, datos_prueba)
#matriz de confusion kknn
matrizkknn <- confusionMatrix(datos_prueba$pred, datos_prueba[["Delivery"]])
#plotear matriz
plot(matrizconfusion[["table"]], , main="Matriz de Confusion")

#MODELO BAYESIANO
modeloentrenamiento <- naive_bayes(Delivery~ .,datos_entrenamiento)
#Prediccion
datos_prueba$pred <- predict(modeloentrenamiento, datos_prueba)
#matriz bayesiana
matrizbayes <- confusionMatrix(datos_prueba$pred,datos_prueba[["Delivery"]])
