#Exemple 3 KNN

download.file("https://resources.oreilly.com/examples/9781784393908/raw/ac9fe41596dd42fc3877cfa8ed410dd346c43548/Machine%20Learning%20with%20R,%20Second%20Edition_Code/Chapter%2003/wisc_bc_data.csv", destfile = "data.csv")
data <- read.csv(file = "data.csv")

# eliminamos la variable ID, ya que puede dar lugar a un sobreajuste ya que identifica cada observación de forma única.
data <- data[,-1]
# Modificamos los nombres de los valores de la variable diagnosis para que sean más claros

# Exploración y preparamos un poco los datos. Necesitaremos las librerias:

library(tidyverse)
library(class)
library(gmodels)
library(caret)

data <- mutate(data,diagnosis = fct_recode(data$diagnosis,"Bening" = "B","Malignant" = "M"))

#Podemos encontrar tanto la cantidad de tumores benignos y malignos como su porporción.
table(data$diagnosis)

round(prop.table(table(data$diagnosis))*100,0)

normalizar  <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}
datan    <- as.data.frame(lapply(data[,-1], normalizar))

#Dividimos la muestra en dos conjuntos, uno para entrenamiento y otro para prueba. 
#Para ello, escoger las primeras 469 para el primero, y las últimas 100 para el segundo, 
#puesto que todas las observaciones de este conjunto de datos ya vienen en orden aleatorio
entrenamiento <- datan[1:469,]
prueba        <- datan[470:569,]

entrenamiento_labels <- data[1:469,1]
prueba_labels        <- data[470:569,1]

pred <- knn(entrenamiento,prueba, cl = entrenamiento_labels, k = 21)

#Veamos que valor de k óptimo encontrariamos con la paqueteria kknn.
train.kknn(entrenamiento_labels ~ ., data = entrenamiento, kmax = 50)

confusionMatrix(data = pred, reference = prueba_labels)

library(kknn)

data_z <- as.data.frame(scale(data[,-1]))
entrenamiento_z <- data_z[1:469,]
prueba_z <- data_z[470:569,]
pred_z <- knn(entrenamiento_z, prueba_z, cl = entrenamiento_labels, k = 21)
confusionMatrix(data = pred_z, reference = prueba_labels)
