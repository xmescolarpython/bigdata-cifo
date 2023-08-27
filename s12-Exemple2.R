#Exemple 2 KNN

data <-iris
head(data)

# Mediante un muestreo aleatorio, separamos el conjunto de entrenamiento y en cojunto de prueba.
#Supongamos 100 observaciones de entrenamiento y 50 de prueba.

set.seed(2020)
muestra       <- sample(1:150, 100)
entrenamiento <- data[muestra,]
prueba        <- data[-muestra,]
dim(entrenamiento)[1]

dim(prueba)#[1]

# Construimos el modelo ajustado con los datos de aentrenamiento. 
# Para este ejemplo utilizaremos la libreria kknn. En esta paquetería se usa la función `train.kknn, 
# a la cual se le debe indicar el valor máximo de k que el modelo puede usar y él determina el valor óptimo.

library('kknn')

modelo <- train.kknn(Species ~ ., data = entrenamiento, kmax = 9)
modelo

# Notemos que encuentra como valor óptimo k=5 vacinos. 
# Podemos correr el modelo con los datos de entrenamiento para revisar el margen de error.
entre <- predict(modelo, entrenamiento[,-5])
tt  <- table(entrenamiento[,5],entre)
tt

# Podemos observar en la tabla, en las filas la clasificación real y en las columnas la predicción. 
# La precisión del modelo respecto a las observaciones de entrenamiento sería:

precision <- (sum(diag(tt)))/sum(tt)
precision

# Podemos hablar de una precisión del 98% o de un error del 2%
# Para analizar la calidad del modelo, se podrían construir las curvas ROC aprendidas en la sección anterior. 
# Ahora, veamos que tan buena es la predicción para el conjunto de prueba.
pred  <- predict(modelo, prueba[,-5])
table <- table(prueba[,5],pred)
table

clas  <- (sum(diag(table)))/sum(table)
clas
