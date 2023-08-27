#Exemple 1 KNN

trabajo       <- c(10,4,6,7,7,6,8,9,2,5,6,5,3,2,2,1,8,9,2,7)
examen        <- c(9,5,6,7,8,7,6,9,1,5,7,6,2,1,5,5,9,10,4,6)

# Participación en la clase, esta en una escala de 1 a 3 (1=máximo, 2=medio,3= mínimo)
participacion <- c(1,2,1,1,1,2,2,1,3,3,3,2,3,3,2,2,1,1,3,3)
tabla <-data.frame(trabajo,examen,participacion)
str(tabla)

head(tabla)

plot(tabla[,1:2],main="Relación entre trabajo en clase y examen 1",xlab="Trabajo en clase", ylab="Examen 1",col=tabla$participacion,pch=19)
legend("topright",legend=c("1","2","3"),pch=19,col=c(1,2,3))

nuevos <- data.frame(trabajo=c(2,9),examen=c(3,8))

plot(tabla[,1:2],xlab="Trabajo en clase", ylab="Examen 1",col=tabla$participacion,pch=19)
legend("topright",legend=c("1","2","3"),pch=19,col=c(1,2,3))
points(nuevos,col="blue",pch=8,lwd=2)

library(class)
modelo <-knn(train = tabla[,-3], test=nuevos, cl = tabla$participacion, k=3)
modelo

modelo1 <-knn(train = tabla[,-3], test=nuevos, cl = tabla$participacion, k=5)
modelo1