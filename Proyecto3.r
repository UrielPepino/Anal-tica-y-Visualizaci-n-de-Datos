"El Trabajo fue realizado en equipo

  Castro Guillen Yael Michael
  Lozano Trejo Uriel

Lic. En ciencia de Datos"

#Funcion para importar los datos
importarData<-function(){
  #Importamos los datos desde el conjunto de iris
  data("iris")
  
  #Cambiamos los encabezados
  encabezados<-c("Largo_Sepalo", "Ancho_Sepalo", "Largo_Petalo", "Ancho_Petalo", "class") 
  colnames(iris)<-encabezados
  
  #visualizamos los datos
  View(iris)
  iris
} 

iris<-importarData()

# Centroides formado por las medias aritméticas de cada variable
centroides<-function(datos){
  
  n<-nrow(datos)
  m<-ncol(datos)
  centroide<-NULL
  
  for (i in 1:m) {
    centroide<-c(centroide, (sum(datos[i])/n))
  }
  centroide
}

covarianzas<-function(){
  cov(iris[-5])
}

#Calculamos las distancias tomando como base el largo y el ancho del petalo

#Base largo y ancho del petalo
centroide<-centroides(iris[3:4])

colores <- c("blue", "red", "green")
plot(x = iris$Largo_Petalo, y = iris$Ancho_Petalo, col=colores[iris$class],
     xlab = "Largo", ylab = "Ancho", main = "Base: largo y ancho del petalo")
points(centroide[1], centroide[2], pch=16)
legend(x = "topleft", legend = c("Setosa", "Versicolor", "Virginica", "Centroide"), 
       fill = c("blue", "red", "green", "black"), title = "Class")

mcov<-cov(iris[3:4])

mahaP<-mahalanobis(iris[3:4], centroide, mcov)

cat("Las distancia de mahalanobis para cada individuo respecto al largo y ancho del Petalo es: ")
print(mahaP)

#Base largo y ancho del sepalo

centroide2<-centroides(iris[1:2])

colores <- c("blue", "red", "green")
plot(x = iris$Largo_Sepalo, y = iris$Ancho_Sepalo, col=colores[iris$class],
     xlab = "Largo", ylab = "Ancho", main = "Base: largo y ancho del sepalo")
points(centroide2[1], centroide2[2], pch=16)
legend(x = "topleft", legend = c("Setosa", "Versicolor", "Virginica", "Centroide"), 
       fill = c("blue", "red", "green", "black"), title = "Class")

mcov2<-cov(iris[1:2])

mahaS<-mahalanobis(iris[1:2], centroide2, mcov2)

cat("Las distancia de mahalanobis para cada individuo respecto al largo y ancho del Sepalo es: ")
print(mahaS)
