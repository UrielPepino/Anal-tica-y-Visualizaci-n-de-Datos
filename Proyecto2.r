"El Trabajo fue realizado en equipo

  Castro Guillen Yael Michael
  Lozano Trejo Uriel

Lic. En ciencia de Datos"

#Funcion para importar los datos
importarData<-function(){
  #Importamos los datos desde el URL oficial del conjunto de iris
  url<-"https://archive.ics.uci.edu/ml/machine-learning-databases/iris/bezdekIris.data"
  iris<-read.table(url, header=FALSE, sep = "," , dec = ".")
  
  #Cambiamos los encabezados
  encabezados<-c("Largo_Sepalo", "Ancho_Sepalo", "Largo_Petalo", "Ancho_Petalo", "class") 
  colnames(iris)<-encabezados
  
  #visualizamos los datos
  View(iris)
  iris
}

#Funcion para el calculo del area del Sepalo y petalo
areas<-function(iris){
  longitudesS<-iris[, "Largo_Sepalo"]
  anchosS<-iris[, "Ancho_Sepalo"]
  
  longitudesP<-iris[, "Largo_Petalo"]
  anchosP<-iris[, "Ancho_Petalo"]
  
  area_Sepalo<-longitudesS*anchosS
  area_Petalo<-longitudesP*anchosP
  
  iris<-cbind(iris[,1:2], area_Sepalo, iris[,3:4], area_Petalo, iris[5])
  
  View(iris)
  iris
}

#Función para el calculo de las normas
disimilitud<-function(iris){
  setosa<-subset(iris, class=="Iris-setosa")
  versicolor<-subset(iris, class=="Iris-versicolor")
  virginica<-subset(iris, class=="Iris-virginica")
  
  setosa<-setosa[,-7]
  versicolor<-versicolor[,-7]
  virginica<-virginica[,-7]
  
  disimilitud<-NULL
  #Setosa-Versicolor
  mat<-NULL
  #calculamos A-B
  for (i in 1:6) {
    for (j in 1:50) {
        mat<-c(mat, setosa[j,i]-versicolor[j,i])
    }
  }
  setver<-matrix(mat, nrow = 50, ncol = 6)
  
  #Calculamos la norma
  disimilitud<-c(disimilitud, sqrt(sum(setver**2)))
  
  #Calculamos la coorelacion de la dupla
  corrsetver<-cor(setosa, versicolor, method = "spearman")
  View(corrsetver)
  
  #Setosa-Virginica
  mat<-NULL
  
  #calculamos A-B
  for (i in 1:6) {
    for (j in 1:50) {
      mat<-c(mat, setosa[j,i]-virginica[j,i])
    }
  }
  setvir<-matrix(mat, nrow = 50, ncol = 6)
  
  #Calculamos la norma
  disimilitud<-c(disimilitud, sqrt(sum(setvir**2)))
  
  #Calculamos la coorelacion de la dupla
  corsetvirr<-cor(setosa, virginica, method = "spearman")
  View(corsetvirr)

  #Versicolor-Virginica
  mat<-NULL
  #calculamos A-B
  for (i in 1:6) {
   for (j in 1:50) {
     mat<-c(mat, versicolor[j,i]-virginica[j,i])
   }
  }
  versivirgi<-matrix(mat, nrow = 50, ncol = 6)
   
  #Calculamos la norma
  disimilitud<-c(disimilitud, sqrt(sum(versivirgi**2)))
  
  #Calculamos la coorelacion de la dupla
  corversivirgi<-cor(versicolor, virginica, method = "spearman")
  View(corversivirgi)
                  
  #Versicolor-Setosa
  mat<-NULL
  
  #calculamos A-B
  for (i in 1:6) {
    for (j in 1:50) {
      mat<-c(mat, versicolor[j,i]-setosa[j,i])
    }
  }
  versiset<-matrix(mat, nrow = 50, ncol = 6)
  
  #Calculamos la norma
  disimilitud<-c(disimilitud, sqrt(sum(versiset**2))) 
  
  #Calculamos la coorelacion de la dupla
  corversiset<-cor(versicolor, setosa, method = "spearman")
  View(corversiset)
  
  #Virginica-Setosa
  mat<-NULL
  #calculamos A-B
  for (i in 1:6) {
    for (j in 1:50) {
      mat<-c(mat, virginica[j,i]-setosa[j,i])
    }
  }
  virgiset<-matrix(mat, nrow = 50, ncol = 6)

  #Calculamos la norma
  disimilitud<-c(disimilitud, sqrt(sum(virgiset**2)))
  
  #Calculamos la coorelacion de la dupla
  corvirgiset<-cor(virginica, setosa, method = "spearman")
  View(corvirgiset)
  
  #Virginica-Versicolor
  mat<-NULL
  
  #calculamos A-B
  for (i in 1:6) {
    for (j in 1:50) {
      mat<-c(mat, virginica[j,i]-versicolor[j,i])
    }
  }
  virgiversi<-matrix(mat, nrow = 50, ncol = 6)

  #Calculamos la norma
  disimilitud<-c(disimilitud, sqrt(sum(virgiversi**2)))
  
  #Calculamos la coorelacion de la dupla
  corvirgiversi<-cor(virginica, versicolor, method = "spearman")
  View(corvirgiversi)
  
  disimlitudes<-data.frame(rbind(disimilitud))

  rownames(disimlitudes)<-c("Disimilitud")
  
  colnames(disimlitudes)<-c("Setosa-Versicolor","Setosa-Virginica", "Versicolor-Virginica", "Versicolor-Setosa",
            "Virginica-Setosa", "Virginica-Versicolor")
  
  View(disimlitudes)
  View(corrsetver)
  disimlitudes
}

#Función para calcular el producto interno entre cada clase de flores 
porducto_interno<-function(iris){
  
}

setosaVersicolor1<-0
setosaVersicolor2<-0
setosaVersicolor3<-0
setosaVersicolor4<-0

for (i in 1:50) {
  #Sepal Length
  acum1<-iris[i,1]*iris[i+50,1]
  setosaVersicolor1 <- setosaVersicolor1+ acum1
  #Sepal Width
  acum2<-iris[i,2]*iris[i+50,2]
  setosaVersicolor2 <- setosaVersicolor2 + acum2
  #Petal Length
  acum3<-iris[i,3]*iris[i+50,3]
  setosaVersicolor3 <-setosaVersicolor3 + acum3
  #Petal Width
  acum4<-iris[i,4]*iris[i+50,4]
  setosaVersicolor4 <- setosaVersicolor4 + acum4  
  
}
proInterSetosaVersi<-setosaVersicolor1+setosaVersicolor2+setosaVersicolor3+setosaVersicolor4

#Producto interno 
#Setosa Virginica
setosaVirginica1<-0
setosaVirginica2<-0
setosaVirginica3<-0
setosaVirginica4<-0

for (i in 1:50) {
  #Sepal Length
  acum1<-iris[i,1]*iris[i+100,1]
  setosaVirginica1 <- setosaVirginica1 + acum1
  #Sepal Width
  acum2<-iris[i,2]*iris[i+100,2]
  setosaVirginica2 <- setosaVirginica2 + acum2
  #Petal Length
  acum3<-iris[i,3]*iris[i+100,3]
  setosaVirginica3 <- setosaVirginica3 + acum3
  #Petal Width
  acum4<-iris[i,4]*iris[i+100,4]
  setosaVirginica4 <- setosaVirginica4 + acum4  
  
}
proInterSetosaVirginica<-setosaVirginica1+setosaVirginica2+setosaVirginica3+setosaVirginica4

#Producto interno 
#Versicolor-Virginica

versicolorVirginica1<-0
versicolorVirginica2<-0
versicolorVirginica3<-0
versicolorVirginica4<-0

for (i in 1:50) {
  #Sepal Length
  acum1<-iris[i+50,1]*iris[i+100,1]
  versicolorVirginica1 <- versicolorVirginica1 + acum1
  #Sepal Width
  acum2<-iris[i+50,2]*iris[i+100,2]
  versicolorVirginica2 <- versicolorVirginica2 + acum2
  #Petal Length
  acum3<-iris[i+50,3]*iris[i+100,3]
  versicolorVirginica3 <- versicolorVirginica3 + acum3
  #Petal Width
  acum4<-iris[i+50,4]*iris[i+100,4]
  versicolorVirginica4 <- versicolorVirginica4 + acum4  
  
}
proInterversicolorVirginica<-versicolorVirginica1+versicolorVirginica2+versicolorVirginica3+versicolorVirginica4

#####################################################################

#Producto interno 
#Versicolor Setosa 

versicolorsetosa1<-0
versicolorsetosa2<-0
versicolorsetosa3<-0
versicolorsetosa4<-0

for (i in 1:50) {
  #Sepal Length
  acum1<-iris[i+50,1]*iris[i,1]
  versicolorsetosa1 <- versicolorsetosa1 + acum1
  #Sepal Width
  acum2<-iris[i+50,2]*iris[i,2]
  versicolorsetosa2 <- versicolorsetosa2 + acum2
  #Petal Length 
  acum3<-iris[i+50,3]*iris[i,3]
  versicolorsetosa3 <- versicolorsetosa3 + acum3
  #Petal Width
  acum4<-iris[i+50,4]*iris[i,4]
  versicolorsetosa4 <- versicolorsetosa4 + acum4  
  
}
proInterversicolorsetosa<-versicolorsetosa1+versicolorsetosa2+versicolorsetosa3+versicolorsetosa4

#Producto interno 
# Virginica Setosa
virginicasetosa1<-0
virginicasetosa2<-0
virginicasetosa3<-0
virginicasetosa4<-0

for (i in 1:50) {
  #Sepal Length
  acum1<-iris[i+100,1]*iris[i,1]
  virginicasetosa1 <- virginicasetosa1 + acum1
  #Sepal Width
  acum2<-iris[i+100,2]*iris[i,2]
  virginicasetosa2 <- virginicasetosa2 + acum2
  #Petal Length
  acum3<-iris[i+100,3]*iris[i,3]
  virginicasetosa3 <- virginicasetosa3 + acum3
  #Petal Width
  acum4<-iris[i+100,4]*iris[i,4]
  virginicasetosa4 <- virginicasetosa4 + acum4  
  
}
proIntervirginicasetosa<-virginicasetosa1+virginicasetosa2+virginicasetosa3+virginicasetosa4

#Producto interno 
#Virginica Versicolor

virginicaVersicolor1<-0
virginicaVersicolor2<-0
virginicaVersicolor3<-0
virginicaVersicolor4<-0

for (i in 1:50) {
  #Sepal Length
  acum1<-iris[i+100,1]*iris[i+50,1]
  virginicaVersicolor1 <- virginicaVersicolor1 + acum1
  #Sepal Width
  acum2<-iris[i+100,2]*iris[i+50,2]
  virginicaVersicolor2 <- virginicaVersicolor2 + acum2
  #Petal Length
  acum3<-iris[i+100,3]*iris[i+50,3]
  virginicaVersicolor3 <- virginicaVersicolor3 + acum3
  #Petal Width
  acum4<-iris[i+100,4]*iris[i+50,4]
  virginicaVersicolor4 <- virginicaVersicolor4 + acum4  
  
}
proIntervirginicaVersicolor<-virginicaVersicolor1+virginicaVersicolor2+virginicaVersicolor3+virginicaVersicolor4


