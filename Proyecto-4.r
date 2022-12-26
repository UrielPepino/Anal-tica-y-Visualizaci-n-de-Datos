#Proyecto 4
#Castro Guillen Yael Michael
#Lozano Trejo Uriel 

"Considerar los datos económicos de la siguiente tabla (dados en millones de dólares) 
de corporaciones industriales

1. Calcular la distancia de Mahalanobis entre Ford y Exxon
2. Calcular la distancia de Mahalanobis entre General Motors e IBM
3. Calcular la distancia de Mahalanobis entre Philip Morris y Texaco"

#Seleccion de archivo
datos <- file.choose()


main <- function(datos) {
  print(datos)
  data <- read.csv(datos,header = T,sep = ",")
  row.names(data) <- data[,1]
  data <- data[,2:dim(data)[2]]
  View(data)
  media <- means(data)
  print("Media")
  print(media)
  covarianza <- covars(data) 
  print("Covarianza")
  print(covarianza)
  print("Distancia de Mahalanobis entre Ford y Exxon")
  FE_D <- distancia_m(data["Ford",], data["Exxon",], covarianza)
  print(FE_D)
  print("Distancia de Mahalanobis entre General Motors e IBM")
  GMIBM_D <- distancia_m(data["General Motors",], data["IBM",], covarianza)
  print(GMIBM_D)
  print("Distancia de Mahalanobis entre Philip Morris y Texaco")
  MT_D <- distancia_m(data["Philip Morris",], data["Texaco",], covarianza)
  print(MT_D)
}

means <- function(data) {
  col_no <- dim(data)[2]
  row_no <- dim(data)[1]
  means_list <- list()
  for(i in 1:col_no) {
    suma <- 0
    for(j in 1:row_no) {
      suma <- suma + data[j,i]
    }
    means_list <- c(means_list, suma / row_no)
  }
  return(means_list)
}

mult_mat <- function(mat1, mat2) {
  row_no1 <- dim(mat1)[1]
  row_no2 <- dim(mat2)[1]
  col_no1 <- dim(mat1)[2]
  col_no2 <- dim(mat2)[2]
  if(col_no1 == row_no2) {
    new_mat <- data.frame()
    for(a in 1:col_no2) {
      rown <- c()
      for(i in 1:row_no1) {
        suma <- 0
        for(j in 1:col_no1) {
          suma <- suma + (mat1[i,j] * mat2[j,a])
        }
        rown <- c(rown, suma)
      }
      if(dim(new_mat)[1] == 0) {
        new_mat <- cbind(rown)
      } else {
        new_mat <- cbind(new_mat, rown)
      }
    }
    return(new_mat)
  }
}

covars <- function(data) {
  cov_mat <- cov(data)
  return(cov_mat)
}

distancia_m <- function(vector1, vector2, covarianza) {
  dis_vect <- vector1 - vector2
  covars_data_inv <- solve(covarianza)
  dis_vect_covars_inv <- mult_mat(dis_vect, covars_data_inv)
  t_dis_vect <- t(dis_vect)
  mahalanobis_dis <- mult_mat(dis_vect_covars_inv, t_dis_vect)
  return(mahalanobis_dis ** 0.5)
}

main(datos)
