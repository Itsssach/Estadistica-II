# ------------------------
#       PRIMER PUNTO
# ------------------------
library(tidyverse)
# Función para muestreo aleatorio simple sin reemplazo
MAS <- function(n, marco) { # Inicio función
  pos <- sample(nrow(marco), n, replace = F)
  muestra <- marco[pos, ]
  return(muestra)
} # Función muestreo
muestra <- MAS(5000, datos)
ingreso_muestra <- muestra$I_HOGAR
# ------------------------
#       SEGUNDO PUNTO
# ------------------------
source("Muestreo.R")
estimar_parametro(ingreso_muestra, 81961, parametro = "tau", intervalo = TRUE)
estimar_parametro(ingreso_muestra, 81961, parametro = "mu", intervalo = TRUE)
# ------------------------
ingreso_promedio <- mean(datos$I_HOGAR)
# ------------------------
#       TERCER PUNTO
# ------------------------
carro_muestra <- muestra$CAR
seguro_muestra <- muestra$SEC

estimar_parametro(carro_muestra, 81961, parametro = "p", intervalo = TRUE)
estimar_parametro(seguro_muestra, 81961, parametro = "A", intervalo = TRUE)
# ------------------------
carro_poblacion <- datos |> 
  dplyr::group_by(CAR) |> 
  dplyr::reframe(Total = n()) |> 
  dplyr::mutate(proporcion = Total/nrow(datos))
seguro_poblacion <- datos |> 
  dplyr::group_by(SEC) |> 
  dplyr::reframe(Total = n()) |> 
  dplyr::mutate(proporcion = Total/nrow(datos))
# ------------------------
#       CUARTO PUNTO
# ------------------------
alpha <- 0.05
D <- ((0.02)^2/(qnorm(1- alpha/2)^2))
n_minimo <- (81961 * 0.6 * (1- 0.6))/((81961 - 1) * D + 0.6 * (1 - 0.6))
