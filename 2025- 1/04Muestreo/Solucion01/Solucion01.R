# ------------------------
#       PRIMER PUNTO
# ------------------------
library(tidyverse)
datos <- read.csv(file.choose())
datos <- datos[, -1]
# Función para muestreo aleatorio simple sin reemplazo
MAS <- function(n, marco) { # Inicio función
  pos <- sample(nrow(marco), n, replace = F)
  muestra <- marco[pos, ]
  return(muestra)
} # Función muestreo
# Ingresar: n (tamaño de la muestra; marco: la información muestreo)
muestra <- MAS(5000, datos)
ingreso_muestra <- muestra$I_HOGAR
# ------------------------
#       SEGUNDO PUNTO
# ------------------------
source("Muestreo.R")
objeto_mu <- estimar_parametro(ingreso_muestra, 81961, parametro = "mu", intervalo = TRUE, confianza = 0.95)
mu_estimado <- objeto_mu$Estimador
objeto_tau <- estimar_parametro(ingreso_muestra, 81961, parametro = "tau", intervalo = TRUE, confianza = 0.95)
tau_estimado <- objeto_tau$Estimador
# ------------------------
#       TERCER PUNTO
# ------------------------
objeto_A <- estimar_parametro(muestra$CAR, 81961, parametro = "A", intervalo = TRUE, confianza = 0.95)
objeto_p <- estimar_parametro(muestra$SEC, 81961, parametro = "p", intervalo = TRUE, confianza = 0.95)
# Verificar resultados
carro_poblacion <- datos |> 
  dplyr::group_by(CAR) |> 
  dplyr::reframe(Total = n()) |> 
  dplyr::mutate(Total = Total)
seguro_poblacion <- datos |> 
  dplyr::group_by(SEC) |> 
  dplyr::reframe(Total = n()) |> 
  dplyr::mutate(proporcion = Total/nrow(datos))
# ------------------------
#       CUARTO PUNTO
# ------------------------
objeto_prec <- estimar_parametro(muestra$REC, 81961, parametro = "p")
p_rec <- objeto_prec$Estimador

N <- 81961
D <- ((0.02)^2/qnorm(0.05, lower.tail = FALSE)^2)
n <- (N*p_rec * (1 - p_rec))/((N - 1) * D + p_rec * (1- p_rec))