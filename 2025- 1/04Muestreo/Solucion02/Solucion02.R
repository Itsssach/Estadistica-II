# -----------------------
#       PRIMER PUNTO
# -----------------------
# Determinar afijación y MAE
library(tidyverse)
# -----------------------
datos <- read.csv(file.choose())
# INFORMACIÓN PARA MUESTREO ESTRATIFICADO
I_ESTRATO <- datos |> 
  dplyr::group_by(ING) |> 
  dplyr::reframe(I_HOGAR = mean(I_HOGAR), Total = n()) |> 
  dplyr::mutate(Relativa = Total/sum(Total))
# -----------------------
# FILTRAR POR ESTRATO
ESTRATO1 <- datos |> 
  dplyr::select(everything()) |> 
  dplyr::filter(ING == 1)
ESTRATO2 <- datos |> 
  dplyr::select(everything()) |> 
  dplyr::filter(ING == 2)
ESTRATO3 <- datos |> 
  dplyr::select(everything()) |> 
  dplyr::filter(ING == 3)
# -----------------------
# Determinar la muestra por estrato
MAS <- function(n, marco) { # Inicio función
  pos <- sample(nrow(marco), n, replace = F)
  muestra <- marco[pos, ]
  return(muestra)
} # Función muestreo
# -----------------------
# Tamaños de población estratificados
N_1 <- 39294; N_2 <- 39157; N_3 <- 3510
N_i <- c(N_1, N_2, N_3) # Vector de poblaciones
N <- sum(N_i) # Tamaño población (general)
# -----------------------
# Determinar el factor de afijación proporcional
psi_i <- N_i/N
# Determinar un tamaño de muestra global
n <- 8000;
n_i <- psi_i * n
# Solución al problema de exactitud
n_1 <- 3836; n_2 <- 3822; n_3 <- 342
n_i <- c(n_1, n_2, n_3)
# -----------------------
# Afijación de Neymann
sigma_est1 <- sd(ESTRATO1$I_HOGAR)
sigma_est2 <- sd(ESTRATO2$I_HOGAR)
sigma_est3 <- sd(ESTRATO3$I_HOGAR)
sigma_i <- c(sigma_est1, sigma_est2, sigma_est3)
# -----------------------
n_iNeymann <- (N_i * sigma_i)/(sum(N_i * sigma_i)) * n
# Proceso de muestreo, con la función MAS y empleando
# una afijación proporcional
# -----------------------
ESTRATO1_muestra <- MAS(3836, ESTRATO1)
ESTRATO2_muestra <- MAS(3822, ESTRATO2)
ESTRATO3_muestra <- MAS(342, ESTRATO3)
# -----------------------
#      SEGUNDO PUNTO
# -----------------------
source("Muestreo2.R")
# Guarda todas las muestras para cada estrato
datos_muestrales <- list(estrato1 = ESTRATO1_muestra$I_HOGAR, 
                         estrato2 = ESTRATO2_muestra$I_HOGAR, 
                         estrato3 = ESTRATO3_muestra$I_HOGAR)
# Guarda todos los tamaños poblacionales por estrato
info_poblacion <- c(estrato1 = N_1, 
                       estrato2 = N_2,
                       estrato3 = N_3)
# Estimar el tau
objeto_tau <- estimar_parametro(datos_muestrales, 
                                info_poblacion, 
                                parametro = "tau", 
                                intervalo = TRUE, 
                                confianza = 0.95)
# -----------------------
objeto_mu <- estimar_parametro(datos_muestrales,
                               info_poblacion, 
                               parametro = "mu", 
                               intervalo = TRUE,
                               confianza = 0.95)
# -----------------------
#      TERCER PUNTO
# -----------------------
# Los hogares que poseen automóvil
datos_muestrales <- list(estrato1 = ESTRATO1_muestra$CAR, 
                         estrato2 = ESTRATO2_muestra$CAR, 
                         estrato3 = ESTRATO3_muestra$CAR)
# Guarda todos los tamaños poblacionales por estrato
info_poblacion <- c(estrato1 = N_1, 
                    estrato2 = N_2,
                    estrato3 = N_3)
objeto_A <- estimar_parametro(datos_muestrales,
                              info_poblacion, 
                              parametro = "p",
                              intervalo = TRUE,
                              confianza = 0.95)
# -----------------------
#      CUARTO PUNTO
# -----------------------
source("Muestreo.R")
# Aplicando MAS
p_hat1 <- estimar_parametro(ESTRATO1_muestra$REC, 
                            N_1, 
                            parametro = "p")$Estimador
p_hat2 <- estimar_parametro(ESTRATO2_muestra$REC, 
                            N_2, 
                            parametro = "p")$Estimador
p_hat3 <- estimar_parametro(ESTRATO3_muestra$REC, 
                            N_3, 
                            parametro = "p")$Estimador
p_i <- c(p_hat1, p_hat2, p_hat3)
# -----------------------
alpha <- 0.05; delta <- 0.02; Z <- qnorm(1- alpha/2)
D <- delta^2/Z^2
psi_i <- N_i/N # Afijación proporcional
N <- nrow(datos);
p_i <- c(p_hat1, p_hat2, p_hat3)
# -----------------------
n <- sum((N_i^2 * p_i * (1- p_i))/psi_i)/((N^2 * D) + sum(N_i * p_i * (1- p_i)))
n_1 <- ceiling(psi_i[1] * n)
n_2 <- ceiling(psi_i[2] * n)
n_3 <- ceiling(psi_i[3] * n)
n_redond <- n_1 + n_2 + n_3






















