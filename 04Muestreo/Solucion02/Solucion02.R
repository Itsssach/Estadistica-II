# -----------------------
#       PRIMER PUNTO
# -----------------------
# Determinar afijación y MAE
library(tidyverse)
# -----------------------
# Función para muestreo aleatorio simple sin reemplazo
datos <- read.csv(file.choose())
# -----------------------
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
N <- nrow(datos)
N_1 <- 39294; N_2 <- 39157; N_3 <- 3510;
# -----------------------
# Afijación proporcional
N_i <- c(N_1, N_2, N_3)
psi_i <- N_i/N
# -----------------------
n <- 8000;
n_i <- ceiling(psi_i * n)
n_1 <- 325; n_2 <- 324; n_3 <- 29
# -----------------------
# Afijación de Neymann
sigma_est1 <- sd(ESTRATO1$I_HOGAR)
sigma_est2 <- sd(ESTRATO2$I_HOGAR)
sigma_est3 <- sd(ESTRATO3$I_HOGAR)
sigma_i <- c(sigma_est1, sigma_est2, sigma_est3)
# -----------------------
n_iNeymann <- ((N_i * sigma_i)/(sum(N_i * sigma_i))) * n
# -----------------------
# Proceso de muestreo, con la función MAS y empleando
# una afijación proporcional
ESTRATO1_muestra <- MAS(325, ESTRATO1)
ESTRATO2_muestra <- MAS(324, ESTRATO2)
ESTRATO3_muestra <- MAS(29, ESTRATO3)
# -----------------------
estimar_mu1 <- estimar_parametro(ESTRATO1_muestra$I_HOGAR, N_1, 
                                 parametro = "mu", intervalo = TRUE)
mu_hat1 <- estimar_mu1$Estimador
varianzaest_mu1 <- estimar_mu1$Varianza
IC_mu1 <- estimar_mu1$IC
# -----------------------
# Estimaciones para el segundo estrato
estimar_mu2 <- estimar_parametro(ESTRATO2_muestra$I_HOGAR, N_2, 
                                 parametro = "mu", intervalo = TRUE)
mu_hat2 <- estimar_mu2$Estimador
varianzaest_mu2 <- estimar_mu2$Varianza
IC_mu2 <- estimar_mu2$IC
# -----------------------
# Estimaciones para el tercer estrato
estimar_mu3 <- estimar_parametro(ESTRATO3_muestra$I_HOGAR, N_3, 
                                 parametro = "mu", intervalo = TRUE)
mu_hat3 <- estimar_mu3$Estimador
varianzaest_mu3 <- estimar_mu3$Varianza
IC_mu3 <- estimar_mu3$IC
# -----------------------
# Determinar el parámetro mu global estimado para toda la población
mu_hat <- c(mu_hat1, mu_hat2, mu_hat3) # Vector de mu por estrato
m_i <- c(N_1/N, N_2/N, N_3/N) # Factores ponderación
# Con la información anterior, se corre la suma ponderada:
mu_hat_global <- sum(m_i * mu_hat)
# -----------------------
# Para el cálculo de los intervalos de confianza
# varianzas estimadas de todos los estratos:
varianzaest_mu <- c(estimar_mu1$Varianza, estimar_mu2$Varianza, estimar_mu3$Varianza)
# Cuando se complete la información anterior, correr la línea:
varianzaest_global <- sum(m_i^2 * varianzaest_mu)
# -------------------------
# Luego el intervalo de confianza:
alpha_global <- 0.05; L <- 3; t_global <- qt(1- alpha_global/2, n- L)
# Con la información anterior, puede definirse el intervalo:
IC_muglobal <- c(mu_hat_global - t_global * sqrt(varianzaest_global), 
                 mu_hat_global + t_global * sqrt(varianzaest_global))
# Así se define el intervalo de confianza con su LI y LS
# -------------------------
estimar_p1 <- estimar_parametro(ESTRATO1_muestra$SEC, N_1, 
                                parametro = "p", intervalo = TRUE)
p_hat1 <- estimar_p1$Estimador
varianzaest_p1 <- estimar_p1$Varianza
IC_p1 <- estimar_p1$IC
# -------------------------
estimar_p2 <- estimar_parametro(ESTRATO2_muestra$SEC, N_2, 
                                parametro = "p", intervalo = TRUE)
p_hat2 <- estimar_p2$Estimador
varianzaest_p2 <- estimar_p2$Varianza
IC_p2 <- estimar_p2$IC
# -------------------------
estimar_p3 <- estimar_parametro(ESTRATO3_muestra$SEC, N_3, 
                                parametro = "p", intervalo = TRUE)
p_hat3 <- estimar_p3$Estimador
varianzaest_p3 <- estimar_p3$Varianza
IC_p3 <- estimar_p3$IC
# -------------------------
p_hat <- c(p_hat1, p_hat2, p_hat3)
m_i <- c(N_1/N, N_2/N, N_3/N) # Factores ponderación
p_hat_global <- sum(p_hat * m_i)
# -------------------------
varianzaest_p <- c(varianzaest_p1, varianzaest_p2, varianzaest_p3)
varianzaest_global_p <- sum(m_i^2 * varianzaest_p)
# -------------------------
alpha_global <- 0.05; L <- 3; t_global <- qt(1- alpha_global/2, n- L)
# Con la información anterior, puede definirse el intervalo:
IC_pglobal <- c(p_hat_global - t_global * sqrt(varianzaest_global_p), 
                 p_hat_global + t_global * sqrt(varianzaest_global_p))
# -----------------------
#       TERCER PUNTO
# -----------------------
alpha <- 0.05; delta <- 0.02; Z <- qnorm(1- alpha/2)
D <- delta^2/Z^2
psi_i <- N_i/N # Afijación proporcional
N <- nrow(datos);
p_i <- c(p_hat1, p_hat2, p_hat3)
# -----------------------
n <- sum((N_i^2 * p_i * (1- p_i))/psi_i)/((N^2 * D) + sum(N_i * p_i * (1- p_i)))
n_redon <- ceiling(n)





