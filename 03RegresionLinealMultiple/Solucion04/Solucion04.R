# -----------------------------
#          PRIMER PUNTO
# -----------------------------
# Lectura información
datos <- read.csv(file.choose())
# Ajustar el modelo de regresión
Y <- datos$Performance # Y
# Definiendo las covariables en análisis
X1 <- datos$Strength; X2 <- datos$Skills; X3 <- datos$Speed
# -----------------------------
modelo <- lm(Y ~ X1 + X2 + X3) # Modelo regresión
# -----------------------------
summary(modelo) # Verificación interceptos
anova(modelo) # Verificación significancia global
# Se comparan los resultados anteriores
# -----------------------------
# Se anexa un análisis de la matriz de correlaciones
cor(datos[, -1]) # Sin la inclusión de la respuesta
# Esta matriz proporciona indicios multicolinealidad
# -----------------------------
#          SEGUNDO PUNTO
# -----------------------------
# Se requiere importar las librerías necesarias
library(car) # Función vif()
library(olsrr) # Función ols_coll_diag()
# -----------------------------
car::vif(modelo) # Identificar multicolinealidad
olsrr::ols_coll_diag(modelo) # Análisis general
# -----------------------------
# También es conveniente analizar los valores centrados
X <- model.matrix(modelo) # Extraer matriz diseño
X <- scale(X, center = TRUE, scale = FALSE) # Centrar
modelo2 <- lm(Y ~ X1 + X2 + X3, data = data.frame(X))

car::vif(modelo2) # Criterio VIF
olsrr::ols_coll_diag(modelo2) # Análisis general
# Notar que se pierde potencia con este procedimiento
# -----------------------------
# También se puede solucionar con las funciones del curso
source("funciones.R") # Establecer fuente
library(leaps) # Para usar las funciones curso
library(perturb) # Para usar las funciones curso
# -----------------------------
myCollinDiag(modelo)
# -----------------------------
#          TERCER PUNTO
# -----------------------------
myAllRegTable(modelo) # Función curso
# También se obtienen gráficas
myCp_criterion(modelo) # Gráfica CP
myR2_criterion(modelo) # Gráfica R2
myAdj_R2_criterion(modelo) # Gráfico R2 adj
modelos <- olsrr::ols_step_all_possible(modelo)
plot(modelos) # Visualización