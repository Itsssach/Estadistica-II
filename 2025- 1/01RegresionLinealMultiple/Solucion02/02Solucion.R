# ---------------------
#     PRIMER PUNTO
# ---------------------
datos = read.csv(file.choose())
# Caracterizar la información
Y <- datos$Performance # Ctrl + enter
X1 <- datos$Strength; X2 <- datos$Skills; X3 <- datos$Speed
# Ya está caracterizada la información
# Proceder ajustar un modelo de regresión
modelo <- lm(Y ~ X1 + X2 + X3)

# Consultar los valores ajustados
summary(modelo)
# ---------------------
#     SEGUNDO PUNTO
# ---------------------
n <- nrow(datos); p <- length(modelo$coefficients)
# UTILIZANDO UN TRUCO
# Primer paso: ajustar el modelo completo
modelo <- lm(Y ~ X1 + X2 + X3)
# Segundo paso: ajustar el modelo reducido
modelo_reducido <- lm(Y ~ 1)
# Tercer paso: utilizar una función anova compuesta
anova_comparacion <- anova(modelo_reducido, modelo)
# ---------------------
# PROCEDIMIENTO COMPLETO
# Primer paso: ajustar el modelo completo
modelo <- lm(Y ~ X1 + X2 + X3)
# Segundo paso: ajustar el modelo reducido
modelo_reducido <- lm(Y ~ 1)
# Tercer paso: hallar cantidades
# Utilizar la función ANOVA
anova(modelo_reducido)
SSE_reducido <- 2183751
anova(modelo)
SSE_completo <- 51427
MSE <- SSE_completo/(n- p)
v <- 3 # Grados de libertad
F_modelo <- ((SSE_reducido - SSE_completo)/v)/MSE
# ---------------------
#     TERCER PUNTO
# ---------------------
summary(modelo)
# Construcción de un intervalo de confianza
confint(modelo) # Por defecto en alpha = 0.05
# ---------------------
#     CUARTO PUNTO
# ---------------------
# Primer paso: definir el modelo completo
modelo <- lm(Y ~ X1 + X2 + X3)
# Segundo paso: definir el modelo reducido
X123 <- X1 + X2 + X3 # Definir nueva variable
modelo_LG <- lm(Y ~ X123)
# Tercer paso: aplicar el truco
anova_LG <- anova(modelo_LG, modelo)





