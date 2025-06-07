# ----------------------
#      PRIMER PUNTO
# ----------------------
datos <- data
# Probar normalidad
# Ajustar el modelo de regresión:
modelo <- lm(Promedio ~ ., data = datos)
# Otra forma
Y <- datos$Promedio
X1 <- datos$Horas_Estudio; X2 <- datos$Estres; X3 <- datos$Sueno
modelo <- lm(Y ~ X1 + X2 + X3)
# ----------------------
# Supuesto de normalidad (gráfica)
plot(modelo, 2) # Verificar con shapiro- wilk
residuales <- modelo$residuals
residuales <- residuals(modelo)
shapiro.test(residuales)
# ----------------------
# Verificar varianza constante
plot(modelo, 1)
library(lmtest)
bptest(modelo)
# ----------------------
# La independencia se asume
# ----------------------
#     SEGUNDO PUNTO
# ----------------------
# Significancia de la regresión global
# (1). Primer paso: Ajustar del modelo completo
modelo <- lm(Y ~ X1 + X2 + X3)
# (2). Segundo paso: Ajustar el modelo reducido
modelo_reducido <- lm(Y ~ 1) 
# (3). Tercer paso: Sacar el estadístico
truco <- anova(modelo_reducido, modelo)
# ----------------------
#     TERCER PUNTO
# ----------------------
summary(modelo)
# Intervalos de confianza
confint(modelo, level = 0.99)
# -----------------------
#      CUARTO PUNTO
# -----------------------
# Definir el tamaño de muestra y parámetros
n <- nrow(datos); p <- length(modelo$coefficients)
# Definir los valores hat (h_{i}):
hat_values <- hatvalues(modelo) # Para puntos balanceo
balanceo <- which(hat_values > ((2 * p)/n))

# Determinar puntos atípicos
estandarizados <- rstandard(modelo)
estudentizados <- rstudent(modelo)
atipicos_estandarizados <- which(abs(estandarizados) > 3)
atipicos_estudentizados <- which(abs(estudentizados) > 3)
# También se puede ver con criterio gráfico
plot(modelo, 3) # Residuales estandarizados
plot(modelo, 2) # Residuales estandarizados

cooks <- cooks.distance(modelo)
which(cooks > 1) # Verificar cooks
DFBetas <- dfbetas(modelo) # Definir
which(abs(DFBetas) > 2/sqrt(n)) # Verificar DFBETAS
DFFITS <- dffits(modelo) # Definir
which(abs(DFFITS) > (2 * sqrt(p/n))) # Verificar DFFITS
# -----------------------------
influencias <- influence.measures(modelo)
summary(influencias)
plot(modelo, 4)
# -----------------------
#      QUINTO PUNTO
# -----------------------
X <- model.matrix(modelo)
Hat_values <- hat(X)
x01 <- c(1, 55, 32, 19)
# -----------------------------
ifelse(t(x01)%*%solve(t(X)%*%X)%*%x01 < max(Hat_values), "Pertence a la region de diseno", "No pertenece")
# -----------------------------
predict(modelo, newdata = data.frame(X1 = 55, 
                                     X2 = 32, 
                                     X3 = 19), interval = "prediction")
