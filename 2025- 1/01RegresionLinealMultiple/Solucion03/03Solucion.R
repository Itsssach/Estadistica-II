# -----------------------------
#          PRIMER PUNTO
# -----------------------------
# Lectura información
datos <- read.csv(file.choose())
# Definir información en analisis:
Y <- datos$Performance; # Definir covariables:
X1 <- datos$Strength; X2 <- datos$Skills; X3 <- datos$Speed
modelo <- lm(Y ~ X1 + X2 + X3)
# -----------------------------
# Validación supuestos del modelo
# -----------------------------
# Se asume que la independencia es cierta
# Normalidad (Criterio Gráfico)
plot(modelo, 2) # Se cumple
shapiro.test(modelo$residuals) # NO rechazo H0

# Media cero siempre se cumple
# Varianza constante
plot(modelo, 1) # Se cumple
# -----------------------------
#          SEGUNDO PUNTO
# -----------------------------
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
# -----------------------------
#          TERCER PUNTO
# -----------------------------
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
# -----------------------------
#          CUARTO PUNTO
# -----------------------------
X <- model.matrix(modelo)
Hat_values <- hat(X)
x01 <- c(1, 45.03,80.88,60.33)
x02 <- c(1,77.08,100,13.76)
summary(X)
# -----------------------------
ifelse(t(x01)%*%solve(t(X)%*%X)%*%x01 < max(Hat_values), "Pertence a la region de diseno", "No pertenece")
ifelse(t(x02)%*%solve(t(X)%*%X)%*%x02 < max(Hat_values), "Pertence a la region de diseno", "No pertenece")
# -----------------------------
predict(modelo, newdata = data.frame(X1 = 45.03, 
                                     X2 = 80.88, 
                                     X3 = 60.33), interval = "prediction")
# -----------------------------













