# PRIMER PUNTO
# -----------------------------
#           Read data
# -----------------------------
data <- read.csv("data/FreedomIndex.csv")
data <- data[complete.cases(data), ] # NA's
data <- data[, 3:15] # Numéricas
names(data) <- c("OS", "PR", "GI", "JE", 
                 "TB", "GS", "FH", "BF", 
                 "LF", "MF", "TF", "IF", 
                 "FF") # Renombrar
# -----------------------------
# Lectura de la base de datos con algún tratamiento

# -----------------------------
#     Análisis descriptivo
# -----------------------------
plot(data) # Correlaciones
# Seleccionar dos covariables

# SEGUNDO PUNTO
set.seed(007) # Fijar semilla replicación
data_model <- data[ , c("OS", "PR", "GI")]
n <- nrow(data_model) # Número filas

rows <- sample(1:n, 0.75 * n) # Filas selección

# Partición del conjunto de datos
data_training <- data_model[rows, ] # Ajuste
data_test <- data_model[-rows, ] # Predicción

# TERCER PUNTO
# -----------------------------
#         Ajuste modelo
# -----------------------------
# También se puede stats::lm()
model_1 <- lm(OS ~ GI, data = data_training)
model_2 <- lm(OS ~ PR, data = data_training)

par(mfrow = c(1, 2))
# Para el primer modelo
plot(data_training$GI, data_training$OS, main = "Modelo 1: OS vs GI", xlab = "GI", ylab = "OS")
abline(model_1, col = "blue")  # Recta regresión

# Para el segundo modelo
plot(data_training$PR, data_training$OS, main = "Modelo 1: OS vs PR", xlab = "PR", ylab = "OS")
abline(model_2, col = "red")  # Recta regresión

# CUARTO PUNTO
# -----------------------------
#         Ajuste modelo
# -----------------------------
# Resúmenes de los modelos
summary_1 <- summary(model_1)
summary_2 <- summary(model_2)

# Análisis de varianza
# También se puede stats::anova()
anova_1 <- anova(model_1)
anova_2 <- anova(model_2)

# QUINTO PUNTO
# -----------------------------
#         Cálculo R2
# -----------------------------
r2_model1 <- summary_1$r.squared
r2_model2 <- summary_2$r.squared

# También se puede realizar a mano
# Con coeficiente correlación
y <- data_training$OS # Datos reales
y_hat1 <- fitted(model_1) # Ajustados (1)
y_hat2 <- fitted(model_2) # Ajustados (2)
# En lugar de fitted también se puede predict(model)
r2_model1 <- cor(y, y_hat1)^2 # Modelo 1
r2_model2 <- cor(y, y_hat2)^2 # Modelo 2

# Con sumas de cuadrados
y_bar <- mean(y) # Media datos reales
SST <- sum((y - y_bar)^2)
SSR <- sum((y_hat1 - y_bar)^2)
SSE <- sum((y - y_hat1)^2)
R2_sum <- SSR/SST

# SEXTO PUNTO
# -----------------------------
#         Cálculo R2
# -----------------------------
# Intervalo confianza respuesta media
confianza_datos <- data_training[, c("OS", "GI")]
predict(model_1, newdata = confianza_datos, 
        interval = "confidence", 
        level = 0.95)[1:3, ]

prediccion_datos <- data_test[, c("OS", "GI")]
predict(model_1, newdata = prediccion_datos, 
        interval = "prediction", 
        level = 0.95)[1:3, ]

# Estimación predicción
predict(model_1, newdata = data.frame(GI = 9), 
        interval = "prediction", 
        level = 0.95)
