# ------------------------
#          Librerías
# ------------------------
# Para carencia de ajuste
if (!require("remotes")) install.packages("remotes")
remotes::install_github("fhernanb/model")
library(model)

# ------------------------
#          Lectura
# ------------------------
datos <- read.csv(file.choose()) 
# Lectura de los datos seleccionándolos directamente


# PRIMER PUNTO Y SEGUNDO PUNTO
modelo <- lm(Satisfaction ~ Experience, data = datos)
# Especificación del modelo
plot(modelo, 1) # Gráfica residuales
summary(modelo) # Resumen

# TERCER PUNTO
plot(modelo, 2) # Gráfico normalidad
shapiro.test(modelo$residuals) # shapiro.test(residuals(modelo))
plot(density(modelo$residuals)) # Gráfico densidad

# CUARTO PUNTO
summary(modelo) # Resumen
min(datos$Experience) # Valor mínimo X_min
confint(modelo) # Intervalo confianza
anova(modelo) # Tabla ANOVA

# QUINTO PUNTO
lack_fit_test(modelo) # Falta de ajuste

# SEXTO PUNTO
plot(datos$Experience, datos$Satisfaction) # Dispersión
abline(modelo) # Ajuste de la recta
modelo_2 <- lm(log(Satisfaction) ~ Experience, data = datos)
# Especificación del modelo transformado

summary(modelo_2) # Resumen
plot(datos$Experience, log(datos$Satisfaction)) # Dispersión
abline(modelo_2) # Ajuste modelo transformado

# Media cero y varianza
plot(modelo_2, 1) # Gráfico residuales
plot(modelo_2, 2) # Gráfico normalidad
shapiro.test(modelo2$residuals) # Prueba normalidad
