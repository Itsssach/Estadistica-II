# -----------------------------
#          PRIMER PUNTO
# -----------------------------
datos <- read.csv(file.choose())
# Ajustar el modelo de regresión
modelo <- lm(Performance ~ ., data = datos)
# Al ajustar Performance ~ ., el . indica que se desean
# considerar la variables restantes como regresoras
resumen_completo <- summary(modelo) # Resumen del modelo 
# -----------------------------
#          SEGUNDO PUNTO
# -----------------------------
# Determinar significancia regresión global
anova_completo <- anova(modelo) # Comparar ambas tablas
anova_completo2 <- model::anova_table_lm(modelo)
# Se sugiere comparar anova_completo con anova_completo2
# ¿Qué diferencias existen entre ambas líneas de código?
# -----------------------------
# También se puede realizar con otros métodos
modelo_reducido <- lm(Performance ~ 1, data = datos)
# Revisar las características del modelo reducido
resumen_reducido <- summary(modelo_reducido) # Revisar ajuste modelo
anova_reducido <- anova(modelo_reducido) # ANOVA modelo reducido
# ¿Cómo determinar el estadístico de prueba en este caso?
# Se debe tomar en consideración la información de los
# modelos completo y reducido.
# -----------------------------
SSE_completo <- anova_completo$`Sum Sq`[4]
# SSE_completo <- 51426.62
SSE_reducido <- anova_reducido$`Sum Sq`[1]
# SSE_reducido <- 2183751
MSE <- anova_completo$`Mean Sq`[4] # MSE en general
# MSE <- 103.6827
# -----------------------------
# Determinar el número de parámetros por modelo:
parametros_completo <- length(modelo$coefficients)
# parametros_completo <- 4
parametros_reducido <- length(modelo_reducido$coefficients)
# parametros_reducido <- 1
# -----------------------------
# Definición provista: F0 = [SSE(MR) - SSE(MF)/k]/MSE(MF)
# donde k = gl(SSE(MR) - gl(SSE(MF))) = (n- 1)- (n- 4) = 3
# Similarmente k = gl(SSR(MF) - gl(SSR(MR))) = 4 - 1 = 3
# AMBOS RESULTADOS SON LOS MISMOS. A continuación se muestra:
# -----------------------------
k <- parametros_completo- parametros_reducido
# k <- 3 # Dada la resta anterior
# -----------------------------
# # Calcular la suma parcial con el SSE
SSE_parcial <- SSE_reducido - SSE_completo
# SSE_parcial <- 2132324
F0 <- (SSE_parcial/k)/MSE # Estadístico prueba
# FO <- 6855.288
# -----------------------------
# CALCULAR AHORA EL VALOR P:
p_value <- pf(F0, df1 = k, df2 = parametros_completo, lower.tail = FALSE)
# Se rechazará H0
# Se pueden verificar los resultados a mano con los resultados
# dados por anova_completo2.
# -----------------------------
#          TERCER PUNTO
# -----------------------------
resumen_completo # Significancia individual
intervalos <- confint(modelo, level = 0.95) # Intervalos confianza
# -----------------------------
#          CUARTO PUNTO
# -----------------------------
Y <- datos$Performance;
X1 <- datos$Strength; X2 <- datos$Skills; X3 <- datos$Speed;
X123 <- X1 + X2 + X3

modelo <- lm(Y ~ X1 + X2 + X3)
modelo_reducido2 <- lm(Y ~ X123)

anova(modelo, modelo_reducido2)








