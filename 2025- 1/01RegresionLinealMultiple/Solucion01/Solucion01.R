# -------------------------
#        PRIMER PUNTO
# -------------------------
datos = read.csv(file.choose()) # Ctrl + enter
View(datos) # Para visualizar los datos
nrow(datos) # Para el número de filas (n)
# Hallar matriz de diseño
Y = datos$Performance
X1 = datos$Strength; X2 = datos$Skills; X3 = datos$Speed
# -------------------------
modelo <- lm(Y ~ X1+ X2+ X3)
X = model.matrix(modelo)
# Hallar el vector de parámetros estimados
B = solve(t(X) %*% X) %*% t(X) %*% Y
# -------------------------
#        SEGUNDO PUNTO
# -------------------------
Y_gorro <- X %*% B
# -------------------------
#        TERCER PUNTO
# -------------------------
errores_estimados = Y - Y_gorro
# -------------------------
#        CUARTO PUNTO
# -------------------------
n = nrow(datos); p = length(datos)
MSE = (t(errores_estimados) %*% errores_estimados)/(n- p)
# -------------------------
# Función en R
summary(modelo)