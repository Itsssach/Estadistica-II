# -----------------------------
#          PRIMER PUNTO
# -----------------------------
datos <- read.csv(file.choose())
# La instrucción anterior abre una pestaña auxiliar
# O también se puede con una dirección absoluta:
# datos <- read.csv('data/students_performance.csv')
X <- as.matrix(cbind(Intercept = 1, datos[, -1]))
# Definiendo la matriz de diseño (objeto matricial)
Y <- as.matrix(datos[, 1]) 
# Definiendo el vector Y (como objeto matricial)

# -----------------------------
#          SEGUNDO PUNTO
# -----------------------------
betas <- solve((t(X) %*% X)) %*% t(X) %*% Y
# Según la definición (X'X)^{-1} X'Y

# -----------------------------
#          TERCER PUNTO
# -----------------------------
matriz_H <- X %*% solve((t(X) %*% X)) %*% t(X)
Y_gorro <- matriz_H %*% Y
# O también se puede de la siguiente manera:
# y_gorro <- X %*% betas

# -----------------------------
#          CUARTO PUNTO
# -----------------------------
residuales <- Y - Y_gorro # Por definición

# -----------------------------
#         Solución tarea
# -----------------------------
n <- nrow(datos); p <- nrow(betas)
# Para hallar el número de datos 'n'
# Para hallar el número de parámetros 'p'
MSE <- (t(residuales) %*% residuales)/(n- p)

# VERIFICAR TODOS LOS RESULTADOS
modelo <- lm(Performance ~ ., data = datos)
# Al ajustar Performance ~ ., el . indica que se desean
# considerar la variables restantes como regresoras
summary(modelo) # Resumen del modelo

