# -----------------------------
#         PRIMER PUNTO
# -----------------------------
datos = read.csv("data/datos.csv")
# Realizar una visualización de los datos
View(datos) # Muestra los datos
# Realizar análisis descriptivo
plot(datos) # Muestra matriz de correlación
cor(datos) # Matriz numérica de correlación
# -----------------------------
#         SEGUNDO PUNTO
# -----------------------------
n = nrow(datos) # Tamaño de los datos
# Ajuste de modelos lineales
modelo1 = lm(OS ~ GI, data = datos)
modelo2 = lm(OS ~ PR, data = datos)
# Ejecución del resumen de los modelos
resumen1 = summary(modelo1) # Primer resumen
resumen2 = summary(modelo2) # Segundo resumen
# Verificar si B0 está contenido en el conjunto de datos
min(datos) # sí está contenido
# Graficar las regresiones
# Primera regresión
plot(datos$PR, datos$OS, main = "Regresión 1")
abline(modelo1, col = "red")
# Segunda regresión  
plot(datos$GI, datos$OS, main = "Regresión 2")
abline(modelo1, col = "red")

# Verificar las unidades
summary(datos)
# -----------------------------
#         TEECER PUNTO
# -----------------------------
# Probar la significancia de la regresión
anova(modelo1)
# -----------------------------
#         QUINTO PUNTO
# -----------------------------
# IC para la respuesta medio
predict(modelo1, newdata = data.frame(GI = 9), 
        interval = "confidence", 
        level = 0.95)
predict(modelo1, newdata = data.frame(GI = 9), 
        interval = "prediction", 
        level = 0.95)