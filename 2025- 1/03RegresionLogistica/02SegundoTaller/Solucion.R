# -----------------------
#      PRIMER PUNTO
# -----------------------
# Utilizar librería
library(pROC)  # Para ROC y punto de corte
library(PRROC) # Utilizar en el parcial
datos <- read.csv(file.choose())

# Ajustar el modelo
datos$Personality <- ifelse(datos$Personality == "Introvert", 1, 0)

# Se debe hacer una división del conjunto de datos
# Conjunto de entrenamiento y conjunto de prueba
# Con el conjunto de prueba: Validación; ROC, AUC, TCC, Matriz confusión
data_train <- datos[1:1766, ]
data_test <- datos[1767:2523, ]

# El modelo se debe ajustar con el conjunto de entrenamiento
modelo <- glm(Personality ~ ., data = data_train, family = binomial(link = "logit"))
summary(modelo)

# Considerar una variable de predicción
predict_test <- predict(modelo, newdata = data_test, type = c("response"))
roc_object <- roc.curve(scores.class0 = predict_test,
                        weights.class0 = data_test$Personality,
                        curve = TRUE) # PRROC
plot(roc_object)
# -----------------------
#      SEGUNDO PUNTO
# -----------------------
# Definición analítica del punto de corte
y <- data_test$Personality
roc_obj <- roc(y, predict_test)
coords(roc_obj, "best", best.method = "closest.topleft")$threshold
# -----------------------
#      TERCER PUNTO
# -----------------------
# Hallar la matriz y el TCC con el punto de corte óptimo
matriz1 <- table(data_test$Personality, predict_test >= 0.8534028)
TCC1 <- sum(diag(matriz1))/sum(matriz1)
matriz2 <- table(data_test$Personality, predict_test >= 0.5)
TCC2 <- sum(diag(matriz2))/sum(matriz2)
matriz3 <- table(data_test$Personality, predict_test > 0.9)
TCC3 <- sum(diag(matriz3))/sum(matriz3)
matriz4 <- table(data_test$Personality, predict_test > 0.2)
TCC4 <- sum(diag(matriz4))/sum(matriz4)
matriz5 <- table(data_test$Personality, predict_test > 0.8)
TCC5 <- sum(diag(matriz5))/sum(matriz5)








