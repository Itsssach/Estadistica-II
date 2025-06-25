# -----------------------
#      PRIMER PUNTO
# -----------------------
# Utilizar librería
library(pROC)  # Para ROC y punto de corte
library(PRROC)
datos <- read.csv(file.choose())

# División de datos
data_train <- datos[1:1766, ]
data_test <- datos[1767:2523, ]

# Ajuste del modelo
data_train$Personality <- ifelse(data_train$Personality == "Introvert", 1, 0)
data_test$Personality <- ifelse(data_test$Personality == "Introvert", 1, 0)
modelo <- glm(Personality ~ ., data = data_train, family = binomial(link = "logit"))

# Validación con el conjunto de prueba
# Crear un objeto de predicción
predict_test <- predict(modelo, newdata = data_test, type = c("response"))
roc_object <- roc.curve(scores.class0 = predict_test,
          weights.class0 = data_test$Personality,
          curve = TRUE)
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
# Punto de corte óptimo
matriz1 <- table(data_test$Personality, predict_test > 0.8534028)
TCC1 <- sum(diag(matriz1))/sum(matriz1)
matriz2 <- table(data_test$Personality, predict_test > 0.8)
TCC2 <- sum(diag(matriz2))/sum(matriz2)
matriz3 <- table(data_test$Personality, predict_test > 0.9)
TCC3 <- sum(diag(matriz3))/sum(matriz3)
matriz4 <- table(data_test$Personality, predict_test > 0.2)
TCC4 <- sum(diag(matriz4))/sum(matriz4)
matriz5 <- table(data_test$Personality, predict_test > 0.5)
TCC5 <- sum(diag(matriz5))/sum(matriz5)



