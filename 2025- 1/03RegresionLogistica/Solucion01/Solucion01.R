# -------------------------
#        PRIMER PUNTO
# -------------------------
# Ajuste del modelo de regresión
datos <- read.csv(file.choose())
datos$Personality <- ifelse(datos$Personality == "Introvert", 1, 0)
modelo <- glm(Personality ~ ., data = datos, family = binomial(link = "logit"))
# -------------------------
#        SEGUNDO PUNTO
# -------------------------
# Modelo reducido
modelo_reducido <- glm(Personality ~ 1, data = datos, family = binomial(link = "logit"))
# Considerar la prueba con anova
anova(modelo_reducido, modelo, test = "Chisq")
# -------------------------
#        TERCER PUNTO
# -------------------------
summary(modelo)
confint(modelo)
exp(0.32057)
# -------------------------
#        CUARTO PUNTO
# -------------------------
x_new <- data.frame(Time_spent_Alone = 3, Stage_fear = "No", 
                    Social_event_attendance = 9, Going_outside = 7, 
                    Friends_circle_size = 10, Post_frequency = 10)
probability <- predict.glm(modelo, newdata = x_new, type = c("response"))
resultado <- ifelse(probability >= 0.5, "Introvertida", "Extrovertida")
resultado
