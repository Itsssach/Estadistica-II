# ---------------------
datos <- read.csv(file.choose())
modelo <- lm(price ~ bore * bodyStyle, data = datos)
summary(modelo)
# ---------------------
library(car)
linearHypothesis(modelo, "bodyStylesedan=0")
linearHypothesis(modelo, "bore:bodyStylewagon=0")
# ---------------------
datos2 <- read.csv(file.choose())
modelo2 <- lm(Calificacion ~ Estudio + Excelencia + Sueno, data = datos2)
source("funciones.R") # Funciones del curso
# Librerías
library(leaps); library(olsrr); library(perturb) # IMPORTANTE
# ---------------------
myBackward(datos2[, -5])
myAnova(modelo2)
modelo_reducido <- lm(Calificacion ~ Excelencia + Sueno, data = datos2)
myAnova(modelo_reducido)
# ---------------------
modelo_step <- ols_step_both_p(modelo2)
ols_step_forward_p(modelo2)

