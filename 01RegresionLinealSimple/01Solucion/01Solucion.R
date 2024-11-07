library("tidyverse") # Librería para graficar

# PRIMER PUNTO
# -----------------------------
#           Read data
# -----------------------------
data <- read.csv("data/FreedomIndex.csv") |> 
  tidyr::drop_na() |> 
  dplyr::select(3:15) |> 
  dplyr::rename(OS = Overall.Score, PR = Property.Rights, 
                GI = Government.Integrity, JE = Judicial.Effectiveness, 
                TB = Tax.Burden, GS = Government.Spending, 
                FH = Fiscal.Health, BF = Business.Freedom, 
                LF = Labor.Freedom, MF = Monetary.Freedom, 
                TF = Trade.Freedom, IF = Investment.Freedom, 
                FF = Financial.Freedom)
# -----------------------------
# Lectura de la base de datos con algún tratamiento



# -----------------------------
#     Análisis descriptivo
# -----------------------------
cor_matriz <- cor(data) |> 
  reshape2::melt() # Formato largo

# -----------------------------
correlation_plot <- ggplot(cor_matriz, aes(Var1, Var2, fill= value)) + 
  geom_tile(color= "blue") + 
  scale_fill_gradient2(low= "blue", high= "red", 
                       midpoint= 0, limit= c(-1, 1), space= "Lab", 
                       name= "Correlacion") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  # coord_fixed() + 
  labs(title= "Matriz de correlaciones", x= "", 
       y= "", subtitle= "En relacion al problema")
# Gráfico de correlaciones. También se puede usar la función
# plot(data)


# -----------------------------
#    Gráfico de correlación
# -----------------------------
highly_correlated <- data |> 
  dplyr::select(-TB, -GS, -FH, -MF)
GGally::ggpairs(highly_correlated, title = "Gráfico de correlación")

# -----------------------------
#    Gráfico de densidad (OS)
# -----------------------------
density_plot <- ggplot(data, aes(OS)) + 
  geom_histogram(aes(y= after_stat(density)), fill= "lightgray",
                 color= "black", position= "identity") +
  geom_density(color= "red", lwd= 1, fill= "red", alpha= 0.5) +
  labs(title= "Histograma de densidad", x= "Observaciones", 
       y= "Densidad", subtitle= "En relación a Overall Score" )
# También se puede con la función plot(density(data))


# SEGUNDO PUNTO
# -----------------------------
#      Modelo de regresión
# -----------------------------
model <- stats::lm(OS ~ GI, data = data)
# Ajustando el modelo de regresión
# Encontrar la información
beta_0 <- coef(model)[1] # Beta_0
# Same as model$coefficients[1]
beta_1 <- coef(model)[2] # Beta_1
y_bar <- mean(data$OS) # Mean Y
y_hat <- fitted(model) # Ajustados
residuals <- model$residuals
sigma_2 <- sigma(model)^2 # Sigma^2
# -----------------------------
# Hallando los valores a mano
x <- data$GI; y <- data$OS; n <- length(data$OS)
x_bar <- mean(x); y_bar <- mean(y)
Sxx <- sum((x- x_bar)^2); Sxy <- sum((x- x_bar)*y)
beta_1 <- Sxy/Sxx # Beta_0
beta_0 <- y_bar - (beta_1 * x_bar) # Beta_1
residuals <- y - y_hat # Residuales
sigma_2 <- sum(residuals^2)/(n -2)
# -----------------------------


# --------------------------
#    Gráfico de regresión
# --------------------------
regression <- ggplot(data, aes(x = GI, y = OS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE,color = "red") + 
  labs(title = "Gráfico de Regresión", x = "GI", y = "OS", 
       subtitle= "En relación al problema")

# TERCER PUNTO
# --------------------------
#         Parámetros
# --------------------------
model_summary <- summary(model)

# CUARTO PUNTO
# --------------------------
#   Intervalo de confianza
# --------------------------
confint_beta0 <- confint(model, "(Intercept)", level = 0.95)
confint_beta1 <- confint(model, "GI", level = 0.95)

# REALIZANDO LOS INTERVALOS A MANO:
# --------------------------
alpha <- 0.05 # Nivel de significancia
beta0_lower <- beta_0 - qt(1 - alpha/2, n- 2)*sqrt(sigma_2*sum(x^2)/(n*Sxx))
beta0_upper <- beta_0 + qt(1 - alpha/2, n-2)*sqrt(sigma_2*sum(x^2)/(n*Sxx))
# --------------------------
beta1_lower <- beta_1 - qt(1- alpha/2, n-2)*sqrt(sigma_2/Sxx)
beta1_upper <- beta_1 + qt(1- alpha/2, n-2)*sqrt(sigma_2/Sxx)














