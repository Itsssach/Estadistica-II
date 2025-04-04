################################
#    Función para estimación
################################
estimar_parametro <- function(muestra, N, parametro, intervalo = FALSE, confianza = 0.95){
  # ----------------------------
  # Esta función recibe como argumentos una muestra
  # preespecificada, una población definida, el parámetro
  # que se desea estimar. La opción para el intervalo
  # de confianza es optativa y puede activarse
  # usando 'interval = TRUE'. El nivel de confianza
  # también puede ser modificado.
  # ----------------------------
  n <- length(muestra) # Se debe ingresar muestra
  # N es el tamaño de la población
  
  # ----------------------------
  #   Definición de estimadores
  # ----------------------------
  media_muestra <- mean(muestra) # Media muestral
  sd_muestra <- sd(muestra) # Desviación estándar
  fpc <- sqrt((N - n) / N) # Factor correción
  # ----------------------------
  # Selección del parámetro a estimar
  estimador <- NULL
  ic <- NULL
  # ----------------------------
  # Estimador para la media poblacional
  if (parametro == "mu"){
    estimador <- media_muestra
    
    if (intervalo){
      error_est <- (sd_muestra / sqrt(n)) * fpc
      alfa <- 1 - confianza
      t_critico <- qt(1 - alfa / 2, df = n - 1)
      ic <- c(estimador - t_critico * error_est, estimador + t_critico * error_est)
    } # AQuí se termina la opción oara mu
  
  # ----------------------------  
  # Estimador del total poblacional
  } else if (parametro == "tau"){
    estimador <- media_muestra * N
    
    if (intervalo){
      error_est <- (sd_muestra / sqrt(n)) * (N * fpc)
      alfa <- 1 - confianza
      t_critico <- qt(1 - alfa / 2, df = n - 1)
      ic <- c(estimador - t_critico * error_est, estimador + t_critico * error_est)
    } # Aquí se termina la opción para tau
  
  # ----------------------------
  # Estimador de la proporción poblacional
  # Solo es útil para datos binarios 
  } else if (parametro == "p"){
    p_muestra <- mean(muestra)
    estimador <- p_muestra
    
    if (intervalo){
      error_est <- sqrt((p_muestra * (1 - p_muestra)) / (n - 1)) * fpc
      alfa <- 1 - confianza
      t_critico <- qt(1 - alfa / 2, df = n - 1)
      ic <- c(estimador - t_critico * error_est, estimador + t_critico * error_est)
    } # Aquí se termina la opción para p
  
  # ----------------------------
  # Estimador de la varianza poblacional
  # Solo es útil para datos binarios 
  } else if (parametro == "A"){
    p_muestra <- mean(muestra)
    estimador <- N * mean(muestra)
    
    if (intervalo){
      error_est <- N * sqrt((p_muestra * (1 - p_muestra)) / (n - 1)) * fpc
      alfa <- 1 - confianza
      t_critico <- qt(1 - alfa / 2, df = n - 1)
      ic <- c(estimador - t_critico * error_est, estimador + t_critico * error_est)
    } # Aquí se termina la opción para A
  } else {
    stop("Parámetro no reconocido. Usar los del curso.")
  } # Se añade una condición adicional para un parámetro no conodido
  # ----------------------------
  resultado <- list(Estimador = estimador)
  if (!is.null(ic)) resultado$IC <- ic
  # ----------------------------
  return(resultado) # Valor a retornar
} # Culminación de la función propuesta
