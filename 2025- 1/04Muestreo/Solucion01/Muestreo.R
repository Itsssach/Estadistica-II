############################################################
#       Función para Estimación por Muestreo
#      Detecta automáticamente MAS y MAE
############################################################
estimar_parametro <- function(datos_muestrales, 
                              info_poblacion, 
                              parametro, 
                              intervalo = FALSE, 
                              confianza = 0.95){
  
  # -----------------------------------------------------------------
  #  Documentación:
  # - datos_muestrales:
  #     - Para MAS: Un vector numérico. ej: c(1, 2, 3)
  #     - Para MAE: Una lista de vectores numéricos. ej: list(e1 = c(1,2), e2 = c(3,4))
  # - info_poblacion:
  #     - Para MAS: Un número único para el tamaño total (N). ej: 1000
  #     - Para MAE: Un vector con los tamaños de cada estrato (N_i). ej: c(e1 = 600, e2 = 400)
  # -----------------------------------------------------------------
  
  # =================================================================
  #           CASO 1: MUESTREO ALEATORIO ESTRATIFICADO (MAE)
  #           Se detecta si 'datos_muestrales' es una lista.
  # =================================================================
  if (is.list(datos_muestrales)) {
    
    # --- Renombrar variables para claridad en este bloque ---
    muestras_por_estrato <- datos_muestrales
    N_por_estrato <- info_poblacion
    
    # --- Validaciones para MAE ---
    if (!is.vector(N_por_estrato) || length(muestras_por_estrato) != length(N_por_estrato)) {
      stop("Para MAE, 'info_poblacion' debe ser un vector con tamaños para cada estrato en la lista")
    }

    # --- Validación de nombres de estratos ---
    if (!is.null(names(N_por_estrato)) && !is.null(names(muestras_por_estrato))) {
      if (!all(names(N_por_estrato) == names(muestras_por_estrato))) {
        stop("Los nombres de los estratos no coinciden entre muestras y info_poblacion")
      }
    }
    
    # --- Cálculos por estrato ---
    L <- length(muestras_por_estrato)
    N <- sum(N_por_estrato)
    pesos_W_h <- N_por_estrato / N
    
    n_h <- sapply(muestras_por_estrato, length)
    media_h <- sapply(muestras_por_estrato, mean)
    var_muestral_h <- sapply(muestras_por_estrato, var)
    
    fpc_h <- (N_por_estrato - n_h) / N_por_estrato
    
    # --- Estimación según el parámetro para MAE ---
    if (parametro == "mu") {
      estimador <- sum(pesos_W_h * media_h)
      var_estimador <- sum((pesos_W_h^2 * var_muestral_h / n_h) * fpc_h)
    } else if (parametro == "tau") {
      estimador <- N * sum(pesos_W_h * media_h)
      var_estimador <- sum((N_por_estrato^2 * var_muestral_h / n_h) * fpc_h)
    } else if (parametro == "p") {
      p_h <- media_h
      estimador <- sum(pesos_W_h * p_h)
      var_p_h <- (p_h * (1 - p_h)) / (n_h - 1)
      var_estimador <- sum((pesos_W_h^2 * var_p_h) * fpc_h)
    } else if (parametro == "A") { # Manteniendo parámetro "A" original
      p_h <- media_h
      estimador <- N * sum(pesos_W_h * p_h)
      var_p_h <- (p_h * (1 - p_h)) / (n_h - 1)
      var_estimador <- N^2 * sum((pesos_W_h^2 * var_p_h) * fpc_h)
    } else {
      stop("Parámetro para MAE no reconocido. Usar 'mu', 'tau', 'p' o 'A'")
    }
    
    # --- Cálculo del Intervalo de Confianza para MAE ---
    if (intervalo) {
      error_est <- sqrt(var_estimador)
      grados_libertad <- sum(n_h - L)
      t_critico <- qt(1 - (1 - confianza) / 2, df = grados_libertad)
      ic <- c(estimador - t_critico * error_est, estimador + t_critico * error_est)
    }
    
    # =================================================================
    #           CASO 2: MUESTREO ALEATORIO SIMPLE (MAS)
    #           Se detecta si 'datos_muestrales' es un vector
    # =================================================================
  } else if (is.vector(datos_muestrales)) {
    
    # --- Renombrar variables para claridad en este bloque (usando tu nomenclatura original) ---
    muestra <- datos_muestrales
    N <- info_poblacion
    
    # --- Validaciones para MAS ---
    if (!is.numeric(N) || length(N) != 1) {
      stop("Para MAS, 'info_poblacion' debe ser un único número (N)")
    }
    
    # --- Cálculos para MAS (tu código original) ---
    n <- length(muestra)
    media_muestra <- mean(muestra)
    sd_muestra <- sd(muestra)
    fpc <- sqrt((N - n) / N)
    
    # --- Estimación según el parámetro para MAS ---
    if (parametro == "mu") {
      estimador <- media_muestra
      var_estimador <- ((sd_muestra / sqrt(n)) * fpc)^2
    } else if (parametro == "tau") {
      estimador <- media_muestra * N
      var_estimador <- ((sd_muestra / sqrt(n)) * (N * fpc))^2
    } else if (parametro == "p") {
      p_muestra <- mean(muestra)
      estimador <- p_muestra
      var_estimador <- (sqrt((p_muestra * (1 - p_muestra)) / (n - 1)) * fpc)^2
    } else if (parametro == "A") { # Manteniendo parámetro "A" original
      p_muestra <- mean(muestra)
      estimador <- N * p_muestra
      var_estimador <- (N * sqrt((p_muestra * (1 - p_muestra)) / (n - 1)) * fpc)^2
    } else {
      stop("Parámetro para MAS no reconocido. Usar 'mu', 'tau', 'p', o 'A'")
    }
    
    # --- Cálculo del Intervalo de Confianza para MAS ---
    if (intervalo) {
      error_est <- sqrt(var_estimador)
      alfa <- 1 - confianza
      t_critico <- qt(1 - alfa / 2, df = n - 1)
      ic <- c(estimador - t_critico * error_est, estimador + t_critico * error_est)
    }
    
    # =================================================================
    #           CASO 3: FORMATO DE DATOS INCORRECTO
    # =================================================================
  } else {
    stop("El formato de 'datos_muestrales' no es reconocido. Debe ser un vector (para MAS) o una lista (para MAE)")
  }
  
  # ----------------------------
  #      Retorno de Resultados
  # ----------------------------
  resultado <- list(Estimador = estimador, Varianza_Estimador = var_estimador)
  if (exists("ic") && !is.null(ic)) {
    resultado$IC <- ic
  }
  
  return(resultado)
}