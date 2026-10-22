# Técnica 5: IQR y Z. Las alertas se conservan para revisión.
tecnica_05_atipicos <- function(datos, columna = "Ingreso_principal",
                                multiplicador_iqr = 1.5, corte_z = 3,
                                codigos_faltantes = c(0, 99999999),
                                solo_positivos = TRUE, columnas_grupo = NULL,
                                etiqueta_variable = "Ingreso",
                                unidad = "colones",
                                generar_graficos = TRUE, tipo_cuantil = 7L) {
  texto_escalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  if (!is.data.frame(datos) || !texto_escalar(columna) ||
    !columna %in% names(datos)) {
    stop("T5: falta la tabla o su columna numérica: ", paste(columna,
      collapse = ", "
    ))
  }
  # Una columna vacía puede importarse como logical; TRUE/FALSE se rechaza.
  numerica_o_vacia <- function(x) {
    is.numeric(x) || (is.logical(x) &&
      all(is.na(x)))
  }
  if (!numerica_o_vacia(datos[[columna]])) {
    stop(paste0(
      "T5: la variable debe ser numérica. No conviertas un ",
      "factor con as.numeric()."
    ))
  }
  if (any(grepl("^d[56]_", names(datos)))) {
    stop(paste0(
      "La entrada ya fue diagnosticada. Utiliza la base ",
      "anterior al aporte de Daniel."
    ))
  }
  umbral_valido <- function(x) {
    is.numeric(x) && length(x) == 1L &&
      is.finite(x) && x > 0
  }
  if (!umbral_valido(multiplicador_iqr) || !umbral_valido(corte_z)) {
    stop("T5: los umbrales deben ser números positivos y finitos.")
  }
  if (!is.numeric(tipo_cuantil) || length(tipo_cuantil) != 1L ||
    !is.finite(tipo_cuantil) || tipo_cuantil != trunc(tipo_cuantil) ||
    tipo_cuantil < 1L || tipo_cuantil > 9L) {
    stop("T5: tipo_cuantil debe ser un entero entre 1 y 9.")
  }
  tipo_cuantil <- as.integer(tipo_cuantil)
  booleano <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)
  if (!booleano(solo_positivos) || !booleano(generar_graficos)) {
    stop("T5: solo_positivos y generar_graficos deben ser TRUE o FALSE.")
  }
  if (!is.null(codigos_faltantes) && !is.numeric(codigos_faltantes)) {
    stop(paste0(
      "T5: codigos_faltantes debe ser numérico; usa ",
      "numeric(0) si no hay códigos."
    ))
  }
  if (any(!is.finite(codigos_faltantes))) {
    stop(paste0(
      "T5: codigos_faltantes sólo admite números finitos; ",
      "los NA/Inf se detectan automáticamente."
    ))
  }
  if (!is.null(columnas_grupo) &&
    (!is.character(columnas_grupo) || anyNA(columnas_grupo))) {
    stop("T5: columnas_grupo debe contener nombres de columnas o ser NULL.")
  }
  if (!texto_escalar(etiqueta_variable) || !texto_escalar(unidad)) {
    stop("T5: etiqueta_variable y unidad deben ser textos únicos.")
  }
  avisos <- character()
  # Los códigos faltantes dependen de la fuente. El original se conserva.
  x <- as.numeric(datos[[columna]])
  es_codigo <- x %in% codigos_faltantes
  valido <- is.finite(x) & !es_codigo & (!solo_positivos | x > 0)
  v <- x[valido]
  n <- length(v)
  datos$d5_fila_entrada <- seq_len(nrow(datos))
  datos$d5_ingreso_utilizable <- valido
  datos$d5_motivo_ingreso <- ifelse(!is.finite(x), "Faltante o no finito",
    ifelse(es_codigo, "Código de no respuesta/no aplica",
      ifelse(solo_positivos & x <= 0, "No positivo", "Utilizable")
    )
  )
  # IQR = Q3 - Q1; límites Q1 - k*IQR y Q3 + k*IQR.
  # type=7 reproduce Horgan (2009), pp. 16–17; otros tipos pueden diferir.
  q <- c(NA_real_, NA_real_)
  amplitud <- inferior <- superior <- NA_real_
  datos$d5_atipico_iqr <- rep(NA, nrow(datos))
  if (n >= 2L) {
    q <- quantile(v, c(.25, .75), type = tipo_cuantil, names = FALSE)
    amplitud <- q[2] - q[1]
    inferior <- q[1] - multiplicador_iqr * amplitud
    superior <- q[2] + multiplicador_iqr * amplitud
    if (all(is.finite(c(q, amplitud, inferior, superior)))) {
      datos$d5_atipico_iqr[valido] <- v < inferior | v > superior
      if (amplitud == 0) {
        avisos <- c(
          avisos,
          paste0(
            "IQR igual a cero: hay empates; interpretar sus ",
            "alertas con precaución."
          )
        )
      }
    } else {
      avisos <- c(avisos, "IQR no evaluable: sus límites no son finitos.")
    }
  } else {
    avisos <- c(
      avisos,
      "IQR no evaluable: menos de dos observaciones utilizables."
    )
  }
  # Z = (x - media) / s, con s muestral (n-1).
  # El corte es exploratorio; no supone normalidad ni demuestra un error.
  promedio <- if (n > 0L) mean(v) else NA_real_
  desviacion <- if (n >= 2L) sd(v) else NA_real_
  datos$d5_z_diagnostico <- rep(NA_real_, nrow(datos))
  datos$d5_atipico_z <- rep(NA, nrow(datos))
  if (n >= 2L && is.finite(promedio) && is.finite(desviacion) &&
    desviacion > 0) {
    z <- (v - promedio) / desviacion
    z[!is.finite(z)] <- NA_real_
    datos$d5_z_diagnostico[valido] <- z
    datos$d5_atipico_z[valido] <- abs(z) > corte_z
    if (anyNA(z)) {
      avisos <- c(avisos, paste0(
        "Z no evaluable en algunos casos por desbordamiento ",
        "numérico."
      ))
    }
  } else {
    avisos <- c(
      avisos,
      paste0(
        "Z no evaluable: se requieren al menos dos ",
        "observaciones y desviación finita mayor que cero."
      )
    )
  }
  # La unión evita duplicados. NA indica una regla no evaluable.
  datos$d5_atipico_alguno <- datos$d5_atipico_iqr | datos$d5_atipico_z
  datos$d5_atipico_ambos <- datos$d5_atipico_iqr & datos$d5_atipico_z
  banderas <- list(
    IQR = datos$d5_atipico_iqr, Z = datos$d5_atipico_z,
    Ambos = datos$d5_atipico_ambos, Alguno = datos$d5_atipico_alguno
  )
  contar <- function(b) {
    b <- b[valido]
    if (!length(b) || anyNA(b)) NA_integer_ else sum(b)
  }
  evaluados <- vapply(
    banderas, function(b) sum(!is.na(b[valido])),
    integer(1)
  )
  resumen <- data.frame(
    tecnica = 5, criterio = names(banderas),
    cantidad = unname(vapply(banderas, contar, integer(1))),
    denominador_ingresos_utilizables = n,
    n_evaluados = unname(evaluados),
    estado = ifelse(n == 0L | evaluados == 0L, "no evaluable",
      ifelse(evaluados < n, "parcialmente evaluable", "evaluable")
    ),
    accion = "Marcados para revisión; no eliminados", row.names = NULL
  )
  resumen$porcentaje <- if (n > 0L) 100 * resumen$cantidad / n else NA_real_
  parametros <- data.frame(
    q1 = q[1], q3 = q[2], iqr = amplitud,
    multiplicador_iqr, limite_inferior = inferior, limite_superior = superior,
    media = promedio, desviacion_muestral = desviacion, corte_z, tipo_cuantil
  )
  grupos <- unique(columnas_grupo)
  ausentes <- setdiff(grupos, names(datos))
  if (length(ausentes)) {
    avisos <- c(
      avisos,
      paste("Grupos omitidos porque no existen:", paste(ausentes,
        collapse = ", "
      ))
    )
  }
  grupos <- intersect(grupos, names(datos))
  graficos <- list()
  if (generar_graficos && n > 0L) {
    graficos <- graficar_atipicos(
      datos, v, valido, grupos, superior, tipo_cuantil, multiplicador_iqr,
      etiqueta_variable, unidad
    )
  } else if (generar_graficos && n == 0L) {
    avisos <- c(
      avisos,
      "Gráficos omitidos: no hay observaciones utilizables."
    )
  }
  # T6 comprueba que el diagnóstico corresponde a la misma variable.
  attr(datos, "daniel_variable") <- columna
  list(
    datos = datos, resumen = resumen, parametros = parametros,
    graficos = graficos, avisos = unique(avisos)
  )
}
