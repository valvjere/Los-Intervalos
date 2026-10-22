# Secuencia: configuración, lectura, T5, T6, T10 y exportación.
leer_argumentos_daniel <- function(args) {
  opciones <- list(
    ayuda = FALSE, config = NULL, entrada = NULL, salida = NULL,
    columna = NULL, sin_validacion = FALSE, sin_graficos = FALSE
  )
  i <- 1L
  posicionales <- character()
  while (i <= length(args)) {
    a <- args[i]
    if (a %in% c("--config", "--entrada", "--salida", "--columna")) {
      if (i == length(args) || startsWith(args[i + 1], "--")) {
        stop("Falta valor después de ", a)
      }
      clave <- sub("^--", "", a)
      if (!is.null(opciones[[clave]])) stop("Opción repetida: ", a)
      opciones[[clave]] <- args[i + 1]
      i <- i + 2L
    } else if (a %in% c("--ayuda", "--sin-validacion", "--sin-graficos")) {
      clave <- gsub("-", "_", sub("^--", "", a))
      opciones[[clave]] <- TRUE
      i <- i + 1L
    } else if (startsWith(a, "--")) {
      stop("Opción desconocida: ", a)
    } else {
      posicionales <- c(posicionales, a)
      i <- i + 1L
    }
  }
  if (length(posicionales) > 2) {
    stop("Máximo dos argumentos posicionales: entrada y salida.")
  }
  for (j in seq_along(posicionales)) {
    clave <- c("entrada", "salida")[j]
    if (!is.null(opciones[[clave]])) {
      stop("La opción ", clave, " se indicó dos veces.")
    }
    opciones[[clave]] <- posicionales[j]
  }
  opciones
}

ejecutar_daniel <- function(
    raiz, opciones = leer_argumentos_daniel(character())) {
  # Precedencia: argumentos > entorno > configuración > perfil ECE.
  explicito <- function(clave, entorno) {
    if (!is.null(opciones[[clave]])) {
      return(opciones[[clave]])
    }
    v <- Sys.getenv(entorno, "")
    if (nzchar(v)) v else NULL
  }
  archivo_cfg <- explicito("config", "DANIEL_CONFIG")
  if (!is.null(archivo_cfg)) {
    archivo_cfg <- normalizePath(archivo_cfg, mustWork = TRUE)
  }
  cambios <- list()
  for (clave in c("entrada", "salida")) {
    v <- explicito(clave, paste0("DANIEL_", toupper(clave)))
    if (!is.null(v)) cambios[[clave]] <- ruta_desde(v, getwd())
  }
  if (!is.null(opciones$columna)) {
    cambios$columnas <- list(ingreso = opciones$columna)
  }
  if (opciones$sin_validacion) cambios$validacion <- list(habilitada = FALSE)
  if (opciones$sin_graficos) cambios$analisis <- list(generar_graficos = FALSE)
  cfg <- leer_configuracion(raiz, archivo_cfg, cambios)
  entrada <- cfg$entrada
  if (is.null(entrada)) {
    candidatas <- vapply(cfg$candidatas, ruta_desde, character(1), base = raiz)
    disponibles <- candidatas[file.exists(candidatas)]
    if (!length(disponibles)) {
      stop("No hay entrada. Ejecuta la ingesta o indica --entrada.")
    }
    entrada <- disponibles[1]
  }
  entrada <- normalizePath(ruta_desde(entrada, raiz), mustWork = TRUE)
  if (is.null(cfg$salida)) {
    salida <- file.path(
      raiz, "Resultados_TASK_04", format(Sys.time(), "%Y%m%d_%H%M%S")
    )
    indice <- 1L
    inicial <- salida
    while (file.exists(salida)) {
      salida <- paste0(inicial, "_", indice)
      indice <- indice + 1L
    }
  } else {
    salida <- ruta_desde(cfg$salida, raiz)
  }
  if (file.exists(salida) && !dir.exists(salida)) {
    stop("La salida es un archivo existente.")
  }
  if (dir.exists(salida) &&
      length(list.files(salida, all.files = TRUE, no.. = TRUE))) {
    stop(paste0(
      "La salida contiene archivos. Elige otra carpeta; no se ",
      "sobrescriben resultados."
    ))
  }

  original <- leer_base_daniel(entrada, cfg$formato)
  columna <- resolver_columna(original, cfg$columnas)
  a <- cfg$analisis
  cat("Entrada:", entrada, "\nVariable:", columna, "\n", cfg$estado, "\n")
  t5 <- tecnica_05_atipicos(original, columna, a$multiplicador_iqr, a$corte_z,
    codigos_faltantes = a$codigos_faltantes, solo_positivos = a$solo_positivos,
    columnas_grupo = cfg$columnas$grupos,
    etiqueta_variable = a$etiqueta_variable,
    unidad = a$unidad, generar_graficos = a$generar_graficos,
    tipo_cuantil = a$tipo_cuantil
  )
  t6 <- tecnica_06_transformacion(
    t5$datos, columna, a$etiqueta_variable, a$unidad, a$generar_graficos
  )
  validacion_cfg <- cfg$validacion
  tolerancia <- validacion_cfg$tolerancia_colones
  validacion_cfg$tolerancia_colones <- NULL
  t10 <- tecnica_10_validacion_inec(
    raiz, tolerancia_colones = tolerancia, config = validacion_cfg
  )
  diagnosticados <- t6$datos
  stopifnot(nrow(diagnosticados) == nrow(original))
  for (n in names(original)) {
    stopifnot(identical(diagnosticados[[n]], original[[n]]))
  }
  exportar_resultados(
    raiz, entrada, archivo_cfg, salida, cfg, columna, original, t5, t6, t10
  )
  invisible(list(
    salida = normalizePath(salida), validacion = t10, tecnica5 = t5$resumen,
    tecnica6 = t6$resumen
  ))
}
