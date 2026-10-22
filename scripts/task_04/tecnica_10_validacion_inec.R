# Técnica 10: control nacional de ingreso bruto, independiente de T5/T6.
tecnica_10_validacion_inec <- function(
  raiz, tolerancia_colones = 1,
  config = list()
) {
  comparacion <- data.frame(
    anio = integer(), trimestre = integer(),
    periodo = character(), grupo = character(), n_registros = integer(),
    suma_ponderaciones = numeric(), media_bruta_ponderada = numeric(),
    media_bruta_inec = numeric(), diferencia_colones = numeric(),
    diferencia_porcentaje = numeric(), tolerancia_colones = numeric(),
    coincide_redondeo = logical(), archivo_fuente = character(),
    hoja = character(), celda = character(), nota_metodologica = character(),
    alcance = character(), estado = character(), diagnostico = character()
  )
  inventario <- data.frame(
    archivo = character(), anio = integer(),
    trimestre = integer(), filas_crudas = integer(),
    columnas_crudas = integer(),
    md5 = character(), estado = character(), diagnostico = character()
  )
  fuentes <- data.frame(
    archivo = character(), hoja = character(),
    celda = character(), titulo = character(), uso = character()
  )
  referencia <- NA_character_
  avisos <- character()

  # La cobertura completa no implica coincidencia numérica de todas las filas.
  terminar <- function(estado_forzado = NULL) {
    hechas <- comparacion$estado == "comparado"
    fallidas <- comparacion$estado == "fallido"
    pendientes <- comparacion$estado == "pendiente"
    estado <- if (!is.null(estado_forzado)) {
      estado_forzado
    } else if (any(fallidas)) {
      "fallido"
    } else if (length(hechas) && all(hechas)) {
      "completo"
    } else if (any(hechas)) {
      "parcial"
    } else {
      "pendiente"
    }
    if (any(hechas & !comparacion$coincide_redondeo, na.rm = TRUE)) {
      avisos <<- c(avisos, paste(
        paste0(
          "Hay diferencias con INEC mayores a la ",
          "tolerancia; revisar definiciones,"
        ),
        paste0(
          "períodos y ponderaciones. No ajustar ",
          "ingresos para forzar coincidencia."
        )
      ))
    }
    list(
      comparacion = comparacion, inventario = inventario, fuentes = fuentes,
      referencia = referencia, estado = estado, avisos = unique(avisos),
      cobertura = list(
        esperadas = nrow(comparacion), comparadas = sum(hechas),
        coinciden = sum(comparacion$coincide_redondeo, na.rm = TRUE),
        pendientes = sum(pendientes), fallidas = sum(fallidas)
      )
    )
  }

  opciones <- validar_config_inec(raiz, tolerancia_colones, config)
  cfg <- opciones$cfg
  errores <- opciones$errores
  if (length(errores)) {
    avisos <- paste0("Configuración T10: ", errores)
    return(terminar("fallido"))
  }

  ruta <- function(p) {
    if (grepl(
      "^(/|~|[A-Za-z]:[/\\\\])",
      p
    )) {
      path.expand(p)
    } else {
      file.path(raiz, p)
    }
  }
  referencia <- ruta(cfg$referencia)
  carpeta_microdatos <- ruta(cfg$carpeta_microdatos)
  # La tabla incluye períodos pendientes en el denominador.
  celda_nota <- paste0(letra_excel(cfg$columna_rotulo), cfg$fila_nota)
  for (anio in cfg$anios) {
    for (trimestre in cfg$trimestres) {
      for (grupo in names(cfg$hojas)) {
        comparacion[nrow(comparacion) + 1L, ] <- list(
          anio, trimestre,
          paste(cfg$prefijos_periodo[trimestre], anio), grupo, NA_integer_,
          NA_real_,
          NA_real_, NA_real_, NA_real_, NA_real_, tolerancia_colones, NA,
          basename(referencia), cfg$hojas[[grupo]], NA_character_,
          paste0(
            celda_nota,
            ": bruto corriente; ocupados con ingreso conocido positivo"
          ),
          paste0(
            "Control nacional ECE; no valida ",
            "directamente el ingreso neto de la ", "submuestra"
          ),
          "pendiente", "Comparación todavía no realizada."
        )
      }
    }
  }
  marcar <- function(indices, estado, mensaje) {
    comparacion$estado[indices] <<- estado
    comparacion$diagnostico[indices] <<- mensaje
    avisos <<- c(avisos, mensaje)
  }
  if (!cfg$habilitada) {
    marcar(
      seq_len(nrow(comparacion)), "pendiente",
      "T10 deshabilitada por configuración; no se realizó validación externa."
    )
    return(terminar())
  }
  if (!file.exists(referencia)) {
    marcar(
      seq_len(nrow(comparacion)), "pendiente",
      paste("Falta la referencia oficial:", referencia)
    )
    return(terminar())
  }
  dependencias <- c("readxl", "haven")
  faltantes <- dependencias[!vapply(dependencias, requireNamespace,
    logical(1),
    quietly = TRUE
  )]
  if (length(faltantes)) {
    marcar(
      seq_len(nrow(comparacion)), "pendiente",
      paste("Faltan paquetes para T10:", paste(faltantes, collapse = ", "))
    )
    return(terminar())
  }

  cuadros <- list()
  for (grupo in names(cfg$hojas)) {
    indices <- which(comparacion$grupo == grupo)
    cuadro <- tryCatch(
      leer_cuadro_inec(referencia, cfg$hojas[[grupo]], cfg), error = identity
    )
    if (inherits(cuadro, "error")) {
      marcar(indices, "fallido", conditionMessage(cuadro))
      next
    }
    cuadros[[grupo]] <- cuadro
    fuentes[nrow(fuentes) + 1L, ] <- list(
      basename(referencia), cfg$hojas[[grupo]], celda_nota,
      cuadro[cfg$fila_nota, cfg$columna_rotulo],
      "Ingreso bruto corriente: control nacional ECE por período y grupo."
    )
  }
  for (anio in cfg$anios) {
    for (trimestre in cfg$trimestres) {
      nombre <- gsub("{anio}", anio, cfg$patron_archivo, fixed = TRUE)
      nombre <- gsub(
        "{trimestre}", cfg$romanos[trimestre], nombre, fixed = TRUE
      )
      indices <- which(comparacion$anio == anio &
        comparacion$trimestre == trimestre & comparacion$estado != "fallido")
      if (!length(indices)) next
      micro <- leer_microdatos_inec(
        file.path(carpeta_microdatos, nombre), nombre, anio, trimestre, cfg
      )
      inventario <- rbind(inventario, micro$inventario)
      if (micro$inventario$estado != "leido") {
        marcar(indices, micro$inventario$estado, micro$inventario$diagnostico)
        next
      }
      for (i in indices) {
        fila <- comparar_ingresos_inec(
          comparacion[i, ], cuadros[[comparacion$grupo[i]]], micro, cfg
        )
        comparacion[i, ] <- fila
        if (fila$estado != "comparado") avisos <- c(avisos, fila$diagnostico)
      }
    }
  }
  terminar()
}
