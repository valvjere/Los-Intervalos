# Figuras de una ejecución guardada; no modifica datos ni diagnósticos.
exportar_graficos_anexos <- function(carpeta) {
  carpeta <- normalizePath(carpeta, mustWork = TRUE)
  leer_csv <- function(nombre) {
    ruta <- file.path(carpeta, nombre)
    if (file.exists(ruta)) read.csv(ruta) else data.frame()
  }
  cfg <- dget(file.path(carpeta, "configuracion_resuelta.R"))
  datos <- readRDS(file.path(carpeta, "datos_diagnosticados.rds"))
  limites <- leer_csv("tecnica_05_limites.csv")
  etapas <- leer_csv("etapas_pipeline.csv")
  completa <- nrow(etapas) > 0L
  if (!completa) etapas <- leer_csv("resultado_antes_despues.csv")
  # La trazabilidad debe corresponder a esta salida antes de exportar.
  if (nrow(etapas)) {
    if (!all(c("etapa", "filas") %in% names(etapas)) ||
        anyNA(etapas$filas) || any(!is.finite(etapas$filas)) ||
        any(etapas$filas < 0 | etapas$filas != trunc(etapas$filas)) ||
        any(diff(etapas$filas) > 0) ||
        tail(etapas$filas, 1) != nrow(datos)) {
      stop("Los conteos por etapa no corresponden a la base final.")
    }
  }
  a <- cfg$analisis
  columna <- cfg$columnas$ingreso
  requeridas <- c(columna, "d5_ingreso_utilizable")
  numerica <- is.numeric(datos[[columna]]) ||
    (is.logical(datos[[columna]]) && all(is.na(datos[[columna]])))
  if (!all(requeridas %in% names(datos)) ||
      !numerica ||
      !is.logical(datos$d5_ingreso_utilizable) ||
      anyNA(datos$d5_ingreso_utilizable) || nrow(limites) != 1L) {
    stop("La salida no contiene la variable o el diagnóstico esperado.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Se requiere ggplot2 para exportar las figuras.")
  }
  envolver <- function(x, ancho = 85) {
    paste(strwrap(x, width = ancho), collapse = "\n")
  }
  numero <- function(x) {
    format(x, big.mark = " ", decimal.mark = ",", trim = TRUE,
      scientific = FALSE)
  }
  ece <- all(c("ID_AMO", "ID_TRIMESTRE") %in% names(datos)) &&
    identical(columna, "Ingreso_principal")
  fuente <- if (ece) {
    "Fuente: elaboración propia con microdatos de la ECE (INEC)."
  } else "Fuente: elaboración propia con la base de la ejecución."
  millones <- identical(tolower(a$unidad), "colones")
  etiquetas <- function(x) numero(if (millones) x / 1e6 else x)
  unidad <- if (millones) "millones de colones" else a$unidad
  eje <- paste0(a$etiqueta_variable,
    if (nzchar(unidad)) paste0(" (", unidad, ")") else "")
  estilo <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(size = 10),
      plot.caption = ggplot2::element_text(size = 9, hjust = 0),
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.margin = ggplot2::margin(12, 16, 12, 12),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
  graficos <- list()
  indice <- data.frame(
    archivo = character(), titulo = character(), nota = character()
  )
  agregar <- function(nombre, grafico, titulo, nota, subtitulo = NULL) {
    nombre <- gsub("[^[:alnum:]_-]", "_", nombre)
    nombre <- tail(make.unique(c(names(graficos), nombre), sep = "_"), 1)
    nota <- paste(nota, fuente)
    graficos[[nombre]] <<- grafico + estilo + ggplot2::labs(
      title = envolver(titulo, 65), subtitle = envolver(subtitulo),
      caption = envolver(nota)
    )
    indice <<- rbind(indice, data.frame(
      archivo = paste0(nombre, ".png"), titulo = titulo, nota = nota
    ))
  }
  valido <- datos$d5_ingreso_utilizable
  v <- as.numeric(datos[[columna]])[valido]
  if (any(!is.finite(v))) stop("Hay valores no finitos marcados utilizables.")
  rotulos <- c(
    Nivel_educativo_cat = "nivel educativo",
    Grupo_edad_estudio = "grupo de edad"
  )
  grupos <- intersect(cfg$columnas$grupos, names(rotulos))
  grupos <- intersect(grupos, names(datos))
  if (length(v) && length(grupos)) {
    originales <- graficar_atipicos(
      datos, v, valido, grupos, limites$limite_superior,
      limites$tipo_cuantil, limites$multiplicador_iqr,
      a$etiqueta_variable, a$unidad
    )
    subtitulo <- paste(numero(length(v)),
      "registros utilizables; resultados sin ponderación")
    for (grupo in grupos) {
      nombre <- paste0("boxplot_", grupo)
      # El formato conserva los cuartiles y extremos del cálculo original.
      g <- suppressMessages(originales[[nombre]] +
        ggplot2::scale_y_continuous(labels = etiquetas)) +
        ggplot2::labs(x = NULL, y = eje)
      if (grupo == "Nivel_educativo_cat") {
        g <- suppressMessages(g + ggplot2::scale_x_discrete(
          labels = function(x) vapply(x, envolver, "", ancho = 25)
        )) + ggplot2::coord_flip()
      }
      nota <- paste(
        "Cada caja usa los cuartiles de su grupo; sus puntos extremos",
        "pueden diferir de las alertas globales.",
        "Se conservan todos los ingresos utilizables."
      )
      agregar(nombre, g,
        paste(a$etiqueta_variable, "por", rotulos[[grupo]]), nota, subtitulo)
    }
  }
  banderas <- c(IQR = "d5_atipico_iqr", Z = "d5_atipico_z",
    Ambos = "d5_atipico_ambos")
  disponibles <- banderas[banderas %in% names(datos)]
  conteos <- data.frame(
    criterio = names(disponibles),
    cantidad = vapply(disponibles, function(x) {
      b <- datos[[x]][valido]
      if (length(b) && !anyNA(b)) sum(b) else NA_integer_
    }, integer(1))
  )
  conteos <- conteos[!is.na(conteos$cantidad), , drop = FALSE]
  if (nrow(conteos)) {
    conteos$criterio <- factor(conteos$criterio, levels = names(banderas))
    g <- ggplot2::ggplot(conteos,
      ggplot2::aes(criterio, cantidad)) +
      ggplot2::geom_col(fill = "#276B8E", width = .6) +
      ggplot2::geom_text(ggplot2::aes(label = numero(cantidad)),
        vjust = -.5, size = 3.5) +
      ggplot2::scale_y_continuous(labels = numero,
        expand = ggplot2::expansion(mult = c(0, .16))) +
      ggplot2::labs(x = "Criterio de revisión", y = "Registros señalados")
    agregar("atipicos_conteos", g, "Ingresos señalados como atípicos",
      paste("Ambos es la intersección de IQR y Z: las barras no se suman.",
        "Son alertas para revisión; los registros se conservaron."),
      paste0(numero(length(v)), " registros; regla de ",
        numero(limites$multiplicador_iqr), "·IQR; |Z| > ",
        numero(limites$corte_z), ". Sin ponderación."))
  }
  if (nrow(etapas)) {
    etapas$etapa <- factor(etapas$etapa, levels = rev(unique(etapas$etapa)))
    g <- ggplot2::ggplot(etapas, ggplot2::aes(etapa, filas)) +
      ggplot2::geom_col(fill = "#276B8E", width = .65) +
      ggplot2::geom_text(ggplot2::aes(label = numero(filas)),
        hjust = -.1, size = 3.5) +
      ggplot2::scale_y_continuous(labels = numero,
        expand = ggplot2::expansion(mult = c(0, .25))) +
      ggplot2::scale_x_discrete(labels = function(x) {
        vapply(x, envolver, "", ancho = 23)
      }) + ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "Registros conservados")
    titulo <- if (completa) "Registros antes y después de la limpieza" else
      "Diagnóstico y transformación: antes y después"
    nota <- if (completa) paste(
      "La reducción incluye delimitación poblacional y depuración;",
      "no equivale exclusivamente a errores."
    ) else paste("Comparación de entrada y salida del módulo final;",
      "no representa la limpieza completa.")
    agregar("antes_despues", g, titulo, nota)
  }
  salida <- file.path(carpeta, "graficos_anexos")
  dir.create(salida, showWarnings = FALSE)
  # Retira únicamente las figuras adicionales de la versión anterior.
  anteriores <- c(
    "histograma_variable", "boxplot_global", "boxplot_Sexo_cat",
    "boxplot_ID_AMO", "boxplot_ID_TRIMESTRE", "tecnica_06_log",
    "tecnica_10_control_inec"
  )
  unlink(file.path(salida, paste0(anteriores, ".png")))
  for (nombre in names(graficos)) {
    alto <- if (grepl("Nivel_educativo|antes_despues", nombre)) 5.5 else 4.8
    ggplot2::ggsave(file.path(salida, paste0(nombre, ".png")),
      graficos[[nombre]], width = 6.65, height = alto, dpi = 300, bg = "white")
  }
  write.csv(indice, file.path(salida, "indice_figuras.csv"),
    row.names = FALSE, fileEncoding = "UTF-8")
  invisible(list(salida = salida, graficos = graficos, indice = indice))
}
