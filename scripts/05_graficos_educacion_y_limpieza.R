# Genera únicamente dos figuras para el informe; no modifica las bases.
# Uso desde la raíz: Rscript scripts/05_graficos_educacion_y_limpieza.R
# Argumentos opcionales: carpeta_datos carpeta_salida.
# Requiere ggplot2 y las salidas de estas etapas del repositorio:
# Rscript scripts/01_ingesta_y_fusion.R
# Rscript scripts/JEREMYSCRIPT.R
# Rscript scripts/03_faltantes_y_categorias.R

crear_graficos <- function(carpeta_datos, salida) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Falta ggplot2. Instálalo con install.packages('ggplot2').")
  }
  leer <- function(nombre) {
    ruta <- file.path(carpeta_datos, nombre)
    if (!file.exists(ruta)) {
      stop("Falta ", ruta, ". Ejecuta primero las etapas de limpieza.")
    }
    readRDS(ruta)
  }
  inicial <- leer("ece_2024_2025_consolidado_crudo.rds")
  final <- leer("ece_2024_2025_preparado.rds")
  llave <- c("ID_AMO", "ID_TRIMESTRE", "Consecutivo", "ID_VIVIENDA",
    "ID_HOGAR", "ID_LINEA")
  if (!is.data.frame(inicial) || !is.data.frame(final) ||
      !all(llave %in% names(inicial)) ||
      !all(c(llave, "Ingreso_principal", "Nivel_educativo_cat") %in%
        names(final)) || !nrow(inicial) || !nrow(final)) {
    stop("Las bases deben contener registros, identificadores y variables.")
  }
  # Una comparación antes/después exige registros del mismo consolidado.
  claves <- function(base) {
    if (anyNA(base[llave])) stop("Hay identificadores incompletos.")
    ids <- do.call(paste, c(unname(as.list(base[llave])), sep = "|"))
    if (anyDuplicated(ids)) stop("Hay identificadores repetidos.")
    ids
  }
  if (!all(claves(final) %in% claves(inicial))) {
    stop("La base preparada contiene registros ajenos al consolidado.")
  }
  ingreso <- final$Ingreso_principal
  educacion <- final$Nivel_educativo_cat
  if (!is.numeric(ingreso) || any(!is.finite(ingreso) | ingreso <= 0 |
      ingreso == 99999999) || !is.factor(educacion) || anyNA(educacion)) {
    stop("La base preparada debe tener ingresos y niveles educativos válidos.")
  }
  numero <- function(x) {
    format(x, big.mark = " ", decimal.mark = ",", trim = TRUE,
      scientific = FALSE)
  }
  envolver <- function(x, ancho = 85) {
    vapply(x, function(s) paste(strwrap(s, width = ancho), collapse = "\n"),
      character(1))
  }
  fuente <- "Fuente: elaboración propia con microdatos de la ECE (INEC)."
  estilo <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(size = 10),
      plot.caption = ggplot2::element_text(size = 9, hjust = 0),
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.margin = ggplot2::margin(12, 16, 12, 12),
      panel.grid.minor = ggplot2::element_blank()
    )
  caja <- list(coef = 1.5, fill = "#D8EAF2", outlier.alpha = .25)
  # Versiones anteriores de ggplot2 usan por defecto cuantiles tipo 7.
  if ("quantile.type" %in% ggplot2::StatBoxplot$parameters()) {
    caja$quantile.type <- 7L
  }
  g_educacion <- ggplot2::ggplot(
    data.frame(educacion, ingreso), ggplot2::aes(educacion, ingreso)
  ) + do.call(ggplot2::geom_boxplot, caja) +
    ggplot2::scale_x_discrete(labels = function(x) envolver(x, 25)) +
    ggplot2::scale_y_continuous(labels = function(x) numero(x / 1e6)) +
    ggplot2::coord_flip() + estilo + ggplot2::labs(
      title = "Ingreso mensual neto por nivel educativo",
      subtitle = paste(numero(nrow(final)), "registros; sin ponderación"),
      x = NULL, y = "Ingreso mensual neto (millones de colones)",
      caption = envolver(paste(
        "Ingreso del empleo principal. Cada caja usa cuartiles de su grupo.",
        "Se conservan todos los ingresos, incluidos los extremos.", fuente
      ))
    )
  resumen <- data.frame(
    etapa = factor(c("Consolidación inicial", "Después de la limpieza"),
      levels = c("Después de la limpieza", "Consolidación inicial")),
    registros = c(nrow(inicial), nrow(final))
  )
  g_registros <- ggplot2::ggplot(resumen, ggplot2::aes(etapa, registros)) +
    ggplot2::geom_col(fill = "#276B8E", width = .6) +
    ggplot2::geom_text(ggplot2::aes(label = numero(registros)),
      hjust = -.1, size = 3.5) +
    ggplot2::scale_y_continuous(labels = numero,
      expand = ggplot2::expansion(mult = c(0, .25))) +
    ggplot2::scale_x_discrete(labels = function(x) envolver(x, 21)) +
    ggplot2::coord_flip() + estilo + ggplot2::labs(
      title = "Registros antes y después de la limpieza",
      x = NULL, y = "Registros persona-trimestre",
      caption = envolver(paste(
        "La reducción incluye selección de población y depuración;",
        "no equivale exclusivamente a errores. El diagnóstico de atípicos",
        "conserva los registros de la base preparada.", fuente
      ))
    )
  graficos <- list(
    ingreso_por_nivel_educativo = g_educacion,
    registros_antes_despues = g_registros
  )
  dir.create(salida, recursive = TRUE, showWarnings = FALSE)
  for (nombre in names(graficos)) {
    ggplot2::ggsave(file.path(salida, paste0(nombre, ".png")),
      graficos[[nombre]], width = 6.65, height = 5.5, dpi = 300, bg = "white")
  }
  invisible(list(graficos = graficos, resumen = resumen))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (identical(args, "--ayuda")) {
    cat(paste0(
      "Uso: Rscript scripts/05_graficos_educacion_y_limpieza.R ",
      "[carpeta_datos] [carpeta_salida]\n",
      "Por defecto: data/procesados y Resultados_TASK_04/graficos_informe.\n",
      "Exporta únicamente educación-ingreso y registros antes/después.\n"
    ))
  } else {
    if (length(args) > 2L) stop("Se admiten dos rutas opcionales.")
    marca <- grep("^--file=", commandArgs(), value = TRUE)
    archivo <- normalizePath(sub("^--file=", "", marca[1]), mustWork = TRUE)
    raiz <- dirname(dirname(archivo))
    entrada <- if (length(args)) args[1] else file.path(raiz, "data/procesados")
    salida <- if (length(args) == 2L) args[2] else
      file.path(raiz, "Resultados_TASK_04/graficos_informe")
    crear_graficos(entrada, salida)
    cat("Dos figuras guardadas en:", normalizePath(salida), "\n")
  }
}
