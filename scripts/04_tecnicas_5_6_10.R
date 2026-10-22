# TASK 04: técnicas 5, 6 y 10. Entrada para Rscript o Source.
argumentos_R <- commandArgs(trailingOnly = FALSE)
marca_archivo <- grep("^--file=", argumentos_R, value = TRUE)
archivos_source <- Filter(
  Negate(is.null), lapply(sys.frames(), function(e) e$ofile)
)
este_archivo <- if (length(archivos_source)) {
  tail(archivos_source, 1)[[1]]
} else if (length(marca_archivo)) {
  sub("^--file=", "", marca_archivo[1])
} else {
  NULL
}
if (is.null(este_archivo)) {
  stop("Ejecuta el archivo completo con Source o Rscript.")
}
raiz_daniel <- dirname(dirname(normalizePath(este_archivo, mustWork = TRUE)))

modulo_daniel <- new.env(parent = globalenv())
source(file.path(raiz_daniel, "scripts/task_04/cargar.R"))
cargar_task_04(raiz_daniel, modulo_daniel)

args <- if (length(archivos_source)) {
  character()
} else {
  commandArgs(trailingOnly = TRUE)
}
opciones <- modulo_daniel$leer_argumentos_daniel(args)
if (opciones$ayuda) {
  cat(paste(c(
    "Uso: Rscript scripts/04_tecnicas_5_6_10.R [entrada] [salida] [opciones]",
    "--config archivo.R   Configuración parcial sobre el perfil ECE.",
    "--entrada archivo    RDS, CSV, TSV, SAV, XLSX o XLS.",
    "--salida carpeta     Debe estar vacía o no existir.",
    "--columna nombre     Nombre exacto de la variable numérica.",
    "--sin-validacion     T10 pendiente; T5 y T6 continúan.",
    "--sin-graficos       Sólo tablas, datos y texto.",
    "Entorno: DANIEL_CONFIG, DANIEL_ENTRADA, DANIEL_SALIDA.",
    "Precedencia: argumentos > entorno > configuración indicada > perfil ECE."
  ), collapse = "\n"), "\n")
} else {
  resultado_daniel <- modulo_daniel$ejecutar_daniel(raiz_daniel, opciones)
  cat(
    "\nResultados:", resultado_daniel$salida,
    "\nTécnica 10:", resultado_daniel$validacion$estado, "\n"
  )
  cat(paste0(
    "Se conservaron todas las filas y columnas originales. ",
    "No se eliminaron atípicos.\n"
  ))
}
