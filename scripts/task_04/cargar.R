# Carga las funciones de TASK 04 sin ejecutar el análisis.
cargar_task_04 <- function(raiz, entorno = parent.frame()) {
  archivos <- c(
    "configuracion_y_lectura.R",
    "graficos_atipicos.R",
    "tecnica_05_atipicos.R",
    "tecnica_06_transformacion.R",
    "configuracion_inec.R",
    "microdatos_inec.R",
    "referencia_inec.R",
    "tecnica_10_validacion_inec.R",
    "reporte.R",
    "resultados.R",
    "flujo.R"
  )
  for (archivo in archivos) {
    sys.source(file.path(raiz, "scripts/task_04", archivo), envir = entorno)
  }
}
