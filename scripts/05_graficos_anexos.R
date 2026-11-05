# Exporta figuras de una ejecución guardada, sin repetir la limpieza.
# Uso desde la raíz: Rscript scripts/05_graficos_anexos.R carpeta_resultados
args <- commandArgs(trailingOnly = TRUE)
if (identical(args, "--ayuda")) {
  cat(paste0(
    "Uso: Rscript scripts/05_graficos_anexos.R carpeta_resultados\n",
    "Lee datos, parámetros y conteos de esa misma ejecución.\n",
    "Guarda cuatro PNG de 300 ppp y sus notas en graficos_anexos/.\n",
    "Al repetir el comando se actualizan las figuras generadas.\n"
  ))
} else {
  if (length(args) != 1L) {
    stop("Indica una carpeta de resultados o usa --ayuda.")
  }
  marca <- grep("^--file=", commandArgs(), value = TRUE)
  if (!length(marca)) stop("Ejecuta este archivo mediante Rscript.")
  archivo <- normalizePath(sub("^--file=", "", marca[1]), mustWork = TRUE)
  raiz <- dirname(dirname(archivo))
  for (nombre in c("graficos_atipicos.R", "graficos_anexos.R")) {
    source(file.path(raiz, "scripts/task_04", nombre), encoding = "UTF-8")
  }
  resultado <- exportar_graficos_anexos(args[1])
  cat(
    "Figuras generadas:", length(resultado$graficos),
    "\nCarpeta:", resultado$salida, "\n"
  )
}
