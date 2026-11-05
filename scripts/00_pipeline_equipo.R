# Pipeline del equipo. Ejecución desde la raíz del repositorio.
# Uso: Rscript scripts/00_pipeline_equipo.R [carpeta_salida]
raiz <- normalizePath(".")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("Se admite una carpeta de salida opcional.")
etapas <- c(
  "scripts/01_ingesta_y_fusion.R", "scripts/JEREMYSCRIPT.R",
  "scripts/03_faltantes_y_categorias.R"
)
for (archivo in etapas) sys.source(archivo, envir = new.env())
source("scripts/task_04/cargar.R")
cargar_task_04(raiz)

carpeta <- "data/procesados"
entrada <- file.path(carpeta, "ece_2024_2025_preparado.rds")
crudo <- readRDS(file.path(carpeta, "ece_2024_2025_consolidado_crudo.rds"))
tipos <- readRDS(file.path(carpeta, "ece_2024_2025_paso2_filtrado_tipos.rds"))
preparado <- readRDS(entrada)
llave <- c(
  "ID_AMO", "ID_TRIMESTRE", "Consecutivo", "ID_VIVIENDA", "ID_HOGAR",
  "ID_LINEA"
)
# La comprobación no modifica valores, tipos ni atributos de la entrada.
for (datos in list(crudo, tipos, preparado)) {
  if (!all(llave %in% names(datos)) || anyNA(datos[llave]) ||
      anyDuplicated(datos[llave])) {
    stop("Las llaves de cada etapa deben ser completas y únicas.")
  }
}
if (nrow(dplyr::anti_join(preparado[llave], tipos[llave], by = llave))) {
  stop("La base preparada contiene registros ajenos al filtrado anterior.")
}
poblacion <- with(crudo, sum(
  Condicion_actividad == 1 & Edad >= 18 & Edad <= 65 & Sexo %in% c(1, 2),
  na.rm = TRUE
))
config <- list(
  entrada = entrada, filas_iniciales = nrow(crudo),
  estado = paste(
    "Base preparada tras delimitación poblacional, consistencia,",
    "tratamiento de faltantes y codificación de categorías."
  )
)
archivo_config <- file.path(carpeta, "configuracion_pipeline_task04.R")
dput(config, archivo_config)
# Las figuras para el informe se exportan una sola vez al final.
opciones <- c(
  "--config", archivo_config, "--entrada", entrada, "--sin-graficos"
)
if (length(args)) opciones <- c(opciones, "--salida", args[1])
resultado <- ejecutar_daniel(raiz, leer_argumentos_daniel(opciones))
salida <- resultado$salida
final <- readRDS(file.path(salida, "datos_diagnosticados.rds"))
resumen <- data.frame(
  etapa = c(
    "Consolidación", "Delimitación poblacional", "Consistencia y tipos",
    "Faltantes y categorías", "Diagnóstico y transformación"
  ),
  filas = c(nrow(crudo), poblacion, nrow(tipos), nrow(preparado), nrow(final))
)
resumen$porcentaje_del_total <- 100 * resumen$filas / nrow(crudo)
write.csv(resumen, file.path(salida, "etapas_pipeline.csv"), row.names = FALSE)
fuentes <- c(etapas, "scripts/00_pipeline_equipo.R",
  "scripts/05_graficos_anexos.R", archivo_config,
  file.path(carpeta, c("ece_2024_2025_consolidado_crudo.rds",
    "ece_2024_2025_paso2_filtrado_tipos.rds", "ece_2024_2025_preparado.rds")))
write.csv(data.frame(
  archivo = normalizePath(fuentes), md5 = unname(tools::md5sum(fuentes))
), file.path(salida, "trazabilidad_pipeline.csv"), row.names = FALSE)
exportar_graficos_anexos(salida)
cat("\nPipeline completo. Resultados:", salida, "\n")
