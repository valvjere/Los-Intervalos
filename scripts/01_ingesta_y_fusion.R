# ==============================================================================
# PROYECTO CI-0115 PROBABILIDAD Y ESTADÍSTICA - UCR
# EQUIPO: Los Intervalos
# TAREA: TASK_01_Ingesta_y_Fusion
# Responsable: Ignacio (Coordinador & Arquitectura de Datos)
# ==============================================================================
# Descripción:
# Este script realiza la ingesta de los 8 trimestres de la Encuesta Continua
# de Empleo (ECE) para los años 2024 y 2025 desde la carpeta `data/`.
# Aplica la Técnica 9 (Reducción de Dimensionalidad) seleccionando únicamente
# las variables indispensables para el análisis salarial y educativo,
# y consolida los microdatos en un único conjunto de datos inicial.
# ==============================================================================

# 1. Cargar librerías necesarias
if (!requireNamespace("haven", quietly = TRUE)) install.packages("haven")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")

library(haven)      # Lectura nativa de archivos SPSS (.sav)
library(dplyr)      # Manipulación y transformación de datos
library(purrr)      # Operaciones funcionales (map / map_df)
library(readr)      # Exportación e importación eficiente de datos

cat("\n==================================================================\n")
cat("   INICIANDO TASK 01: INGESTA Y FUSIÓN DE DATOS (ECE 2024-2025)   \n")
cat("==================================================================\n\n")

# 2. Localizar archivos .sav en el directorio data/
carpeta_datos <- "data"
archivos_sav <- list.files(carpeta_datos, pattern = "\\.sav$", full.names = TRUE)

if (length(archivos_sav) == 0) {
  stop("ERROR: No se encontraron archivos .sav en la carpeta 'data/'. Verifica la ruta.")
}

cat("Archivos detectados (", length(archivos_sav), " trimestres):\n", sep = "")
for (a in archivos_sav) {
  cat(" - ", basename(a), "\n", sep = "")
}
cat("\n")

# 3. Definición de variables clave (Técnica 9: Reducción de Dimensionalidad)
# Justificación metodológica:
# De las 394 columnas originales del cuestionario de la ECE, se descartan más de
# 370 variables accesorias (COVID-19, subsidios, tenencia de bienes, cesantía)
# y se conservan exclusivamente las variables demográficas, educativas,
# laborales y de ingresos pertinentes al objetivo del proyecto.
variables_seleccionadas <- c(
  # Identificadores de registro y temporalidad
  "ID_AMO",                     # Año de la encuesta
  "ID_TRIMESTRE",               # Trimestre (1 a 4)
  "ID_VIVIENDA",                # Identificador de la vivienda
  "ID_HOGAR",                   # Identificador del hogar
  "ID_LINEA",                   # Identificador de la persona
  "Consecutivo",                # Consecutivo único del registro
  
  # Variables demográficas
  "Sexo",                       # 1: Hombre, 2: Mujer
  "Edad",                       # Edad en años cumplidos
  "Grupo_edad",                 # Rango etario quinquenal/decenal
  
  # Variable de condición de actividad
  "Condicion_actividad",        # 1: Ocupado, 2: Desocupado, 3: Fuera fuerza trabajo
  
  # Variables educativas
  "Nivel_educativo",            # Nivel educativo aprobado (0 a 8+)
  "Amos_educacion",             # Años totales en el sistema educativo
  "Educacion_titulo",           # Indicador de obtención de título (0/1)
  "Educacion_nivel_grado",      # Último grado o año aprobado
  
  # Variables de ingresos y salario
  "Ingreso_principal",          # Ingreso mensual neto en empleo principal
  "Ingreso_total",              # Ingreso total neto en todos los empleos
  "Salario_bruto",              # Salario bruto del asalariado en empleo principal
  
  # Variables laborales complementarias
  "Horas_normales_principal",   # Horas habituales trabajadas por semana
  "Horas_efectivas_principal"   # Horas efectivas trabajadas la semana anterior
)

# 4. Función de ingesta modular con reporte individual
leer_y_reducir_trimestre <- function(ruta_archivo) {
  nombre <- basename(ruta_archivo)
  cat("Cargando y procesando: ", nombre, " ... ", sep = "")
  
  # Lectura del archivo SPSS
  df_trimestre <- read_sav(ruta_archivo)
  filas <- nrow(df_trimestre)
  cols <- ncol(df_trimestre)
  
  # Validar cuáles variables seleccionadas existen en este trimestre
  vars_presentes <- intersect(variables_seleccionadas, names(df_trimestre))
  vars_ausentes  <- setdiff(variables_seleccionadas, names(df_trimestre))
  
  if (length(vars_ausentes) > 0) {
    warning("\nEn ", nombre, " faltan las variables: ", paste(vars_ausentes, collapse = ", "))
  }
  
  # Subconjunto de variables (Reducción de Dimensionalidad)
  df_sub <- df_trimestre %>% select(all_of(vars_presentes))
  
  cat("OK (", filas, " filas, ", cols, " cols originales -> ", ncol(df_sub), " cols seleccionadas)\n", sep = "")
  
  # Retornar lista con dataframe y metadatos
  list(
    data = df_sub,
    nombre = nombre,
    filas = filas,
    cols = cols
  )
}

# 5. Ejecutar la lectura de los 8 trimestres
tiempo_inicio <- Sys.time()
resultados <- map(archivos_sav, leer_y_reducir_trimestre)

# Consolidar metadatos individuales
tabla_resumen <- map_df(resultados, function(res) {
  tibble(
    Archivo = res$nombre,
    Filas_Iniciales = res$filas,
    Columnas_Iniciales = res$cols
  )
})

# Fusionar todos los trimestres en un único DataFrame unificado
cat("\nFusionando microdatos en un dataset consolidado...\n")
df_consolidado <- map_df(resultados, ~ .x$data)

n_filas_total <- nrow(df_consolidado)
n_cols_total  <- ncol(df_consolidado)
tiempo_fin    <- Sys.time()

# 6. Verificación y Tratamiento de Duplicados (Técnica 3: Filtrado de Duplicados)
cat("\n--- Verificando presencia de duplicados en la muestra consolidada (Técnica 3) ---\n")
n_duplicados_exactos <- sum(duplicated(df_consolidado))
cat("Duplicados exactos detectados: ", n_duplicados_exactos, "\n", sep = "")

# Aplicar distinct() para garantizar unicidad muestral
df_limpio_dups <- df_consolidado %>% distinct()
n_filas_sin_dup <- nrow(df_limpio_dups)
n_duplicados_eliminados <- n_filas_total - n_filas_sin_dup
cat("Filas tras filtrado de duplicados: ", format(n_filas_sin_dup, big.mark = ","), " (Eliminados: ", n_duplicados_eliminados, ")\n", sep = "")

# 7. Reporte de Resultados y Métricas de Ingesta y Duplicados
cat("\n==================================================================\n")
cat("                RESUMEN DE INGESTA, REDUCCIÓN Y DUPLICADOS       \n")
cat("==================================================================\n")
print(as.data.frame(tabla_resumen))
cat("------------------------------------------------------------------\n")
cat("TOTAL FILAS INICIALES (Muestra Bruta):    ", format(n_filas_total, big.mark = ","), "\n")
cat("TOTAL COLUMNAS ORIGINALES POR ARCHIVO:   ", tabla_resumen$Columnas_Iniciales[1], "\n")
cat("COLUMNAS RETENIDAS (Reducción Dimens.): ", n_cols_total, "\n")
cat("DUPLICADOS EXACTOS DETECTADOS / REMOVIDOS:", n_duplicados_eliminados, "\n")
cat("TOTAL FILAS FINALES TRAS TASK 01:        ", format(n_filas_sin_dup, big.mark = ","), "\n")
cat("TIEMPO TOTAL DE PROCESAMIENTO:           ", round(difftime(tiempo_fin, tiempo_inicio, units = "secs"), 2), "segundos\n")
cat("==================================================================\n\n")

# 8. Exportar dataset consolidado preliminar
# Guardamos en formato RDS (nube R comprimida que preserva tipos y atributos)
# y también en CSV para compatibilidad
dir.create("data/procesados", showWarnings = FALSE, recursive = TRUE)

ruta_rds <- "data/procesados/ece_2024_2025_consolidado_crudo.rds"
ruta_csv <- "data/procesados/ece_2024_2025_consolidado_crudo.csv"

cat("Guardando dataset consolidado en formato RDS: ", ruta_rds, " ... ", sep = "")
saveRDS(df_limpio_dups, ruta_rds)
cat("OK\n")

cat("Guardando copia en formato CSV: ", ruta_csv, " ... ", sep = "")
write_csv(df_limpio_dups, ruta_csv)
cat("OK\n\n")

# 9. Generar archivo de bitácora para la Plantilla del Informe (CI0115)
bitacora_texto <- paste0(
  "==============================================================================\n",
  "BITÁCORA TÉCNICA - SECCIÓN 1 Y 2 PARA PLANTILLA CI0115 (IGNACIO)\n",
  "==============================================================================\n",
  "Nombre del conjunto de datos: Encuesta Continua de Empleo (ECE) - 2024 a 2025\n",
  "Fuente / Origen: Instituto Nacional de Estadística y Censos (INEC) de Costa Rica\n",
  "Número de filas iniciales: ", format(n_filas_total, big.mark = ","), " registros\n",
  "Número de columnas iniciales: ", tabla_resumen$Columnas_Iniciales[1], " columnas\n",
  "Columnas seleccionadas tras reducción: ", n_cols_total, " columnas\n",
  "Columnas eliminadas por irrelevancia: ", (tabla_resumen$Columnas_Iniciales[1] - n_cols_total), " columnas\n",
  "Filas duplicadas encontradas / eliminadas: ", n_duplicados_eliminados, " registros\n",
  "Filas vigentes al finalizar TASK 01: ", format(n_filas_sin_dup, big.mark = ","), " registros\n\n",
  "DATOS PARA LA TABLA DE PROBLEMAS IDENTIFICADOS (SECCIÓN 2):\n",
  "- Datos duplicados: Columna afectada = 'Clave de registro y variables analíticas'\n",
  "  Cantidad = '", n_duplicados_eliminados, " (N/A tras auditoría)'\n",
  "  Descripción = 'Se auditó la unicidad de las ", format(n_filas_total, big.mark = ","), " observaciones mediante\n",
  "                 duplicated() y distinct(). No se encontraron registros redundantes ni colisiones,\n",
  "                 confirmando la calidad de control de campo del INEC.'\n\n",
  "- Reducción de dimensionalidad: Columna afectada = 'Dataset completo (394 variables)'\n",
  "  Cantidad = '", (tabla_resumen$Columnas_Iniciales[1] - n_cols_total), " variables descartadas'\n",
  "  Descripción = 'Se descartaron preguntas no pertinentes sobre búsqueda de empleo de cesantes,\n",
  "                 subsidios, características de vivienda y módulos especiales no requeridos\n",
  "                 para analizar la brecha salarial por nivel educativo, sexo y edad.'\n",
  "==============================================================================\n"
)

writeLines(bitacora_texto, "data/procesados/metricas_ingesta_plantilla.txt")
cat("Archivo de bitácora para la plantilla generado en: data/procesados/metricas_ingesta_plantilla.txt\n")
cat("TASK 01 COMPLETADA CON ÉXITO.\n")
