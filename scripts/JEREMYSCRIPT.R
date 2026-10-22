# ==============================================================================
# PROYECTO CI-0115 PROBABILIDAD Y ESTADÍSTICA - UCR
# EQUIPO: Los Intervalos
# TAREA: TASK_02_Filtrado_Tipos_Incoherencias
# Responsable: Jeremy (Especialista en Consistencia & Tipos) - Técnicas 4 y 8
# ==============================================================================
# Descripción:
# Continúa el flujo de TASK_01 (Ignacio). Lee el consolidado crudo de la ECE
# 2024-2025 y ejecuta, en este orden:
#   Paso A. Delimitación poblacional (ocupados, 18 a 65 años, Hombre/Mujer).
#   Paso B. Conversión y validación estricta de tipos de datos     (Técnica 4).
#   Paso C. Detección y depuración de incoherencias lógicas        (Técnica 8).
# Cada descarte queda registrado con su regla en un archivo de auditoría.
#
# Requisito previo (1 línea en TASK_01): agregar "Ingreso_bruto_principal" a
# `variables_seleccionadas`. Se necesita para comparar ingreso NETO contra
# ingreso BRUTO del mismo concepto (ver Paso C, regla I1).
#
# NO elimina duplicados, NO trata faltantes (99999999, 0, 99), NO recodifica
# etiquetas ni detecta atípicos: eso corresponde a Ignacio, Lucas y Daniel.
# ==============================================================================

# 1. Librerías ------------------------------------------------------------------
if (!requireNamespace("haven", quietly = TRUE)) install.packages("haven")

suppressPackageStartupMessages({
  library(haven)   # zap_labels(): quita metadatos de SPSS
  library(dplyr)   # filter / mutate / across
  library(tibble)  # tablas de resumen
  library(readr)   # exportación CSV
})

cat("\n==================================================================\n")
cat("  INICIANDO TASK 02: FILTRADO, TIPOS E INCOHERENCIAS (ECE 24-25)  \n")
cat("==================================================================\n\n")

# 2. Parámetros de las reglas (editables y documentados en el informe) ----------
EDAD_MIN <- 18          # población objetivo: edad mínima (inclusive)
EDAD_MAX <- 65          # población objetivo: edad máxima (inclusive)

# Edades mínimas verosímiles para un título (inicio de primaria a los 6 años,
# primaria 6 años, secundaria 5 años -> 17 años; + 4 años de bachillerato = 21;
# se concede 1 año de tolerancia por ingreso temprano y se redondea hacia abajo).
EDAD_MIN_GRADO    <- 20  # Bachillerato (3) o Licenciatura (4)
EDAD_MIN_POSGRADO <- 23  # Especialización (5), Maestría (6, 7) o Doctorado (8)

INICIO_ESCOLAR   <- 4    # edad más temprana de ingreso al sistema (maternal)
HORAS_SEMANA_MAX <- 112  # 16 h diarias x 7 días: tope de jornada físicamente creíble
TOL_COLONES      <- 0.01 # tolerancia por redondeo en comparaciones de ingresos

# 3. Cargar el consolidado de TASK_01 -------------------------------------------
ruta_entrada <- "data/procesados/ece_2024_2025_consolidado_crudo.rds"
if (!file.exists(ruta_entrada)) {
  stop("ERROR: no existe '", ruta_entrada, "'. Ejecute primero TASK_01_Ingesta_y_Fusion.R.")
}
df_crudo <- readRDS(ruta_entrada)

vars_requeridas <- c("ID_AMO", "ID_TRIMESTRE", "ID_VIVIENDA", "ID_HOGAR", "ID_LINEA",
                     "Consecutivo", "Sexo", "Edad", "Grupo_edad", "Condicion_actividad",
                     "Nivel_educativo", "Amos_educacion", "Educacion_titulo",
                     "Educacion_nivel_grado", "Ingreso_principal", "Ingreso_total",
                     "Salario_bruto", "Horas_normales_principal",
                     "Horas_efectivas_principal", "Ingreso_bruto_principal")
faltan <- setdiff(vars_requeridas, names(df_crudo))
if (length(faltan) > 0) {
  stop("ERROR: faltan columnas en el consolidado: ", paste(faltan, collapse = ", "),
       ". Si falta Ingreso_bruto_principal, agréguela a variables_seleccionadas en TASK_01.")
}

n_inicial <- nrow(df_crudo)
cat("Registros de entrada (TASK_01): ", format(n_inicial, big.mark = ","), "\n")
cat("Columnas de entrada:            ", ncol(df_crudo), "\n\n")

# ==============================================================================
# PASO A. DELIMITACIÓN POBLACIONAL
# ==============================================================================
# Se aplica secuencialmente y se registra cuántos registros sale en cada criterio.
# Un NA en cualquiera de las variables excluye al registro (filter descarta NA);
# en la ECE, Condicion_actividad es NA para menores de 15 años (fuera del universo).
cat("--- Paso A: Delimitación de la población objetivo ---\n")

df_a0 <- df_crudo %>% mutate(across(everything(), ~ as.numeric(haven::zap_labels(.x))))

df_a1 <- df_a0 %>% filter(Condicion_actividad == 1)                 # Ocupados
df_a2 <- df_a1 %>% filter(Edad >= EDAD_MIN, Edad <= EDAD_MAX)       # 18 a 65
df_a3 <- df_a2 %>% filter(Sexo %in% c(1, 2))                        # Hombre / Mujer

tabla_poblacion <- tibble(
  Criterio = c("Registros de entrada",
               "Condicion_actividad == 1 (ocupados)",
               paste0("Edad entre ", EDAD_MIN, " y ", EDAD_MAX, " años (inclusive)"),
               "Sexo en {1 = Hombre, 2 = Mujer}"),
  Quedan   = c(nrow(df_a0), nrow(df_a1), nrow(df_a2), nrow(df_a3))
) %>% mutate(Descartados = lag(Quedan, default = first(Quedan)) - Quedan)

print(as.data.frame(tabla_poblacion))
cat("\n")
n_poblacion <- nrow(df_a3)

# ==============================================================================
# PASO B. CONVERSIÓN Y VALIDACIÓN DE TIPOS DE DATOS (Técnica 4)
# ==============================================================================
cat("--- Paso B: Conversión y validación de tipos ---\n")

# Tipo objetivo de cada columna:
#   entero     -> identificadores, conteos y códigos discretos (sin decimales)
#   factor     -> Sexo (nominal; las etiquetas Hombre/Mujer las asigna Lucas)
#   continuo   -> montos en colones (double)
tipos_objetivo <- c(
  ID_AMO = "entero", ID_TRIMESTRE = "entero", ID_VIVIENDA = "entero",
  ID_HOGAR = "entero", ID_LINEA = "entero", Consecutivo = "entero",
  Sexo = "factor", Edad = "entero", Grupo_edad = "entero",
  Condicion_actividad = "entero",
  Nivel_educativo = "entero", Amos_educacion = "entero",
  Educacion_titulo = "entero", Educacion_nivel_grado = "entero",
  Ingreso_principal = "continuo", Ingreso_total = "continuo",
  Salario_bruto = "continuo", Ingreso_bruto_principal = "continuo",
  Horas_normales_principal = "entero", Horas_efectivas_principal = "entero"
)

clase_actual <- function(x) class(x)[1]
clase_esperada <- c(entero = "integer", factor = "factor", continuo = "numeric")

# Auditoría ANTES de convertir (sobre el consolidado tal como llegó de TASK_01)
auditoria_tipos <- tibble(
  Variable      = names(tipos_objetivo),
  Tipo_leido    = vapply(names(tipos_objetivo), function(v) clase_actual(df_crudo[[v]]), ""),
  Tipo_objetivo = unname(clase_esperada[tipos_objetivo])
) %>% mutate(Incorrecto = Tipo_leido != Tipo_objetivo)

# "numeric" y "double" son el mismo tipo continuo para este propósito
auditoria_tipos$Incorrecto[auditoria_tipos$Tipo_objetivo == "numeric" &
                             auditoria_tipos$Tipo_leido == "numeric"] <- FALSE

# Validación previa: una columna "entera" solo se castea si TODOS sus valores
# no nulos son enteros exactos (si no, as.integer truncaría datos en silencio).
vars_enteras <- names(tipos_objetivo)[tipos_objetivo == "entero"]
no_enteros <- vapply(vars_enteras, function(v) {
  x <- df_a3[[v]]
  sum(!is.na(x) & x != round(x))
}, numeric(1))
if (any(no_enteros > 0)) {
  stop("ERROR: valores no enteros en columnas definidas como enteras: ",
       paste(names(no_enteros)[no_enteros > 0], collapse = ", "))
}

# Validación previa de montos: finitos y no negativos
vars_continuas <- names(tipos_objetivo)[tipos_objetivo == "continuo"]
montos_invalidos <- vapply(vars_continuas, function(v) {
  x <- df_a3[[v]]
  sum(!is.na(x) & (!is.finite(x) | x < 0))
}, numeric(1))

# Conversión explícita
df_b <- df_a3 %>%
  mutate(
    across(all_of(vars_enteras), as.integer),
    across(all_of(vars_continuas), as.numeric),
    Sexo = factor(Sexo, levels = c(1, 2))   # nominal; labels = tarea de Lucas (T7)
  )

# Verificación posterior
auditoria_tipos$Tipo_final <- vapply(auditoria_tipos$Variable,
                                     function(v) clase_actual(df_b[[v]]), "")
stopifnot(all(auditoria_tipos$Tipo_final == auditoria_tipos$Tipo_objetivo |
                (auditoria_tipos$Tipo_objetivo == "numeric" & auditoria_tipos$Tipo_final == "numeric")))

n_cols_tipo_incorrecto <- sum(auditoria_tipos$Incorrecto)
cat("Columnas validadas:                               ", nrow(auditoria_tipos), "\n")
cat("Columnas con tipo incorrecto antes de convertir:   ", n_cols_tipo_incorrecto, "\n")
cat("   - tipo leído 'haven_labelled' (SPSS):           ",
    sum(auditoria_tipos$Tipo_leido == "haven_labelled"), "\n")
cat("Valores no enteros en columnas enteras:            ", sum(no_enteros), "\n")
cat("Montos negativos o no finitos:                     ", sum(montos_invalidos), "\n\n")

# ==============================================================================
# PASO C. INCOHERENCIAS LÓGICAS (Técnica 8)
# ==============================================================================
cat("--- Paso C: Detección de incoherencias lógicas ---\n")

# Las reglas se evalúan TODAS sobre la misma base (no excluyentes entre sí) para
# reportar cuántos casos detecta cada una; luego se descarta la unión.
# Un NA en una comparación NO se considera incoherencia (eso es un faltante,
# tarea de Lucas): coalesce(..., FALSE).
bandera <- function(cond) dplyr::coalesce(cond, FALSE)

df_c <- df_b %>%
  mutate(
    # I1. El ingreso NETO no puede superar al BRUTO del mismo concepto
    #     (empleo principal), pues el neto resulta de deducir CCSS y renta.
    I1_neto_mayor_bruto = bandera(Ingreso_principal > Ingreso_bruto_principal + TOL_COLONES),
    
    # I2. El ingreso total (todos los empleos) no puede ser menor que el del
    #     empleo principal, que es uno de sus componentes.
    I2_total_menor_principal = bandera(Ingreso_total < Ingreso_principal - TOL_COLONES),
    
    # I3. Edad mínima verosímil para el título declarado.
    #     Educacion_titulo: 3 Bachillerato, 4 Licenciatura,
    #                       5 Especialización, 6 Maestría y doctorado, 7 Maestría, 8 Doctorado
    I3_edad_vs_titulo = bandera(
      (Educacion_titulo %in% c(3L, 4L)       & Edad < EDAD_MIN_GRADO) |
        (Educacion_titulo %in% c(5L, 6L, 7L, 8L) & Edad < EDAD_MIN_POSGRADO)
    ),
    
    # I4. Nivel educativo incompatible con el título declarado.
    #     Nivel_educativo: 0-4 = hasta secundaria completa, 5 = universitario sin
    #     título, 6 = universitario con título. (Código 9 = "No especificado" es
    #     un faltante: no se evalúa aquí.)
    I4_nivel_vs_titulo = bandera(
      (Nivel_educativo %in% 0:4 & Educacion_titulo >= 2L) |   # título superior sin haber pasado a la universidad
        (Nivel_educativo == 5L    & Educacion_titulo >= 2L) |   # "sin título" pero con título superior
        (Nivel_educativo == 6L    & (Educacion_titulo == 0L | is.na(Educacion_titulo)))  # "con título" sin título
    ),
    
    # I5. Años en el sistema educativo imposibles para la edad. El código 99
    #     ("Ignorado") se excluye: es un faltante, no una incoherencia.
    I5_amos_vs_edad = bandera(Amos_educacion < 99L & Amos_educacion > Edad - INICIO_ESCOLAR),
    
    # I6. Jornada semanal físicamente imposible (más de 16 h diarias los 7 días).
    I6_horas_imposibles = bandera(Horas_normales_principal > HORAS_SEMANA_MAX |
                                    Horas_efectivas_principal > HORAS_SEMANA_MAX)
  )

reglas <- c("I1_neto_mayor_bruto", "I2_total_menor_principal", "I3_edad_vs_titulo",
            "I4_nivel_vs_titulo", "I5_amos_vs_edad", "I6_horas_imposibles")

descripcion_reglas <- c(
  I1_neto_mayor_bruto      = "Ingreso neto > ingreso bruto (empleo principal)",
  I2_total_menor_principal = "Ingreso total < ingreso del empleo principal",
  I3_edad_vs_titulo        = "Edad menor a la mínima para el título declarado",
  I4_nivel_vs_titulo       = "Nivel educativo incompatible con el título declarado",
  I5_amos_vs_edad          = "Años de educación mayores a Edad - 4",
  I6_horas_imposibles      = "Más de 112 horas semanales"
)

df_c <- df_c %>% mutate(n_reglas = rowSums(across(all_of(reglas))), incoherente = n_reglas > 0)

tabla_incoherencias <- tibble(
  Regla       = reglas,
  Descripcion = unname(descripcion_reglas[reglas]),
  Casos       = vapply(reglas, function(r) sum(df_c[[r]]), numeric(1))
)
n_incoherentes <- sum(df_c$incoherente)

print(as.data.frame(tabla_incoherencias))
cat("Registros incoherentes (unión, sin doble conteo): ", n_incoherentes, "\n\n")

# Diagnóstico NO aplicado: la comparación literal "Ingreso_principal > Salario_bruto"
# mezcla conceptos distintos. Ingreso_principal incluye salario escolar, pagos en
# especie y promedios de ingresos variables; Salario_bruto es solo el salario base.
# Se reporta para justificar por qué NO se usa como criterio de descarte.
n_literal <- sum(bandera(df_b$Ingreso_principal > df_b$Salario_bruto + TOL_COLONES))
n_con_ambos <- sum(!is.na(df_b$Ingreso_principal) & !is.na(df_b$Salario_bruto))
cat("Diagnóstico (NO aplicado): Ingreso_principal > Salario_bruto en ", n_literal,
    " de ", n_con_ambos, " asalariados con ambos datos (",
    round(100 * n_literal / max(n_con_ambos, 1), 1), "%)\n\n", sep = "")

# Auditoría: cada registro descartado con las reglas que incumple
auditoria <- df_c %>%
  filter(incoherente) %>%
  mutate(reglas_incumplidas = apply(across(all_of(reglas)), 1,
                                    function(f) paste(reglas[f], collapse = " | "))) %>%
  select(ID_AMO, ID_TRIMESTRE, ID_VIVIENDA, ID_HOGAR, ID_LINEA, Consecutivo,
         Edad, Nivel_educativo, Educacion_titulo, Amos_educacion,
         Ingreso_principal, Ingreso_bruto_principal, Horas_normales_principal,
         Horas_efectivas_principal, reglas_incumplidas)

df_final <- df_c %>% filter(!incoherente) %>% select(-all_of(reglas), -n_reglas, -incoherente)
n_final <- nrow(df_final)

# 4. Reporte ----------------------------------------------------------------------
cat("==================================================================\n")
cat("                 RESUMEN TASK 02 (JEREMY)                         \n")
cat("==================================================================\n")
cat("Registros de entrada:                    ", format(n_inicial, big.mark = ","), "\n")
cat("Registros tras delimitación poblacional: ", format(n_poblacion, big.mark = ","), "\n")
cat("Descartados por incoherencias lógicas:   ", format(n_incoherentes, big.mark = ","), "\n")
cat("Registros de salida:                     ", format(n_final, big.mark = ","), "\n")
cat("Retención de este módulo (salida/entrada):", round(100 * n_final / n_inicial, 2), "%\n")
cat("Columnas de salida:                      ", ncol(df_final), "\n")
cat("==================================================================\n\n")

# 5. Exportar -----------------------------------------------------------------------
dir.create("data/procesados", showWarnings = FALSE, recursive = TRUE)
saveRDS(df_final, "data/procesados/ece_2024_2025_paso2_filtrado_tipos.rds")
write_csv(df_final, "data/procesados/ece_2024_2025_paso2_filtrado_tipos.csv")
write_csv(auditoria, "data/procesados/auditoria_incoherencias_descartadas.csv")
write_csv(tabla_incoherencias, "data/procesados/tabla_incoherencias_por_regla.csv")
write_csv(auditoria_tipos, "data/procesados/auditoria_tipos_de_datos.csv")

# 6. Bitácora para la Plantilla del Informe (CI0115) -------------------------------
fmt <- function(x) format(x, big.mark = " ", scientific = FALSE, trim = TRUE)
lineas_reglas <- paste0("  - ", tabla_incoherencias$Regla, ": ", tabla_incoherencias$Casos,
                        " casos (", tabla_incoherencias$Descripcion, ")", collapse = "\n")

bitacora <- paste0(
  "==============================================================================\n",
  "BITÁCORA TÉCNICA - SECCIONES II Y III PARA PLANTILLA CI0115 (JEREMY)\n",
  "==============================================================================\n",
  "POBLACIÓN OBJETIVO\n",
  "  Registros de entrada:          ", fmt(n_inicial), "\n",
  "  Ocupados (Condicion_actividad == 1): ", fmt(nrow(df_a1)), " (descartados: ", fmt(n_inicial - nrow(df_a1)), ")\n",
  "  Edad 18 a 65:                  ", fmt(nrow(df_a2)), " (descartados: ", fmt(nrow(df_a1) - nrow(df_a2)), ")\n",
  "  Sexo Hombre/Mujer:             ", fmt(nrow(df_a3)), " (descartados: ", fmt(nrow(df_a2) - nrow(df_a3)), ")\n\n",
  "TABLA DE PROBLEMAS - FORMATOS INCORRECTOS\n",
  "  Columna afectada = ", paste(auditoria_tipos$Variable[auditoria_tipos$Incorrecto], collapse = ", "), "\n",
  "  Cantidad = ", n_cols_tipo_incorrecto, " de ", nrow(auditoria_tipos), " columnas con tipo incorrecto\n",
  "  Valores no enteros en columnas enteras: ", sum(no_enteros), "; montos negativos/no finitos: ", sum(montos_invalidos), "\n\n",
  "TABLA DE PROBLEMAS - INCOHERENCIAS LÓGICAS\n",
  lineas_reglas, "\n",
  "  Cantidad (unión sin doble conteo) = ", n_incoherentes, " registros\n",
  "  Diagnóstico no aplicado: Ingreso_principal > Salario_bruto en ", n_literal, " de ", n_con_ambos, " asalariados\n",
  "==============================================================================\n"
)
writeLines(bitacora, "data/procesados/metricas_jeremy_plantilla.txt")
cat("Bitácora generada en: data/procesados/metricas_jeremy_plantilla.txt\n")
cat("TASK 02 COMPLETADA CON ÉXITO.\n")
