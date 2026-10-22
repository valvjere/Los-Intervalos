# ==============================================================================
# EQUIPO: Los Intervalos
# TAREA: TASK_03_Faltantes_y_Categorias
# Responsable: Lucas
# Técnicas 1 (faltantes), 2 (tipografía) y 7 (codificación categórica) de T5.
# ==============================================================================

library(haven)
library(dplyr)
library(purrr)
library(stringr)

# Salida de TASK 02: población de estudio ya filtrada y con tipos validados
df <- readRDS("data/procesados/ece_2024_2025_paso2_filtrado_tipos.rds")
n_poblacion <- nrow(df)

# TASK 02 quita las etiquetas de SPSS; se recuperan de los .sav originales
archivos_sav <- list.files("data", pattern = "\\.sav$", full.names = TRUE)
etiquetas_sav <- read_sav(archivos_sav[1], n_max = 1,
                          col_select = c(Sexo, Nivel_educativo, Educacion_titulo))
con_etiquetas <- function(x, variable) {
  as_factor(labelled(as.numeric(as.character(x)), attr(etiquetas_sav[[variable]], "labels")))
}

# read_sav() convierte en NA los códigos 99999999 y 0; user_na = TRUE los conserva
llave <- c("ID_AMO", "ID_TRIMESTRE", "Consecutivo", "ID_VIVIENDA", "ID_HOGAR", "ID_LINEA")
df_sav <- archivos_sav %>%
  map_df(~ read_sav(.x, user_na = TRUE,
                    col_select = all_of(c(llave, "Ingreso_principal", "Posicion_empleo")))) %>%
  mutate(across(everything(), ~ as.numeric(unclass(.x)))) %>%
  rename(Ingreso_sav = Ingreso_principal)

df <- left_join(df, df_sav, by = llave)

# ------------------------------------------------------------------------------
# Técnica 1: Manejo de valores faltantes
# ------------------------------------------------------------------------------
df <- df %>%
  mutate(
    Estado_ingreso = case_when(
      Posicion_empleo == 4      ~ "Auxiliar no remunerado",
      is.na(Ingreso_sav)        ~ "Sin dato en origen",
      Ingreso_sav == 99999999   ~ "Código 99999999",
      Ingreso_sav == 0          ~ "Código 0",
      TRUE                      ~ "Válido"
    ),
    Ingreso_principal = if_else(Estado_ingreso == "Válido", Ingreso_sav, NA_real_),
    Amos_educacion = na_if(as.numeric(Amos_educacion), 99)
  )

estado_ingreso <- count(df, Estado_ingreso)
tasa_no_respuesta <- function(variable) {
  df %>%
    filter(Estado_ingreso != "Auxiliar no remunerado") %>%
    group_by(Grupo = con_etiquetas(.data[[variable]], variable)) %>%
    summarise(Tasa_pct = round(100 * mean(Estado_ingreso != "Válido"), 2))
}
no_respuesta_sexo  <- tasa_no_respuesta("Sexo")
no_respuesta_nivel <- tasa_no_respuesta("Nivel_educativo")

# El ingreso es la variable respuesta: se elimina en vez de imputar
df <- df %>% filter(Estado_ingreso == "Válido", Nivel_educativo != 9)

# ------------------------------------------------------------------------------
# Técnica 2: Corrección de errores tipográficos en etiquetas
# ------------------------------------------------------------------------------
etiquetas_titulo <- attr(etiquetas_sav$Educacion_titulo, "labels")
names(etiquetas_titulo) <- str_squish(names(etiquetas_titulo)) %>%
  str_replace("^Maestria$", "Maestría")
df$Educacion_titulo <- labelled(as.numeric(df$Educacion_titulo), etiquetas_titulo)
n_maestria <- sum(df$Educacion_titulo == 7, na.rm = TRUE)

# ------------------------------------------------------------------------------
# Técnica 7: Codificación de variables categóricas
# ------------------------------------------------------------------------------
niveles_educativos <- c(
  "Ninguno", "Primaria incompleta", "Primaria completa",
  "Secundaria incompleta", "Secundaria completa",
  "Universitario sin título", "Universitario con título de grado", "Posgrado"
)

# Nivel_educativo no distingue posgrado; se toma de Educacion_titulo (5 a 8)
df <- df %>%
  mutate(
    Sexo_cat = factor(Sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    Mujer = as.integer(Sexo == 2),
    Nivel_educativo_ord = if_else(Nivel_educativo == 6 & Educacion_titulo %in% 5:8,
                                  7, as.numeric(Nivel_educativo)),
    Nivel_educativo_cat = factor(niveles_educativos[Nivel_educativo_ord + 1],
                                 levels = niveles_educativos, ordered = TRUE),
    Grupo_edad_estudio = cut(Edad, breaks = c(17, 24, 34, 49, 65),
                             labels = c("18-24", "25-34", "35-49", "50-65"),
                             ordered_result = TRUE)
  ) %>%
  select(-Ingreso_sav, -Estado_ingreso)

saveRDS(df, "data/procesados/ece_2024_2025_preparado.rds")

# Cifras para el informe
print(estado_ingreso)
print(no_respuesta_sexo)
print(no_respuesta_nivel)
cat("Población:", n_poblacion, "| Filas finales:", nrow(df), "| Columnas:", ncol(df), "\n")
cat("Registros con 'Maestria' corregida:", n_maestria, "\n")
print(table(df$Nivel_educativo_cat))
print(table(df$Grupo_edad_estudio))
