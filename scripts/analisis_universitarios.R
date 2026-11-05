# Análisis descriptivo de ingresos de personas con estudios universitarios.
# Compara registros sin título con registros titulados de grado o posgrado.
# Los conteos corresponden a registros persona-trimestre, no a personas únicas.
# No se aplican ponderaciones, imputaciones ni exclusiones de valores atípicos.
#
# Uso desde Terminal, indicando el repositorio y la carpeta de resultados:
# Rscript scripts/analisis_universitarios.R /ruta/repo /ruta/resultados
#
# Si no se indican rutas, se usan los valores predeterminados del bloque 1.
# El paquete dplyr debe estar instalado antes de ejecutar el script.

# 1. Configuración y lectura de la base ---------------------------------------

# Carga dplyr sin mostrar los mensajes de inicio del paquete.
suppressPackageStartupMessages(library(dplyr))

# Los argumentos son las rutas escritas después del nombre del script.
a <- commandArgs(trailingOnly = TRUE)
repo <- if (length(a)) a[1] else "/Users/daniel/Dev/Los-Intervalos"
out <- if (length(a) > 1) a[2] else "resultados_universitarios"

# Crea la carpeta de salida. Si ya existe, reutiliza esa carpeta.
# Los archivos de resultados con el mismo nombre se sobrescriben al ejecutar.
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Lee la base preparada por las etapas anteriores del equipo.
# RDS conserva los tipos de las columnas, sus etiquetas y sus categorías.
ruta <- file.path(repo, "data/procesados/ece_2024_2025_preparado.rds")
base <- readRDS(ruta)

# 2. Selección de los grupos universitarios ----------------------------------

# La codificación de la base preparada es:
#   5: universitario sin título.
#   6: universitario con título de grado.
#   7: posgrado.
# El operador %in% conserva los registros con alguno de esos tres códigos.
# El operador |> pasa el resultado de una operación a la siguiente.
d <- base |>
    filter(Nivel_educativo_ord %in% c(5, 6, 7)) |>
    mutate(
        Grupo_universitario = factor(
            if_else(
                Nivel_educativo_ord == 5,
                "Sin título",
                "Con título (grado y posgrado)"
            ),
            # El orden importa: la diferencia se calcula como grupo 2 menos 1.
            levels = c("Sin título", "Con título (grado y posgrado)")
        )
    )

# 3. Comprobaciones de la muestra --------------------------------------------

# Estas columnas deben estar completas para poder calcular los resúmenes.
# No se usa na.rm = TRUE: los faltantes deben detectarse antes del cálculo.
variables_analisis <- c(
    "Ingreso_principal",
    "Grupo_universitario",
    "Sexo_cat",
    "Grupo_edad_estudio"
)

# Detiene la ejecución si los datos no cumplen los criterios del informe.
# Las 15 798 filas incluyen 2 614 sin título y 13 184 con grado o posgrado.
# Si se actualiza la base, hay que revisar esta cifra junto con el documento.
stopifnot(
    nrow(d) == 15798,
    all(d$Ingreso_principal > 0),
    all(d$Condicion_actividad == 1),
    all(d$Edad >= 18 & d$Edad <= 65),
    !anyNA(d[variables_analisis])
)

# La llave combina período, vivienda, hogar y persona dentro de la encuesta.
# Una misma persona podría aparecer en trimestres diferentes sin ser duplicado.
llave <- c(
    "ID_AMO",
    "ID_TRIMESTRE",
    "Consecutivo",
    "ID_VIVIENDA",
    "ID_HOGAR",
    "ID_LINEA"
)

# anyDuplicated() devuelve 0 si no hay combinaciones repetidas de la llave.
# ! convierte ese 0 en TRUE; stopifnot() detiene el script si hay duplicados.
stopifnot(!anyDuplicated(d[llave]))

# 4. Funciones reutilizables para resumir y exportar --------------------------

# x es la tabla que se quiere resumir.
# ... recibe las columnas de agrupación, separadas por comas.
# Sin columnas de agrupación, res(d) devuelve una fila para toda la muestra.
# Con agrupación, summarise() devuelve una fila por cada combinación presente.
res <- function(x, ...) {
    x |>
        group_by(...) |>
        summarise(
            # Cantidad de registros, no de personas distintas entre trimestres.
            n = n(),
            # Promedio aritmético del ingreso mensual neto, en colones.
            Media = mean(Ingreso_principal),
            # Valor central del ingreso ordenado; menos sensible a extremos.
            Mediana = median(Ingreso_principal),
            # Desviación estándar muestral, con denominador n - 1.
            DE = sd(Ingreso_principal),
            # Cuartiles que delimitan el 50 % central de los ingresos.
            # type = 7 es el método predeterminado de interpolación de R.
            Q1 = quantile(Ingreso_principal, 0.25, type = 7),
            Q3 = quantile(Ingreso_principal, 0.75, type = 7),
            # Devuelve el resultado sin mantener agrupaciones activas.
            .groups = "drop"
        )
}

# Guarda una tabla como CSV en la carpeta out, sin números de fila añadidos.
# UTF-8 conserva tildes y otros caracteres. Las cifras no se redondean aquí.
# nombre_archivo es el nombre del CSV, sin la extensión .csv.
export <- function(x, nombre_archivo) {
    write.csv(
        x,
        file.path(out, paste0(nombre_archivo, ".csv")),
        row.names = FALSE,
        fileEncoding = "UTF-8"
    )
}

# 5. Resúmenes de ingreso para las tablas del informe -------------------------

# Resumen conjunto de los 15 798 registros universitarios.
export(res(d), "resumen_general")

# Tabla 6: dos filas, sin título y con título de grado o posgrado.
export(res(d, Grupo_universitario), "comparacion_dos_grupos")

# Tabla 4: tres filas para mostrar por separado sin título, grado y posgrado.
# La columna n aporta los conteos; grado y posgrado suman 13 184 titulados.
export(res(d, Nivel_educativo_cat), "composicion_grado_posgrado")

# Tabla 7: 16 filas, una por combinación de titulación, sexo y grupo de edad.
# arrange() ordena las filas; no modifica los estadísticos calculados.
comparacion_sexo_edad <- res(
    d,
    Grupo_universitario,
    Sexo_cat,
    Grupo_edad_estudio
) |>
    arrange(Sexo_cat, Grupo_edad_estudio, Grupo_universitario)

export(comparacion_sexo_edad, "comparacion_sexo_edad")

# 6. Frecuencias y porcentajes de la muestra ----------------------------------

# Se repite el mismo cálculo para cada una de estas variables.
variables_frecuencias <- c(
    "Grupo_universitario",
    "Sexo_cat",
    "Grupo_edad_estudio"
)

for (v in variables_frecuencias) {
    # .data[[v]] selecciona la columna cuyo nombre está guardado en v.
    # count() crea una fila por categoría y cuenta sus registros en n.
    # Cada archivo usa como denominador el total de 15 798 registros.
    frecuencias <- d |>
        count(.data[[v]]) |>
        mutate(Porcentaje = 100 * n / sum(n))

    export(frecuencias, paste0("frecuencias_", v))
}

# Tabla 5 utiliza los CSV de Sexo_cat y Grupo_edad_estudio.
# El CSV de Grupo_universitario aporta los porcentajes sin y con título.

# 7. Diagnóstico de ingresos potencialmente atípicos --------------------------

# Este diagnóstico usa exclusivamente el subconjunto universitario.
# Solo genera alertas: no elimina registros ni modifica los ingresos.
x <- d$Ingreso_principal

# El rango intercuartílico es Q3 - Q1. diff() resta el primer valor al segundo.
q <- quantile(x, c(0.25, 0.75), type = 7)
iqr <- diff(q)
limite_inferior <- q[1] - 1.5 * iqr
limite_superior <- q[2] + 1.5 * iqr

# Cada posición es TRUE si el ingreso está fuera de los límites IQR.
# | significa que basta cumplir una de las dos condiciones.
i <- x < limite_inferior | x > limite_superior

# Z expresa la distancia respecto de la media en desviaciones estándar.
# Se usa como revisión exploratoria, sin asumir una distribución normal.
z <- (x - mean(x)) / sd(x)
alerta_z <- abs(z) > 3

# R cuenta TRUE como 1 y FALSE como 0 al sumar o promediar valores lógicos.
# Por eso sum(i) cuenta alertas y 100 * mean(i) calcula su porcentaje.
# & identifica las observaciones señaladas por ambos criterios a la vez.
diagnostico_atipicos <- data.frame(
    n = length(x),
    Q1 = q[1],
    Q3 = q[2],
    IQR = iqr,
    limite_inferior = limite_inferior,
    limite_superior = limite_superior,
    alertas_IQR = sum(i),
    porcentaje_IQR = 100 * mean(i),
    alertas_Z = sum(alerta_z),
    porcentaje_Z = 100 * mean(alerta_z),
    ambos = sum(i & alerta_z)
)

export(diagnostico_atipicos, "diagnostico_atipicos")

# 8. Diferencia descriptiva entre los ingresos promedio -----------------------

# El factor definido en el bloque 2 mantiene este orden de filas:
#   fila 1: sin título; fila 2: con título de grado o posgrado.
r <- res(d, Grupo_universitario)

# La diferencia monetaria se calcula con las medias sin redondear.
# El porcentaje usa como referencia el ingreso promedio del grupo sin título.
# Estos cálculos describen la muestra; no son pruebas de hipótesis.
diferencia_descriptiva <- data.frame(
    diferencia_media = r$Media[2] - r$Media[1],
    porcentaje = 100 * (r$Media[2] / r$Media[1] - 1)
)

export(diferencia_descriptiva, "diferencia_descriptiva")

# 9. Comparación adicional excluyendo posgrado --------------------------------

# Conserva solo los códigos 5 y 6 como referencia para revisar cuánto cambia
# la comparación al incluir posgrado. No sustituye el análisis principal.
solo_grado <- d |>
    filter(Nivel_educativo_ord %in% c(5, 6))

export(res(solo_grado, Nivel_educativo_cat), "comparacion_solo_grado")

# 10. Trazabilidad y versiones para reproducir los resultados -----------------

# Guarda la ruta y la huella MD5 de la base, junto con los conteos utilizados.
# La huella permite comprobar si se usó el mismo archivo de entrada.
trazabilidad <- c(
    paste("Base:", normalizePath(ruta)),
    paste("MD5:", tools::md5sum(ruta)),
    paste("Filas base:", nrow(base)),
    paste("Universitarios:", nrow(d)),
    "DE muestral; cuartiles type=7; montos corrientes sin ponderar."
)

writeLines(trazabilidad, file.path(out, "trazabilidad.txt"))

# Registra la versión de R, el sistema y los paquetes de la sesión.
capture.output(sessionInfo(), file = file.path(out, "versiones_R.txt"))

# 11. Vista de los principales resultados en la consola -----------------------

# Los CSV ya están guardados. print() permite revisar estos resúmenes en R.
print(res(d, Grupo_universitario))
print(res(d, Nivel_educativo_cat))
print(res(d))
print(read.csv(file.path(out, "diagnostico_atipicos.csv")))
