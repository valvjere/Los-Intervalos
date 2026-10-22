# Ejecutar desde la raíz: Rscript tests/task_04/test_configuracion_y_flujo.R
# Estas pruebas crean entradas pequeñas en tempdir(); no modifican datos reales.
# Verifican riesgos de integración: columnas desplazadas, identificadores,
# cambios de variable, referencias ausentes y conservación de filas/atributos.
raiz <- normalizePath(".")
source("scripts/task_04/cargar.R")
cargar_task_04(raiz)
temporal <- tempfile("pruebas_daniel_")
dir.create(temporal)
error_esperado <- function(expr) {
  resultado <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = identity
  )
  stopifnot(inherits(resultado, "error"))
}
base_cfg <- leer_configuracion(raiz)
formato <- base_cfg$formato
columnas <- list(ingreso = "valor", aliases = c("valor", "sueldo"))
error_esperado(mezclar_config(base_cfg, list(analisis = list(corte_zz = 2))))
error_esperado(mezclar_config(base_cfg, list(entrada = "a", entrada = "b")))
stopifnot(is.null(mezclar_config(
  base_cfg, list(columnas = list(ingreso = NULL))
)$columnas$ingreso))
error_esperado(resolver_columna(data.frame(otro = 1), columnas))
error_esperado(resolver_columna(
  data.frame(valor = 1, sueldo = 2),
  list(ingreso = NULL, aliases = c("valor", "sueldo"))
))
error_esperado(resolver_columna(data.frame(valor = factor("12")), columnas))
error_esperado(resolver_columna(data.frame(valor = TRUE), columnas))

# Separador y decimal son independientes. Preservar ceros iniciales en IDs.
csv <- file.path(temporal, "nuevo.csv")
writeLines(c("id;valor;grupo", "001;12,5;A", "002;24,5;B"), csv)
formato$separador <- ";"
formato$decimal <- ","
formato$columnas_texto <- "id"
importado <- leer_base_daniel(csv, formato)
stopifnot(
  identical(importado$id, c("001", "002")),
  identical(importado$valor, c(12.5, 24.5))
)
mal <- file.path(temporal, "desplazado.csv")
writeLines(c("id;valor", "001;2;3"), mal)
error_esperado(leer_base_daniel(mal, formato))
formato_mal <- formato
formato_mal$columnas_texto <- "ID_NO_EXISTE"
error_esperado(leer_base_daniel(csv, formato_mal))
writeLines(c("valor;valor", "1;2"), mal)
error_esperado(leer_base_daniel(mal, formato))
writeLines(c("id;valor", '001;"sin cierre'), mal)
error_esperado(suppressWarnings(leer_base_daniel(mal, formato)))
tsv <- file.path(temporal, "entrada.tsv")
writeLines(c("id\tvalor", "003\t2,5"), tsv)
stopifnot(
  leer_base_daniel(tsv, formato)$id == "003",
  leer_base_daniel(tsv, formato)$valor == 2.5
)
multilinea <- file.path(temporal, "comillas.csv")
writeLines(c("id;valor;nota", '004;3,5;"texto', 'con salto"'), multilinea)
stopifnot(nrow(leer_base_daniel(multilinea, formato)) == 1)

# Sin positivos, una sola observación y varianza cero quedan no evaluables.
for (v in list(
  numeric(), c(NA_real_, Inf), 10, c(10, 10), c(-2, 0, 3), c(NA, NA)
)) {
  original <- data.frame(valor = v, id = seq_along(v))
  t5 <- tecnica_05_atipicos(original, "valor",
    codigos_faltantes = numeric(),
    solo_positivos = FALSE, generar_graficos = FALSE
  )
  t6 <- tecnica_06_transformacion(t5$datos, "valor", generar_graficos = FALSE)
  for (n in names(original)) stopifnot(identical(original[[n]], t6$datos[[n]]))
  stopifnot(
    nrow(t6$datos) == nrow(original),
    t6$resumen$n_transformados[2] == sum(is.finite(v) & v > 0),
    all(is.na(t6$datos$d6_log_ingreso[is.finite(v) & v <= 0]))
  )
  if (sum(is.finite(v)) < 2 || length(unique(v[is.finite(v)])) < 2) {
    stopifnot(is.na(t5$resumen$cantidad[t5$resumen$criterio == "Z"]))
  }
}
doble <- tecnica_05_atipicos(
  data.frame(valor = 1:4, otro = 5:8), "valor", generar_graficos = FALSE
)
error_esperado(tecnica_06_transformacion(
  doble$datos, "otro", generar_graficos = FALSE
))
error_esperado(tecnica_05_atipicos(data.frame(valor = TRUE), "valor"))
error_esperado(tecnica_05_atipicos(
  data.frame(valor = 1:4), "valor", codigos_faltantes = Inf
))

# Cada corrida tiene una carpeta propia. Una referencia ausente es pendiente;
# no bloquea T5/T6 ni permite sobrescribir una corrida previa.
correr <- function(entrada, nombre, ajustes = list()) {
  cfg <- mezclar_config(base_cfg, list(
    entrada = entrada,
    salida = file.path(temporal, nombre),
    columnas = list(
      ingreso = "valor", grupos = character(), identificadores = "id"
    ),
    analisis = list(generar_graficos = FALSE),
    validacion = list(referencia = file.path(temporal, "no_existe.xlsx"))
  ))
  cfg <- mezclar_config(cfg, ajustes)
  archivo <- file.path(temporal, paste0(nombre, ".R"))
  dput(cfg, archivo)
  salida <- NULL
  invisible(capture.output(salida <- ejecutar_daniel(
    raiz,
    leer_argumentos_daniel(c("--config", archivo))
  )))
  stopifnot(
    salida$validacion$estado == "pendiente",
    salida$validacion$cobertura$comparadas == 0,
    all(is.na(salida$validacion$comparacion$coincide_redondeo))
  )
  invisible(salida)
}
para_rds <- data.frame(
  valor = c(10, 20, 30, 10000, NA), id = sprintf("%03d", 1:5)
)
attr(para_rds$valor, "unidad") <- "unidad de prueba"
rds <- file.path(temporal, "entrada.rds")
saveRDS(para_rds, rds)
r <- correr(rds, "rds")
diagnostico <- readRDS(file.path(r$salida, "datos_diagnosticados.rds"))
for (n in names(para_rds)) stopifnot(identical(para_rds[[n]], diagnostico[[n]]))
error_esperado(ejecutar_daniel(raiz, leer_argumentos_daniel(c(
  "--config", file.path(temporal, "rds.R")
))))
stopifnot(
  file.exists(file.path(r$salida, "configuracion_resuelta.R")),
  file.exists(file.path(r$salida, "trazabilidad.csv"))
)
# La convención debe atravesar configuración, coordinador y exportación.
# Con 10,20,30,10000: tipo 2 usa medias de pares (15 y 5015);
# tipo 7 interpola (17.5 y 2522.5). La diferencia detecta una opción ignorada.
cuantiles <- correr(rds, "cuantiles_tipo2", list(
  analisis = list(tipo_cuantil = 2L)
))
limites <- read.csv(file.path(cuantiles$salida, "tecnica_05_limites.csv"))
stopifnot(
  limites$tipo_cuantil == 2L, limites$q1 == 15, limites$q3 == 5015,
  dget(file.path(cuantiles$salida, "configuracion_resuelta.R"))$
    analisis$tipo_cuantil == 2L
)
error_esperado(correr(rds, "tipo_invalido", list(
  analisis = list(tipo_cuantil = 1.5)
)))
# El resultado conserva la explicación exacta que respaldó esa corrida.
fuente <- file.path(raiz, "docs/Fundamento_Libros_TASK_04.adoc")
copia <- file.path(cuantiles$salida, "fundamento_metodologico.adoc")
traza <- read.csv(file.path(cuantiles$salida, "trazabilidad.csv"))
stopifnot(
  file.exists(copia),
  unname(tools::md5sum(fuente)) == unname(tools::md5sum(copia)),
  normalizePath(fuente) %in% traza$archivo
)
correr(csv, "csv", list(formato = formato))
for (contenido in list("valor", c("valor", "NA", "NA"))) {
  archivo <- file.path(temporal, paste0("vacio", length(contenido), ".csv"))
  writeLines(contenido, archivo)
  r <- correr(archivo, paste0("sin_datos", length(contenido)))
  stopifnot(
    all(is.na(r$tecnica5$cantidad)), all(r$tecnica6$n_transformados == 0)
  )
  ret <- read.csv(file.path(r$salida, "retencion.csv"))
  if (length(contenido) == 1) stopifnot(is.na(ret$porcentaje[1]))
}

# El adaptador externo no convierte una desactivación o una estructura inválida
# en una validación superada. Los estados son parte del resultado público.
desactivada <- tecnica_10_validacion_inec(
  raiz, config = list(habilitada = FALSE)
)
fallida <- tecnica_10_validacion_inec(raiz, config = list(trimestres = 5))
stopifnot(
  desactivada$estado == "pendiente", desactivada$cobertura$comparadas == 0,
  fallida$estado == "fallido"
)

if (requireNamespace("haven", quietly = TRUE)) {
  etiquetas <- haven::labelled(c(1, 2, NA), labels = c(igual = 1, igual = 2))
  rotulos <- as.character(categorizar_grupo_daniel(etiquetas))
  stopifnot(identical(rotulos, c("igual [1]", "igual [2]", NA_character_)))
  sav <- file.path(temporal, "base.sav")
  haven::write_sav(data.frame(valor = 1:3, id = c("001", "002", "003")), sav)
  r <- correr(sav, "sav")
  stopifnot(nrow(readRDS(file.path(r$salida, "datos_diagnosticados.rds"))) == 3)
}
stopifnot(identical(
  as.character(categorizar_grupo_daniel(as.Date("2024-01-01"))), "2024-01-01"
))

# readxl distribuye estos pequeños ejemplos: no dependemos de generar Excel ni
# de instalar otra biblioteca sólo para las pruebas de los dos formatos.
if (requireNamespace("readxl", quietly = TRUE)) {
  for (extension in c("xlsx", "xls")) {
    ejemplo <- readxl::readxl_example(paste0("datasets.", extension))
    r <- correr(ejemplo, extension, list(
      columnas = list(ingreso = "mpg"),
      formato = list(hoja = "mtcars"),
      analisis = list(
        unidad = "millas/galón", etiqueta_variable = "Rendimiento"
      )
    ))
    stopifnot(
      nrow(readRDS(file.path(r$salida, "datos_diagnosticados.rds"))) == 32
    )
  }
}

# RStudio/source y Rscript deben localizar el repo aunque la carpeta activa sea
# otra. Se usan procesos separados para no alterar el entorno de quien prueba.
principal <- file.path(raiz, "scripts/04_tecnicas_5_6_10.R")
rscript <- file.path(R.home("bin"), "Rscript")
anterior <- getwd()
setwd(temporal)
cli <- system2(rscript, shQuote(c(
  principal, "--config", file.path(temporal, "csv.R"),
  "--entrada", rds, "--salida", file.path(temporal, "cli"),
  "--columna", "valor",
  "--sin-validacion", "--sin-graficos"
)), stdout = TRUE, stderr = TRUE)
stopifnot(
  is.null(attr(cli, "status")),
  file.exists(file.path(temporal, "cli", "datos_diagnosticados.rds"))
)
literal <- function(x) encodeString(x, quote = '"')
expresion <- paste0(
  "Sys.setenv(DANIEL_CONFIG=", literal(file.path(temporal, "rds.R")),
  ", DANIEL_ENTRADA=", literal(rds),
  ", DANIEL_SALIDA=", literal(file.path(temporal, "source")),
  "); source(", literal(principal), ")"
)
desde_source <- system2(
  rscript, c("-e", shQuote(expresion)), stdout = TRUE, stderr = TRUE
)
setwd(anterior)
stopifnot(
  is.null(attr(desde_source, "status")),
  file.exists(file.path(temporal, "source", "datos_diagnosticados.rds"))
)
cat(paste0(
  "OK: configuración, importadores, conservación, ",
  "casos límite y validación pendiente/fallida.\n"
))
