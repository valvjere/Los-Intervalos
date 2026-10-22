# Ejecutar desde la raíz del repo. No escribe ni modifica datos del proyecto.
for (p in c("haven", "ggplot2", "readxl")) {
  stopifnot(requireNamespace(p, quietly = TRUE))
}
source("scripts/task_04/cargar.R")
cargar_task_04(normalizePath("."))

# Ejemplo con no respuesta, no positivos, infinito y un extremo conocido.
entrada <- data.frame(
  Ingreso_principal = c(NA, 0, -2, Inf, 99999999, 100, 200, 300, 400, 10000),
  etiqueta = LETTERS[1:10]
)
t5 <- tecnica_05_atipicos(entrada)
t6 <- tecnica_06_transformacion(t5$datos)
stopifnot(
  nrow(t6$datos) == nrow(entrada),
  identical(t6$datos$Ingreso_principal, entrada$Ingreso_principal),
  identical(t6$datos$etiqueta, entrada$etiqueta),
  sum(t5$datos$d5_ingreso_utilizable) == 5,
  all(is.na(t5$datos$d5_atipico_iqr[1:5])),
  all(is.na(t6$datos$d6_log_ingreso[1:5])),
  t5$parametros$q1 == 200, t5$parametros$q3 == 400,
  t5$parametros$limite_inferior == -100, t5$parametros$limite_superior == 700,
  sum(t5$datos$d5_atipico_iqr, na.rm = TRUE) == 1,
  isTRUE(all.equal(
    t6$datos$d6_z_ingreso[6:10],
    as.numeric(scale(c(100, 200, 300, 400, 10000)))
  )),
  max(abs(
    exp(t6$datos$d6_log_ingreso[6:10]) - entrada$Ingreso_principal[6:10]
  )) < 1e-8
)

# Igualdad al límite: con Q1=2 y Q3=4 el límite superior es 7.
# El ingreso exactamente igual a 7 no debe señalarse.
borde <- tecnica_05_atipicos(data.frame(Ingreso_principal = c(1, 2, 3, 4, 7)))
stopifnot(borde$parametros$limite_superior == 7, !borde$datos$d5_atipico_iqr[5])

# Horgan (2009): datos p. 7; cuartiles p. 16 y deciles p. 17.
# Se conserva el cero real: no es un código faltante en este ejemplo.
# Sólo type=7 reproduce todos sus deciles entre las nueve opciones de R.
horgan <- c(
  0, 1, 2, 12, 12, 14, 18, 21, 21, 23, 24, 25,
  28, 29, 30, 30, 30, 33, 36, 44, 45, 47, 51
)
deciles_horgan <- c(
  0, 4, 12.8, 19.8, 22.6, 25, 29.2,
  30, 34.8, 44.8, 51
)
coincide_horgan <- vapply(1:9, function(tipo) {
  isTRUE(all.equal(
    unname(quantile(horgan, seq(0, 1, .1), type = tipo)), deciles_horgan
  ))
}, logical(1))
stopifnot(identical(which(coincide_horgan), 7L))
th <- tecnica_05_atipicos(
  data.frame(Ingreso_principal = horgan),
  codigos_faltantes = numeric(), solo_positivos = FALSE
)
stopifnot(
  th$parametros$tipo_cuantil == 7L,
  th$parametros$q1 == 16, th$parametros$q3 == 31.5
)
th6 <- tecnica_06_transformacion(th$datos, generar_graficos = FALSE)
g1_horgan <- sqrt(length(horgan)) * sum((horgan - mean(horgan))^3) /
  sum((horgan - mean(horgan))^2)^1.5
stopifnot(isTRUE(all.equal(
  th6$resumen$asimetria_g1[th6$resumen$escala == "original"], g1_horgan
)))

# Baron (2019), pp. 217 y 224: n=30, Q1=34, Q3=59 por orden.
# type=2 reproduce ese ejemplo; type=7 interpola y da 34.25/58.25.
# Ninguna de estas pruebas convierte una convención en regla universal.
baron <- c(
  70, 36, 43, 69, 82, 48, 34, 62, 35, 15,
  59, 139, 46, 37, 42, 30, 55, 56, 36, 82,
  38, 89, 54, 25, 35, 24, 22, 9, 56, 19
)
admite_tipo <- "quantile.type" %in% ggplot2::StatBoxplot$parameters()
baron_resultados <- lapply(c(2L, 7L), function(tipo) {
  tecnica_05_atipicos(
    data.frame(Ingreso_principal = baron),
    tipo_cuantil = tipo,
    generar_graficos = admite_tipo || tipo == 7L
  )
})
stopifnot(
  baron_resultados[[1]]$parametros$q1 == 34,
  baron_resultados[[1]]$parametros$q3 == 59,
  baron_resultados[[1]]$parametros$limite_inferior == -3.5,
  baron_resultados[[1]]$parametros$limite_superior == 96.5,
  baron_resultados[[2]]$parametros$q1 == 34.25,
  baron_resultados[[2]]$parametros$q3 == 58.25,
  baron_resultados[[2]]$parametros$limite_inferior == -1.75,
  baron_resultados[[2]]$parametros$limite_superior == 94.25
)

# Verificar la integración estadística/gráfica, no sólo el cálculo tabular:
# caja global, límites implícitos y puntos externos deben usar el mismo tipo.
verificar_caja <- function(resultado) {
  if (!length(resultado$graficos)) {
    return(invisible(NULL))
  }
  caja <- ggplot2::ggplot_build(resultado$graficos$boxplot_global)$data[[1]]
  d <- resultado$datos
  esperados <- d$Ingreso_principal[which(d$d5_atipico_iqr)]
  stopifnot(
    isTRUE(all.equal(caja$lower, resultado$parametros$q1)),
    isTRUE(all.equal(caja$upper, resultado$parametros$q3)),
    isTRUE(all.equal(sort(caja$outliers[[1]]), sort(esperados)))
  )
  invisible(NULL)
}
verificar_caja(th)
invisible(lapply(baron_resultados, verificar_caja))

# Walpole et al., tabla 1.8 / ejemplo 1.5, p. 25: IQR publicado .365.
# Sus tres atípicos (.72, .85, 2.55) se reproducen con type=2. La convención
# type=7 añade 1.09: un caso concreto donde sí cambia la decisión de revisión.
# Los Q1/Q3 siguientes se derivan de los datos, no se atribuyen como una
# prescripción universal del libro sobre los algoritmos de R.
walpole <- c(
  1.09, 1.92, 2.31, 1.79, 2.28, 1.74, 1.47, 1.97, .85, 1.24,
  1.58, 2.03, 1.70, 2.17, 2.55, 2.11, 1.86, 1.90, 1.68, 1.51,
  1.64, .72, 1.69, 1.85, 1.82, 1.79, 2.46, 1.88, 2.08, 1.67,
  1.37, 1.93, 1.40, 1.64, 2.09, 1.75, 1.63, 2.37, 1.75, 1.69
)
walpole_resultados <- lapply(c(2L, 7L), function(tipo) {
  tecnica_05_atipicos(
    data.frame(Ingreso_principal = walpole),
    tipo_cuantil = tipo,
    generar_graficos = admite_tipo || tipo == 7L
  )
})
esperados_walpole <- list(
  c(1.635, 2, .365, 1.0875, 2.5475),
  c(1.6375, 1.985, .3475, 1.11625, 2.50625)
)
atipicos_walpole <- list(c(.72, .85, 2.55), c(.72, .85, 1.09, 2.55))
for (i in seq_along(walpole_resultados)) {
  r <- walpole_resultados[[i]]
  observados <- unname(unlist(r$parametros[c(
    "q1", "q3", "iqr", "limite_inferior", "limite_superior"
  )]))
  stopifnot(
    isTRUE(all.equal(observados, esperados_walpole[[i]])),
    identical(
      sort(walpole[which(r$datos$d5_atipico_iqr)]),
      atipicos_walpole[[i]]
    )
  )
  verificar_caja(r)
}

# Fronteras inferiores y superiores exactas: igualdad no señala atípicos.
# Mover sólo los extremos apenas fuera del intervalo sí debe señalarlos.
for (tipo in c(2L, 7L)) {
  for (delta in c(0, .0001)) {
    x <- c(-1.5 - delta, 0, 0, 0, 1, 1, 1, 2.5 + delta)
    tb <- tecnica_05_atipicos(
      data.frame(Ingreso_principal = x),
      tipo_cuantil = tipo,
      solo_positivos = FALSE, codigos_faltantes = numeric(),
      generar_graficos = admite_tipo || tipo == 7L
    )
    stopifnot(
      tb$parametros$limite_inferior == -1.5,
      tb$parametros$limite_superior == 2.5,
      all(tb$datos$d5_atipico_iqr[c(1, 8)] == (delta > 0)),
      !any(tb$datos$d5_atipico_iqr[2:7])
    )
    verificar_caja(tb)
  }
}

# Tipos fuera del contrato se rechazan antes de producir resultados.
for (tipo in list(0, 10, 2.5, NA_real_, Inf, "7", TRUE, c(2L, 7L))) {
  stopifnot(inherits(try(tecnica_05_atipicos(
    data.frame(Ingreso_principal = 1:4),
    tipo_cuantil = tipo,
    generar_graficos = FALSE
  ), silent = TRUE), "try-error"))
}

# Compatibilidad con ggplot2 anterior: type7 sin argumento nuevo; otro tipo
# falla explícitamente, nunca queda una caja con cuartiles diferentes.
stopifnot(
  is.null(opciones_boxplot_daniel(7L, 1.5, FALSE)$quantile.type),
  opciones_boxplot_daniel(2L, 1.5, TRUE)$quantile.type == 2L,
  inherits(try(opciones_boxplot_daniel(2L, 1.5, FALSE),
    silent = TRUE
  ), "try-error")
)

# Sin variación: Z no es calculable. No inventar resultados numéricos.
constante <- suppressWarnings(tecnica_05_atipicos(
  data.frame(Ingreso_principal = rep(100, 5))
))
stopifnot(
  all(is.na(constante$datos$d5_z_diagnostico)),
  is.na(constante$resumen$cantidad[constante$resumen$criterio == "Z"])
)
stopifnot(inherits(
  try(
    tecnica_05_atipicos(data.frame(Ingreso_principal = factor(c(100, 200)))),
    silent = TRUE
  ),
  "try-error"
))
stopifnot(inherits(
  try(tecnica_05_atipicos(t5$datos), silent = TRUE), "try-error"
))
cat(paste0(
  "OK: fórmulas, fronteras, faltantes, tipos, ",
  "varianza cero y conservación.\n"
))

if ("--integracion" %in% commandArgs(trailingOnly = TRUE)) {
  t10 <- tecnica_10_validacion_inec(normalizePath("."))
  c <- t10$comparacion
  stopifnot(
    nrow(c) == 24, sum(t10$inventario$filas_crudas) == 174943,
    all(c$coincide_redondeo), all(is.finite(c$media_bruta_ponderada)),
    !anyDuplicated(c[c("anio", "trimestre", "grupo")]),
    setequal(c$periodo, as.vector(outer(
      c("EFM", "AMJ", "JAS", "OND"), c(2024, 2025), paste
    )))
  )
  cat("OK: 24 comparaciones INEC por trimestre y sexo, con períodos únicos.\n")
}
