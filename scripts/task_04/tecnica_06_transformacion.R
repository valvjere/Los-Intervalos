# Técnica 6: logaritmo natural y estandarización muestral.
tecnica_06_transformacion <- function(datos, columna = "Ingreso_principal",
                                      etiqueta_variable = "Ingreso",
                                      unidad = "colones",
                                      generar_graficos = TRUE) {
  numerica_o_vacia <- function(x) {
    is.numeric(x) || (is.logical(x) &&
      all(is.na(x)))
  }
  if (!is.data.frame(datos) || !is.character(columna) ||
    length(columna) != 1L ||
    is.na(columna) || !columna %in% names(datos) ||
    !numerica_o_vacia(datos[[columna]])) {
    stop("T6: falta la tabla o la columna numérica configurada.")
  }
  if (!all(c(
    "d5_ingreso_utilizable",
    "d5_z_diagnostico"
  ) %in% names(datos))) {
    stop("T6: ejecuta primero la técnica 5 sobre la misma columna.")
  }
  if (!identical(attr(datos, "daniel_variable", exact = TRUE), columna)) {
    stop(paste0(
      "T6: la variable no coincide con la diagnosticada o ",
      "falta su identificación; vuelve a ejecutar T5 para "
    ), columna, ".")
  }
  if (any(grepl("^d6_", names(datos)))) {
    stop(paste0(
      "T6: la entrada ya contiene transformaciones d6_; ",
      "utiliza la salida de T5."
    ))
  }
  if (!is.logical(datos$d5_ingreso_utilizable) ||
    anyNA(datos$d5_ingreso_utilizable) ||
    !is.numeric(datos$d5_z_diagnostico)) {
    stop(paste0(
      "T6: las columnas de diagnóstico no corresponden a la ",
      "salida esperada de T5."
    ))
  }
  if (!is.logical(generar_graficos) || length(generar_graficos) != 1L ||
    is.na(generar_graficos)) {
    stop("T6: generar_graficos debe ser TRUE o FALSE.")
  }
  texto_escalar <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  if (!texto_escalar(etiqueta_variable) || !texto_escalar(unidad)) {
    stop("T6: etiqueta_variable y unidad deben ser textos únicos.")
  }
  avisos <- character()
  x <- as.numeric(datos[[columna]])
  valido <- datos$d5_ingreso_utilizable & is.finite(x)
  v <- x[valido]
  # ln(x) sólo está definido para x > 0. No se usa log(x + 1).
  # Walpole et al. (2012), p. 201: ln(X) es normal si X es lognormal.
  log_valido <- valido & x > 0
  datos$d6_log_ingreso <- rep(NA_real_, nrow(datos))
  datos$d6_log_ingreso[log_valido] <- log(x[log_valido])
  if (any(valido & x <= 0)) {
    avisos <- c(
      avisos,
      paste(sum(valido & x <= 0), paste0(
        "valores utilizables no positivos: logaritmo no ",
        "evaluable; filas conservadas."
      ))
    )
  }
  # Z reutiliza la media y s de T5; cambia la escala, no la asimetría.
  datos$d6_z_ingreso <- datos$d5_z_diagnostico
  # g1 = m3 / m2^(3/2), con momentos de denominador n.
  # Horgan (2009), p. 21. Un g1 cercano a cero no demuestra normalidad.
  asimetria <- function(v) {
    v <- v[is.finite(v)]
    if (length(v) < 2L) {
      return(NA_real_)
    }
    centrado <- v - mean(v)
    m2 <- mean(centrado^2)
    if (!is.finite(m2) || m2 <= 0) {
      return(NA_real_)
    }
    resultado <- mean(centrado^3) / m2^1.5
    if (is.finite(resultado)) resultado else NA_real_
  }
  escalas <- list(
    original = v,
    logaritmo_natural = datos$d6_log_ingreso[valido],
    z_ingreso = datos$d6_z_ingreso[valido]
  )
  resumen <- do.call(rbind, lapply(names(escalas), function(nombre) {
    a <- escalas[[nombre]]
    a <- a[is.finite(a)]
    n <- length(a)
    data.frame(
      tecnica = 6, escala = nombre, n_transformados = n,
      n_utilizables = length(v),
      estado = if (!n) {
        "no evaluable"
      } else if (n < length(v)) {
        "parcialmente evaluable"
      } else {
        "evaluable"
      },
      media = if (n) mean(a) else NA_real_,
      mediana = if (n) median(a) else NA_real_,
      desviacion = if (n > 1L) sd(a) else NA_real_,
      asimetria_g1 = asimetria(a)
    )
  }))
  if (!any(log_valido)) {
    avisos <- c(
      avisos,
      "Logaritmo no evaluable: no hay observaciones utilizables positivas."
    )
  }
  if (!any(is.finite(datos$d6_z_ingreso[valido]))) {
    avisos <- c(
      avisos,
      "Escalado Z no evaluable: revisa los avisos de la técnica 5."
    )
  }
  p <- NULL
  if (generar_graficos && any(log_valido)) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop(paste0(
        "T6: instala ggplot2 para generar gráficos o usa ",
        "generar_graficos=FALSE."
      ))
    }
    g <- data.frame(log_valor = datos$d6_log_ingreso[log_valido])
    eje <- paste0(
      "ln(", etiqueta_variable,
      if (nzchar(unidad)) paste0(" en ", unidad) else "", ")"
    )
    p <- ggplot2::ggplot(g, ggplot2::aes(log_valor)) +
      ggplot2::geom_histogram(bins = 60, fill = "#288275", color = "white") +
      ggplot2::labs(
        title = paste("Técnica 6: logaritmo natural de", etiqueta_variable),
        subtitle = paste(
          sum(log_valido), "observaciones positivas de",
          sum(valido), "utilizables"
        ),
        x = eje, y = "Registros",
        caption = paste0(
          "No se elimina ninguna fila. La transformación no ",
          "garantiza normalidad."
        )
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        plot.caption = ggplot2::element_text(hjust = 0)
      )
  }
  list(datos = datos, resumen = resumen, grafico = p, avisos = unique(avisos))
}
