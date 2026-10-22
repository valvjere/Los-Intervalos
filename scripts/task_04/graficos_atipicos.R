# Histogramas y cajas con la misma convención de cuantiles que T5.
categorizar_grupo_daniel <- function(categoria) {
  if (is.factor(categoria)) {
    return(categoria)
  }
  # Las fechas mantienen su representación legible.
  if (inherits(categoria, c("Date", "POSIXt"))) {
    return(factor(as.character(categoria)))
  }
  etiquetas <- attr(categoria, "labels", exact = TRUE)
  if (inherits(categoria, "haven_labelled") && length(etiquetas)) {
    valores_originales <- if (is.numeric(categoria)) {
      as.numeric(categoria)
    } else {
      as.character(categoria)
    }
    valores <- sort(unique(valores_originales[!is.na(valores_originales)]))
    rotulos <- as.character(valores)
    posiciones <- match(valores, unname(etiquetas))
    rotulos[!is.na(posiciones)] <-
      names(etiquetas)[posiciones[!is.na(posiciones)]]
    repetidos <- duplicated(rotulos) | duplicated(rotulos, fromLast = TRUE)
    rotulos[repetidos] <- paste0(
      rotulos[repetidos], " [", valores[repetidos],
      "]"
    )
    # Códigos distintos conservan categorías distintas aun con igual rótulo.
    rotulos <- make.unique(rotulos)
    return(factor(valores_originales, levels = valores, labels = rotulos))
  }
  factor(categoria)
}

opciones_boxplot_daniel <- function(tipo_cuantil, multiplicador_iqr,
                                    admite_tipo) {
  if (!admite_tipo && tipo_cuantil != 7L) {
    stop(paste0(
      "T5: esta versión de ggplot2 no admite quantile.type. ",
      "Actualiza ggplot2 o usa generar_graficos=FALSE; ",
      "no se puede representar fielmente tipo_cuantil=", tipo_cuantil, "."
    ))
  }
  opciones <- list(
    coef = multiplicador_iqr, fill = "#D8EAF2", outlier.alpha = .25
  )
  if (admite_tipo) opciones$quantile.type <- tipo_cuantil
  opciones
}

graficar_atipicos <- function(datos, v, valido, grupos, superior,
                              tipo_cuantil, multiplicador_iqr,
                              etiqueta_variable, unidad) {
  graficos <- list()
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(paste0(
      "T5: instala ggplot2 para generar gráficos o usa ",
      "generar_graficos=FALSE."
    ))
  }
  opciones_caja <- opciones_boxplot_daniel(
    tipo_cuantil, multiplicador_iqr,
    admite_tipo = "quantile.type" %in% ggplot2::StatBoxplot$parameters()
  )
  capa_caja <- function() do.call(ggplot2::geom_boxplot, opciones_caja)
  estilo <- ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0)
    )
  numeros <- function(x) {
    format(x,
      big.mark = " ", scientific = FALSE,
      trim = TRUE
    )
  }
  eje <- paste0(etiqueta_variable, if (nzchar(unidad)) {
    paste0(
      " (", unidad,
      ")"
    )
  } else {
    ""
  })
  histograma <- ggplot2::ggplot(
    data.frame(valor = v),
    ggplot2::aes(valor)
  ) +
    ggplot2::geom_histogram(
      bins = 60, boundary = 0, fill = "#276B8E",
      color = "white"
    ) +
    ggplot2::scale_x_continuous(labels = numeros) +
    ggplot2::labs(
      title = paste("Técnica 5: distribución de", etiqueta_variable),
      subtitle = "Observaciones utilizables; sin ponderación", x = eje,
      y = "Registros",
      caption = if (is.finite(superior)) {
        "Línea: límite IQR superior global. No se han eliminado atípicos."
      } else {
        "Límite IQR no evaluable. No se han eliminado filas."
      }
    ) +
    estilo
  if (is.finite(superior)) {
    histograma <- histograma +
      ggplot2::geom_vline(
        xintercept = superior, color = "#BC4B35",
        linetype = "dashed"
      )
  }
  graficos$histograma_variable <- histograma
  graficos$boxplot_global <- ggplot2::ggplot(
    data.frame(valor = v),
    ggplot2::aes(x = "Conjunto", y = valor)
  ) +
    capa_caja() +
    ggplot2::scale_y_continuous(labels = numeros) +
    ggplot2::labs(
      title = paste("Técnica 5: boxplot de", etiqueta_variable),
      subtitle = "Escala original; sin ponderación", x = NULL, y = eje,
      caption = paste0(
        "Los puntos del boxplot son señales visuales; revisar ",
        "con las banderas IQR."
      )
    ) +
    estilo
  # Cada caja usa cuartiles de su grupo; las banderas de T5 son globales.
  for (nombre in grupos) {
    categoria <- categorizar_grupo_daniel(datos[[nombre]])
    g <- data.frame(valor = v, grupo = categoria[valido])
    graficos[[paste0("boxplot_", nombre)]] <- ggplot2::ggplot(
      g,
      ggplot2::aes(grupo, valor)
    ) +
      capa_caja() +
      ggplot2::scale_y_continuous(labels = numeros) +
      ggplot2::scale_x_discrete(labels = function(s) {
        vapply(s, function(t) {
          if (is.na(t)) {
            "Sin dato"
          } else {
            paste(strwrap(t, width = 16),
              collapse = "\n"
            )
          }
        }, character(1))
      }) +
      ggplot2::labs(
        title = paste("Técnica 5:", etiqueta_variable, "por", nombre),
        subtitle = "Cajas por grupo; escala original; sin ponderación",
        x = NULL, y = eje,
        caption = paste0(
          "Los puntos son alertas por grupo. No equivalen ",
          "necesariamente a errores."
        )
      ) +
      estilo
  }
  graficos
}
