# Tablas, gráficas e información de reproducción de cada ejecución.
exportar_resultados <- function(
    raiz, entrada, archivo_cfg, salida, cfg, columna, original, t5, t6, t10) {
  diagnosticados <- t6$datos
  a <- cfg$analisis
  dir.create(salida, recursive = TRUE, showWarnings = FALSE)
  csv <- function(d, nombre) {
    write.csv(d, file.path(salida, nombre),
      row.names = FALSE,
      na = "NA", fileEncoding = "UTF-8"
    )
  }
  csv(t5$resumen, "tecnica_05_conteos.csv")
  csv(t5$parametros, "tecnica_05_limites.csv")
  csv(t6$resumen, "tecnica_06_escalas.csv")
  csv(t10$comparacion, "tecnica_10_comparacion_inec.csv")
  csv(t10$inventario, "tecnica_10_inventario.csv")
  csv(t10$fuentes, "tecnica_10_fuentes.csv")
  cobertura <- t10$cobertura
  csv(data.frame(
    estado = t10$estado, esperadas = cobertura$esperadas,
    comparadas = cobertura$comparadas, coinciden = cobertura$coinciden,
    pendientes = cobertura$pendientes, fallidas = cobertura$fallidas
  ), "tecnica_10_estado.csv")
  saveRDS(diagnosticados, file.path(salida, "datos_diagnosticados.rds"))

  # La tabla de revisión conserva las llaves disponibles de la entrada.
  revisar <- !diagnosticados$d5_ingreso_utilizable |
    diagnosticados$d5_atipico_alguno %in% TRUE
  seleccion <- unique(c(
    intersect(
      c(cfg$columnas$identificadores, cfg$columnas$grupos, columna),
      names(original)
    ),
    grep("^d[56]_", names(diagnosticados), value = TRUE)
  ))
  casos <- as.data.frame(
    lapply(diagnosticados[revisar, seleccion, drop = FALSE], function(v) {
      if (inherits(v, "haven_labelled")) as.numeric(v) else v
    }),
    check.names = FALSE
  )
  csv(casos, "casos_para_revision.csv")

  tablas <- list()
  for (col in intersect(cfg$columnas$grupos, names(original))) {
    g <- as.character(categorizar_grupo_daniel(original[[col]]))
    for (nivel in unique(g)) {
      filas <- if (is.na(nivel)) is.na(g) else !is.na(g) & g == nivel
      x <- as.numeric(original[[columna]])[
        filas & diagnosticados$d5_ingreso_utilizable
      ]
      tablas[[length(tablas) + 1]] <- data.frame(
        variable_grupo = col,
        grupo = if (is.na(nivel)) NA_character_ else as.character(nivel),
        variable_analizada = columna, n = length(x),
        media_no_ponderada = if (length(x)) mean(x) else NA_real_,
        mediana_no_ponderada = if (length(x)) median(x) else NA_real_
      )
    }
  }
  if (length(tablas)) csv(do.call(rbind, tablas), "resumen_por_grupo.csv")
  n <- nrow(original)
  csv(data.frame(
    etapa = c("Entrada", "Salida"), filas = c(n, nrow(diagnosticados)),
    columnas = c(ncol(original), ncol(diagnosticados)),
    valores_utilizables = sum(diagnosticados$d5_ingreso_utilizable)
  ), "resultado_antes_despues.csv")
  csv(data.frame(
    concepto = c("Retención dentro del módulo", "Retención final del equipo"),
    numerador = c(nrow(diagnosticados), NA), denominador = c(n, NA),
    porcentaje = c(if (n) 100 else NA, NA),
    estado = c(
      if (n) {
        "Calculada; ninguna fila eliminada"
      } else {
        "No evaluable: entrada vacía"
      },
      "Pendiente de integrar todas las etapas"
    )
  ), "retencion.csv")

  # Nombres de archivo saneados y únicos para evitar rutas o colisiones.
  graficos <- t5$graficos
  if (!is.null(t6$grafico)) graficos$tecnica_06_log <- t6$grafico
  if (length(graficos)) {
    nombres <- make.unique(
      gsub("[^[:alnum:]_-]", "_", names(graficos)), sep = "_"
    )
    for (i in seq_along(graficos)) {
      ggplot2::ggsave(file.path(salida, paste0(nombres[i], ".png")),
        graficos[[i]],
        width = 12, height = 7, dpi = 140
      )
    }
  }
  if (a$generar_graficos && cobertura$comparadas > 0) {
    necesitar_paquete("ggplot2")
    c <- t10$comparacion
    c <- c[
      is.finite(c$media_bruta_ponderada) & is.finite(c$media_bruta_inec),
      ,
      drop = FALSE
    ]
    c$periodo_orden <- paste0(c$anio, " T", c$trimestre)
    g <- ggplot2::ggplot(
      c, ggplot2::aes(periodo_orden, media_bruta_inec, group = grupo)
    ) +
      ggplot2::geom_point(color = "#276B8E", size = 3) +
      ggplot2::geom_point(
        ggplot2::aes(y = media_bruta_ponderada),
        color = "#CE583A", shape = 4, size = 3
      ) +
      ggplot2::facet_wrap(~grupo) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      ) +
      ggplot2::labs(
        title = paste("Técnica 10: control nacional ECE —", t10$estado),
        subtitle = paste0(
          "Azul: INEC. Cruz: cálculo ponderado. Sólo se muestran ",
          "comparaciones disponibles."
        ),
        x = NULL, y = "Promedio mensual bruto (colones)",
        caption = paste0(
          "Control independiente: no valida directamente la variable ",
          "ni la población de la entrada."
        )
      )
    ggplot2::ggsave(
      file.path(salida, "tecnica_10_control_inec.png"), g,
      width = 12, height = 6, dpi = 140
    )
  }
  fundamento <- file.path(raiz, "docs", "Fundamento_Libros_TASK_04.adoc")
  if (file.exists(fundamento)) {
    stopifnot(file.copy(
      fundamento, file.path(salida, "fundamento_metodologico.adoc")
    ))
  } else {
    t5$avisos <- c(t5$avisos, "Falta el documento de fundamento metodológico.")
  }
  avisos <- unique(c(t5$avisos, t6$avisos, t10$avisos))
  if (!length(avisos)) avisos <- "No se registraron avisos adicionales."
  writeLines(avisos, file.path(salida, "avisos.txt"), useBytes = TRUE)
  redactar_aporte(cfg, columna, original, diagnosticados, t5, t6, t10, salida)

  # La configuración resuelta y los MD5 identifican datos y código usados.
  cfg$entrada <- entrada
  cfg$salida <- normalizePath(salida)
  cfg$columnas$ingreso <- columna
  dput(cfg, file.path(salida, "configuracion_resuelta.R"))
  fuentes <- c(
    entrada, archivo_cfg, fundamento,
    list.files(file.path(raiz, "scripts/task_04"), full.names = TRUE),
    file.path(raiz, "config/task_04_ece.R"),
    file.path(raiz, "scripts/04_tecnicas_5_6_10.R"), t10$referencia
  )
  fuentes <- unique(fuentes[
    !is.na(fuentes) & nzchar(fuentes) & file.exists(fuentes)
  ])
  csv(data.frame(
    archivo = normalizePath(fuentes), md5 = unname(tools::md5sum(fuentes))
  ), "trazabilidad.csv")
  writeLines(
    capture.output(sessionInfo()), file.path(salida, "versiones_R.txt")
  )
  paquetes <- intersect(c("haven", "ggplot2", "readxl"), loadedNamespaces())
  writeLines(
    capture.output({
      print(citation())
      for (p in paquetes) print(citation(p))
    }),
    file.path(salida, "referencias_software.txt")
  )
}
