# Texto del informe calculado a partir de la ejecución.
redactar_aporte <- function(
    cfg, columna, original, datos, t5, t6, t10, salida) {
  n <- nrow(original)
  c <- t10$cobertura
  f <- function(x) {
    ifelse(
      is.na(x), "no evaluable",
      format(round(x, 3), scientific = FALSE, trim = TRUE)
    )
  }
  r <- t5$resumen
  s <- t6$resumen
  texto <- c(
    "TASK 04 — TÉCNICAS 5, 6 Y 10 — DIAGNÓSTICO PRELIMINAR",
    cfg$estado, paste(
      "Variable:", columna, "|", cfg$analisis$etiqueta_variable,
      "| Unidad:", cfg$analisis$unidad
    ),
    paste0(
      "No se cambió la población ni se eliminaron registros. ",
      "Avisos detallados: avisos.txt."
    ), "",
    "SECCIÓN 2 — PROBLEMAS IDENTIFICADOS",
    paste(
      "Atípicos IQR:", f(r$cantidad[1]), "| Z:", f(r$cantidad[2]),
      "| Unión:", f(r$cantidad[4])
    ),
    paste(
      "Denominador:", sum(datos$d5_ingreso_utilizable),
      "valores utilizables. Los métodos pueden solaparse."
    ),
    paste(
      "Logaritmo:", s$n_transformados[2], "valores transformados. Z:",
      s$n_transformados[3], "."
    ),
    paste0(
      "Las transformaciones no son errores corregidos. NA ",
      "significa no evaluado, no cero problemas."
    ), "",
    "SECCIÓN 3 — TÉCNICAS APLICADAS",
    paste(
      "Técnica 5: Q1-k*IQR y Q3+k*IQR, con k =", cfg$analisis$multiplicador_iqr,
      "; cuantiles de R type =", cfg$analisis$tipo_cuantil,
      ". Complemento |Z| >", cfg$analisis$corte_z, "."
    ),
    paste(
      "Límites:", f(t5$parametros$limite_inferior), "y",
      f(t5$parametros$limite_superior), cfg$analisis$unidad
    ),
    paste0(
      "Los boxplots por grupo tienen límites propios. Las banderas ",
      "usan límites globales."
    ),
    paste0(
      "IQR y revisión de extremos: Horgan (2009), pp. 16-17 y 23-26; ",
      "Baron (2019), pp. 223-229; Walpole et al. (2012), pp. 24-25."
    ),
    paste0(
      "type=7 reproduce los cuantiles del ejemplo R de Horgan. ",
      "Otras convenciones pueden dar cuartiles distintos. El corte Z ",
      "es exploratorio: no se supone normalidad ni se eliminan casos."
    ),
    paste0(
      "Técnica 6: logaritmo natural sólo de valores utilizables ",
      "positivos; Z muestral cuando es calculable."
    ),
    paste(
      "Asimetría g1 original:", f(s$asimetria_g1[1]),
      "; logaritmo:", f(s$asimetria_g1[2]), "."
    ),
    paste0(
      "No se afirma normalidad. Si el log excluye valores no ",
      "positivos, sus observaciones no son las mismas."
    ),
    paste0(
      "Asimetría g1: Horgan (2009), pp. 20-21. Z es una ",
      "estandarización muestral; no convierte cualquier variable ",
      "en normal. Véase fundamento_metodologico.adoc."
    ),
    paste0(
      "Walpole et al. (2012), sección 6.9, p. 201, relacionan ln(X) ",
      "con la normal si X es lognormal. Aquí se explora el logaritmo ",
      "sin afirmar que los datos sigan ese modelo."
    ),
    paste(
      "Técnica 10 — estado:", t10$estado, ". Comparadas:", c$comparadas,
      "de", c$esperadas, "; coinciden por redondeo:", c$coinciden, "."
    ),
    paste("Pendientes:", c$pendientes, "; fallidas:", c$fallidas, "."),
    paste0(
      "El adaptador ECE reconstruye ingreso BRUTO nacional con el ",
      "universo y ponderadores configurados."
    ),
    paste0(
      "Ese control es independiente de la entrada. Cobertura ",
      "completa no significa ausencia de diferencias."
    ),
    paste0(
      "La comparación ECE y la tolerancia son decisiones del proyecto, ",
      "basadas en las definiciones del INEC; no son una receta de los ",
      "libros ni una prueba de hipótesis."
    ),
    paste0(
      "Las faltas de referencia o desactivación quedan pendientes, ",
      "nunca como validación exitosa."
    ), "",
    "SECCIÓN 5 — RESULTADO DEL MÓDULO",
    paste(
      "Antes:", n, "filas y", ncol(original), "columnas. Después:",
      nrow(datos), "filas y", ncol(datos), "columnas."
    ),
    paste(
      "Retención del módulo:",
      if (n) "100%." else "no evaluable: entrada vacía."
    ),
    paste0(
      "Retención final del equipo: pendiente. Las columnas ",
      "originales conservan sus valores y atributos."
    ), "",
    "SECCIÓN 6 — REFERENCIAS",
    paste0(
      "Libros, páginas y referencias APA: fundamento_metodologico.adoc. ",
      "Aplicación: docs/Aporte_TASK_04_Limpieza.adoc. ",
      "Versiones y citas del software: referencias_software.txt."
    ),
    paste0(
      "Revisar tecnica_10_estado.csv, ",
      "tecnica_10_comparacion_inec.csv y avisos.txt antes de ",
      "concluir."
    )
  )
  writeLines(texto, file.path(salida, "aporte_plantilla.txt"), useBytes = TRUE)
}
