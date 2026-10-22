# Lectura del cuadro salarial y comparación de una cifra oficial.
letra_excel <- function(n) {
  letras <- character()
  while (n > 0) {
    letras <- c(LETTERS[(n - 1) %% 26 + 1], letras)
    n <- (n - 1) %/% 26
  }
  paste(letras, collapse = "")
}

leer_cuadro_inec <- function(referencia, hoja, cfg) {
  ultima_fila <- max(cfg$fila_periodos, cfg$fila_valor, cfg$fila_nota)
  cuadro <- as.data.frame(readxl::read_excel(
    referencia, sheet = hoja, range = readxl::cell_rows(seq_len(ultima_fila)),
    col_names = FALSE, col_types = "text", .name_repair = "minimal"
  ))
  if (nrow(cuadro) < ultima_fila || ncol(cuadro) < cfg$columna_rotulo) {
    stop("Dimensiones insuficientes en la hoja ", hoja)
  }
  rotulo <- cuadro[cfg$fila_valor, cfg$columna_rotulo]
  nota <- cuadro[cfg$fila_nota, cfg$columna_rotulo]
  if (is.na(rotulo) || !identical(trimws(rotulo), cfg$rotulo_esperado) ||
    is.na(nota) || !grepl(cfg$nota_esperada, nota, fixed = TRUE)) {
    stop("Cambió la estructura o definición en ", hoja, " (rótulo/nota).")
  }
  cuadro
}

comparar_ingresos_inec <- function(fila, cuadro, micro, cfg) {
  problema <- function(estado, mensaje) {
    fila$estado <- estado
    fila$diagnostico <- paste(mensaje, fila$periodo, "/", fila$grupo)
    fila
  }
  columna <- which(
    trimws(as.character(cuadro[cfg$fila_periodos, ])) == fila$periodo
  )
  if (!length(columna)) {
    return(problema("pendiente", "Período no disponible en la referencia:"))
  }
  if (length(columna) != 1L) {
    return(problema("fallido", "Período duplicado en la referencia:"))
  }
  oficial <- suppressWarnings(as.numeric(cuadro[cfg$fila_valor, columna]))
  if (length(oficial) != 1L || !is.finite(oficial) || oficial <= 0) {
    return(problema("fallido", "El dato oficial no es un promedio positivo:"))
  }
  filas <- micro$elegible & if (fila$grupo == "Total") {
    TRUE
  } else {
    micro$datos[[cfg$columnas$grupo]] %in% cfg$codigos_grupo[[fila$grupo]]
  }
  if (!any(filas)) {
    return(problema("pendiente", "No hay ingresos elegibles para comparar:"))
  }
  # Media = suma(peso * ingreso) / suma(peso).
  # No se calculan errores estándar del diseño muestral.
  calculado <- weighted.mean(micro$bruto[filas], micro$peso[filas])
  suma_pesos <- sum(micro$peso[filas])
  if (!is.finite(calculado) || !is.finite(suma_pesos)) {
    return(problema("fallido", "La media o suma de pesos no es finita:"))
  }
  diferencia <- calculado - oficial
  fila$n_registros <- sum(filas)
  fila$suma_ponderaciones <- suma_pesos
  fila$media_bruta_ponderada <- calculado
  fila$media_bruta_inec <- oficial
  fila$diferencia_colones <- diferencia
  fila$diferencia_porcentaje <- 100 * diferencia / oficial
  fila$coincide_redondeo <- abs(diferencia) <= fila$tolerancia_colones
  fila$celda <- paste0(letra_excel(columna), cfg$fila_valor)
  fila$estado <- "comparado"
  fila$diagnostico <- if (fila$coincide_redondeo) {
    "Coincide dentro de la tolerancia numérica por redondeo."
  } else {
    "Diferencia superior a la tolerancia; requiere revisión."
  }
  fila
}
