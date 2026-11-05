# Une la limpieza de TASK 03 con la selección y tipos de TASK 02.
integrar_etapas <- function(tipos, categorias, llave) {
  for (base in list(tipos, categorias)) {
    if (!all(llave %in% names(base)) || anyNA(base[llave]) ||
        anyDuplicated(base[llave])) {
      stop("Integración: la llave debe existir y ser completa y única.")
    }
  }
  if (!all(names(tipos) %in% names(categorias))) {
    stop("Integración: faltan columnas de TASK 02 en TASK 03.")
  }
  if (".fila_tipo" %in% names(categorias)) stop("Nombre reservado: .fila_tipo")
  indice <- tipos[llave]
  indice$.fila_tipo <- seq_len(nrow(tipos))
  unidos <- dplyr::inner_join(
    categorias, indice, by = llave, relationship = "one-to-one"
  )
  excluidos <- dplyr::anti_join(categorias, tipos[llave], by = llave)
  comparar <- function(x) {
    if (is.factor(x)) as.character(x) else as.numeric(x)
  }
  for (nombre in names(tipos)) {
    anterior <- tipos[[nombre]][unidos$.fila_tipo]
    limpio <- unidos[[nombre]]
    # TASK 03 trata estos dos valores; las demás variables deben coincidir.
    if (!nombre %in% c("Ingreso_principal", "Amos_educacion") &&
        !isTRUE(all.equal(
          as.character(comparar(anterior)), as.character(comparar(limpio))
        ))) {
      stop("Integración: valores incompatibles en ", nombre)
    }
    etiquetas <- attr(limpio, "labels", exact = TRUE)
    if (is.factor(anterior)) {
      convertido <- factor(comparar(limpio), levels = levels(anterior))
      if (any(is.na(convertido) & !is.na(limpio))) {
        stop("Integración: categoría nueva sin tipo acordado en ", nombre)
      }
    } else if (is.integer(anterior)) {
      valores <- as.numeric(limpio)
      if (any(!is.na(valores) &
          (!is.finite(valores) | valores != trunc(valores) |
            abs(valores) > .Machine$integer.max))) {
        stop("Integración: conversión a entero perdería datos en ", nombre)
      }
      convertido <- as.integer(valores)
    } else if (is.numeric(anterior)) {
      convertido <- as.numeric(limpio)
    } else {
      stop("Integración: tipo no previsto en ", nombre)
    }
    # Se retienen las etiquetas corregidas sin restaurar la clase SPSS.
    if (!is.factor(convertido) && length(etiquetas)) {
      attr(convertido, "labels") <- etiquetas
    }
    unidos[[nombre]] <- convertido
  }
  unidos$.fila_tipo <- NULL
  list(datos = unidos, excluidos = excluidos)
}
