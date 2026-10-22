# Microdatos ECE: estructura, período y ponderaciones válidas.
leer_microdatos_inec <- function(archivo, nombre, anio, trimestre, cfg) {
  inv <- data.frame(
    archivo = nombre, anio, trimestre, filas_crudas = NA_integer_,
    columnas_crudas = NA_integer_, md5 = NA_character_, estado =
      "pendiente", diagnostico = ""
  )
  if (!file.exists(archivo)) {
    inv$diagnostico <- paste("Faltan microdatos del período:", archivo)
    return(list(inventario = inv))
  }
  d <- tryCatch(haven::read_sav(archivo), error = function(e) e)
  if (inherits(d, "error")) {
    inv$estado <- "fallido"
    inv$diagnostico <- paste(
      "No se pudo leer", archivo, ":",
      conditionMessage(d)
    )
    return(list(inventario = inv))
  }
  inv$filas_crudas <- nrow(d)
  inv$columnas_crudas <- ncol(d)
  inv$md5 <- unname(tools::md5sum(archivo))
  requeridas <- unlist(cfg$columnas, use.names = FALSE)
  problema <- NULL
  if (!all(requeridas %in% names(d))) {
    problema <- paste("Faltan variables:", paste(setdiff(
      requeridas,
      names(d)
    ), collapse = ", "))
  } else if (!all(vapply(d[requeridas], is.numeric, logical(1)))) {
    problema <- paste0(
      "Las variables del adaptador ECE deben ser ",
      "numéricas; revisar los tipos de origen."
    )
  } else if (!nrow(d) || anyNA(d[[cfg$columnas$anio]]) ||
    anyNA(d[[cfg$columnas$trimestre]]) ||
    any(d[[cfg$columnas$anio]] != anio) ||
    any(d[[cfg$columnas$trimestre]] != trimestre)) {
    problema <- paste0(
      "Identificadores temporales ausentes o ",
      "distintos del período solicitado."
    )
  }
  if (!is.null(problema)) {
    inv$estado <- "fallido"
    inv$diagnostico <- paste(nombre, ":", problema)
    return(list(inventario = inv))
  }
  bruto <- as.numeric(d[[cfg$columnas$valor]])
  peso <- as.numeric(d[[cfg$columnas$peso]])
  elegible <- d[[cfg$columnas$actividad]] %in% cfg$ocupado &
    is.finite(bruto) & bruto > 0 & !(bruto %in% cfg$codigos_faltantes)
  if (any(!is.finite(peso[elegible]) | peso[elegible] <= 0)) {
    inv$estado <- "fallido"
    inv$diagnostico <- paste(
      nombre,
      ": ponderaciones faltantes/no positivas en el universo comparable."
    )
    return(list(inventario = inv))
  }
  inv$estado <- "leido"
  inv$diagnostico <- "Estructura y período de microdatos comprobados."
  list(
    inventario = inv, datos = d, bruto = bruto, peso = peso,
    elegible = elegible
  )
}
