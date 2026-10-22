# Parámetros del control nacional ECE y validación de sus opciones.
config_ece_default <- function() {
  list(
    habilitada = TRUE,
    referencia = "info/inec/seempleoiiitri2010-jja2026-04.xlsx",
    carpeta_microdatos = "data",
    anios = c(2024L, 2025L), trimestres = 1:4,
    hojas = c(Total = "C4 total", Hombre = "C4 hombre", Mujer = "C4 mujer"),
    codigos_grupo = c(Hombre = 1, Mujer = 2),
    columnas = list(
      anio = "ID_AMO", trimestre = "ID_TRIMESTRE",
      actividad = "Condicion_actividad", grupo = "Sexo",
      valor = "Ingreso_bruto_principal", peso = "Factor_ponderacion"
    ),
    ocupado = 1, codigos_faltantes = c(0, 99999999),
    fila_periodos = 5L, fila_valor = 8L, columna_rotulo = 1L,
    rotulo_esperado = "1. Rama de actividad", fila_nota = 45L,
    nota_esperada = "ingreso bruto corriente",
    prefijos_periodo = c("EFM", "AMJ", "JAS", "OND"),
    romanos = c("I", "II", "III", "IV"),
    patron_archivo = "{trimestre} Trimestre {anio}.sav"
  )
}

validar_config_inec <- function(raiz, tolerancia_colones, config) {
  errores <- character()
  error_config <- function(mensaje) errores <<- c(errores, mensaje)
  texto <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) &&
      nzchar(x)
  }
  enteros_positivos <- function(x) {
    is.numeric(x) && length(x) > 0L &&
      all(is.finite(x)) && all(x > 0 & x == floor(x)) && !anyDuplicated(x)
  }
  if (!texto(raiz)) error_config("raiz debe ser una ruta de texto no vacía.")
  if (!is.numeric(tolerancia_colones) || length(tolerancia_colones) != 1L ||
    !is.finite(tolerancia_colones) || tolerancia_colones < 0) {
    error_config("tolerancia_colones debe ser un número finito no negativo.")
  }
  if (!is.list(config) || (length(config) &&
    (is.null(names(config)) || any(!nzchar(names(config))) ||
      anyDuplicated(names(config))))) {
    error_config("config debe ser una lista con nombres únicos.")
    config <- list()
  }
  predeterminada <- config_ece_default()
  desconocidos <- setdiff(names(config), names(predeterminada))
  if (length(desconocidos)) {
    error_config(paste(
      "Campos desconocidos:",
      paste(desconocidos, collapse = ", ")
    ))
  }
  cfg <- predeterminada
  for (nombre in intersect(names(config), names(cfg))) {
    cfg[nombre] <-
      config[nombre]
  }
  if ("columnas" %in% names(config)) {
    if (!is.list(config$columnas) || is.null(names(config$columnas)) ||
      anyDuplicated(names(config$columnas)) ||
      length(setdiff(names(config$columnas), names(predeterminada$columnas)))) {
      error_config(paste0(
        "columnas debe contener sólo anio, ",
        "trimestre, actividad, grupo, valor y peso."
      ))
    } else {
      cfg$columnas <- predeterminada$columnas
      for (nombre in names(config$columnas)) {
        cfg$columnas[nombre] <-
          config$columnas[nombre]
      }
    }
  }
  if (!is.logical(cfg$habilitada) || length(cfg$habilitada) != 1L ||
    is.na(cfg$habilitada)) {
    error_config("habilitada debe ser TRUE o FALSE.")
  }
  for (nombre in c(
    "referencia", "carpeta_microdatos", "rotulo_esperado",
    "nota_esperada", "patron_archivo"
  )) {
    if (!texto(cfg[[nombre]])) {
      error_config(paste(
        nombre,
        "debe ser texto no vacío."
      ))
    }
  }
  if (!enteros_positivos(cfg$anios)) {
    error_config("anios debe contener años enteros positivos sin repetir.")
  }
  if (!enteros_positivos(cfg$trimestres) || any(!cfg$trimestres %in% 1:4)) {
    error_config("trimestres debe contener números del 1 al 4 sin repetir.")
  }
  for (nombre in c(
    "fila_periodos", "fila_valor", "columna_rotulo",
    "fila_nota"
  )) {
    if (!enteros_positivos(cfg[[nombre]]) || length(cfg[[nombre]]) != 1L) {
      error_config(paste(nombre, "debe ser un entero positivo."))
    }
  }
  for (nombre in c("prefijos_periodo", "romanos")) {
    x <- cfg[[nombre]]
    if (!is.character(x) || length(x) != 4L || anyNA(x) ||
      any(!nzchar(x)) || anyDuplicated(x)) {
      error_config(paste(nombre, "requiere cuatro textos únicos."))
    }
  }
  hojas_validas <- is.character(cfg$hojas) && length(cfg$hojas) > 0L &&
    !anyNA(cfg$hojas) && all(nzchar(cfg$hojas)) && !is.null(names(cfg$hojas)) &&
    !anyNA(names(cfg$hojas)) && all(nzchar(names(cfg$hojas))) &&
    !anyDuplicated(names(cfg$hojas)) && "Total" %in% names(cfg$hojas)
  if (!hojas_validas) {
    error_config(paste0(
      "hojas debe ser un vector con nombres de ",
      "grupos únicos e incluir Total."
    ))
  }
  if (hojas_validas && length(setdiff(
    setdiff(names(cfg$hojas), "Total"),
    names(cfg$codigos_grupo)
  ))) {
    error_config(paste0(
      "Falta el código numérico de uno de los ",
      "grupos configurados en hojas."
    ))
  }
  if (!is.numeric(cfg$codigos_grupo) || any(!is.finite(cfg$codigos_grupo)) ||
    (length(cfg$codigos_grupo) && (is.null(names(cfg$codigos_grupo)) ||
      anyDuplicated(names(cfg$codigos_grupo)) ||
      anyDuplicated(cfg$codigos_grupo)))) {
    error_config(paste0(
      "codigos_grupo debe ser un vector numérico ",
      "finito con nombres y códigos únicos."
    ))
  }
  if (!is.list(cfg$columnas) || !all(vapply(cfg$columnas, texto, logical(1)))) {
    error_config("Cada nombre de columna debe ser texto no vacío.")
  }
  if (!is.numeric(cfg$ocupado) || length(cfg$ocupado) != 1L ||
    !is.finite(cfg$ocupado)) {
    error_config("ocupado debe ser un código numérico finito.")
  }
  if (!is.numeric(cfg$codigos_faltantes) ||
    any(!is.finite(cfg$codigos_faltantes))) {
    error_config(paste0(
      "codigos_faltantes debe ser un vector ",
      "numérico finito; numeric(0) permite ", "ninguno."
    ))
  }
  if (texto(cfg$patron_archivo) &&
    (!grepl("{anio}", cfg$patron_archivo, fixed = TRUE) ||
      !grepl("{trimestre}", cfg$patron_archivo, fixed = TRUE))) {
    error_config("patron_archivo debe incluir {anio} y {trimestre}.")
  }
  list(cfg = cfg, errores = errores)
}
