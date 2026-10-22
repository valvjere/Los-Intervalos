# Lectura y configuración de tablas, sin modificar su población.
es_ruta_absoluta <- function(ruta) grepl("^(/|[A-Za-z]:[/\\\\]|~)", ruta)
ruta_desde <- function(ruta, base) {
  if (es_ruta_absoluta(ruta)) path.expand(ruta) else file.path(base, ruta)
}

mezclar_config <- function(base, cambios, prefijo = "config") {
  if (is.list(cambios) && !length(cambios)) {
    return(base)
  }
  if (!is.list(cambios) || is.null(names(cambios)) ||
    any(!nzchar(names(cambios))) ||
    anyDuplicated(names(cambios))) {
    stop(prefijo, ": se necesita una lista con nombres únicos.")
  }
  desconocidas <- setdiff(names(cambios), names(base))
  if (length(desconocidas)) {
    stop(
      prefijo, ": opciones desconocidas: ",
      paste(desconocidas, collapse = ", ")
    )
  }
  for (nombre in names(cambios)) {
    if (is.list(base[[nombre]]) && !is.null(base[[nombre]])) {
      base[[nombre]] <- mezclar_config(
        base[[nombre]], cambios[[nombre]], paste0(prefijo, "$", nombre)
      )
    } else {
      base[nombre] <- list(cambios[[nombre]])
    } # La clave se conserva aunque su valor sea NULL.
  }
  base
}

leer_configuracion <- function(raiz, archivo = NULL, cambios = list()) {
  cfg <- dget(file.path(raiz, "config/task_04_ece.R"))
  if (!is.null(archivo)) cfg <- mezclar_config(cfg, dget(archivo))
  if (length(cambios)) cfg <- mezclar_config(cfg, cambios)
  texto <- function(x, nombre, nulo = FALSE) {
    if (nulo && is.null(x)) {
      return(invisible(TRUE))
    }
    if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
      stop(nombre, ": se requiere texto no vacío.")
    }
  }
  vector_texto <- function(x, nombre) {
    if (!is.null(x) &&
      (!is.character(x) || anyNA(x) || any(!nzchar(x)) || anyDuplicated(x))) {
      stop(nombre, ": usa un vector de nombres únicos o NULL.")
    }
  }
  texto(cfg$entrada, "entrada", TRUE)
  texto(cfg$salida, "salida", TRUE)
  texto(cfg$estado, "estado")
  texto(cfg$columnas$ingreso, "columnas$ingreso", TRUE)
  for (n in c("aliases", "grupos", "identificadores")) {
    vector_texto(cfg$columnas[[n]], paste0("columnas$", n))
  }
  vector_texto(cfg$candidatas, "candidatas")
  for (n in c("separador", "decimal", "codificacion")) {
    texto(cfg$formato[[n]], paste0("formato$", n))
  }
  if (nchar(cfg$formato$separador) != 1 ||
    !cfg$formato$decimal %in% c(".", ",") ||
    cfg$formato$separador == cfg$formato$decimal) {
    stop("Separador y decimal deben ser distintos y válidos.")
  }
  if (!is.character(cfg$formato$faltantes_texto) ||
    anyNA(cfg$formato$faltantes_texto)) {
    stop("formato$faltantes_texto debe ser un vector de texto.")
  }
  vector_texto(cfg$formato$columnas_texto, "formato$columnas_texto")
  hoja <- cfg$formato$hoja
  if (length(hoja) != 1 || is.na(hoja) ||
    !(is.character(hoja) && nzchar(hoja) ||
      is.numeric(hoja) && is.finite(hoja) &&
        hoja >= 1 && hoja == floor(hoja))) {
    stop("formato$hoja debe ser un nombre o un número entero positivo.")
  }
  for (n in c("solo_positivos", "generar_graficos")) {
    x <- cfg$analisis[[n]]
    if (!is.logical(x) || length(x) != 1 || is.na(x)) {
      stop("analisis$", n, ": usa TRUE o FALSE.")
    }
  }
  if (!is.numeric(cfg$analisis$codigos_faltantes) ||
    any(!is.finite(cfg$analisis$codigos_faltantes))) {
    stop(
      "analisis$codigos_faltantes debe contener números finitos; ",
      "usa numeric() si no hay códigos."
    )
  }
  for (n in c("multiplicador_iqr", "corte_z")) {
    x <- cfg$analisis[[n]]
    if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x <= 0) {
      stop("analisis$", n, ": número positivo requerido.")
    }
  }
  texto(cfg$analisis$etiqueta_variable, "analisis$etiqueta_variable")
  texto(cfg$analisis$unidad, "analisis$unidad")
  cfg
}

necesitar_paquete <- function(nombre) {
  if (!requireNamespace(nombre, quietly = TRUE)) {
    stop(
      "Falta ", nombre,
      '. Instálalo con install.packages("', nombre, '").'
    )
  }
}

leer_base_daniel <- function(archivo, formato) {
  extension <- tolower(tools::file_ext(archivo))
  if (!file.exists(archivo)) stop("No existe la entrada: ", archivo)
  datos <- switch(extension,
    rds = readRDS(archivo),
    csv = ,
    tsv = {
      separador <- if (extension == "tsv") "\t" else formato$separador
      # El conteo de campos evita desplazar columnas al importar CSV/TSV.
      conexion <- file(archivo, open = "rt", encoding = formato$codificacion)
      campos <- tryCatch(count.fields(conexion,
        sep = separador, quote = '"',
        comment.char = "", blank.lines.skip = TRUE
      ), finally = close(conexion))
      cabecera <- scan(archivo,
        what = character(), nlines = 1, sep = separador,
        quote = '"', quiet = TRUE, comment.char = "", na.strings = character(),
        fileEncoding = formato$codificacion
      )
      if (!length(cabecera) || anyNA(cabecera) ||
        any(!nzchar(cabecera)) || anyDuplicated(cabecera)) {
        stop("CSV/TSV: los encabezados deben ser únicos y no vacíos.")
      }
      if (any(campos[!is.na(campos)] != length(cabecera))) {
        stop(
          "CSV/TSV: una fila tiene distinta cantidad de campos ",
          "que el encabezado. Revisa separador y comillas."
        )
      }
      ausentes <- setdiff(formato$columnas_texto, cabecera)
      if (length(ausentes)) {
        stop(
          "columnas_texto contiene nombres ausentes: ",
          paste(ausentes, collapse = ", ")
        )
      }
      # Los identificadores configurados como texto conservan ceros iniciales.
      clases <- if (length(formato$columnas_texto)) {
        setNames(
          rep("character", length(formato$columnas_texto)),
          formato$columnas_texto
        )
      } else {
        NA
      }
      importado <- withCallingHandlers(
        read.table(archivo,
          header = TRUE, sep = separador, row.names = NULL,
          dec = formato$decimal, fileEncoding = formato$codificacion,
          na.strings = formato$faltantes_texto, colClasses = clases,
          stringsAsFactors = FALSE, check.names = FALSE,
          comment.char = "", quote = '"'
        ),
        warning = function(w) {
          if (grepl(
            "EOF within quoted string", conditionMessage(w),
            fixed = TRUE
          )) {
            stop("CSV/TSV: hay un campo entre comillas sin cerrar.")
          }
        }
      )
      # Los campos multilínea deben producir el mismo número de registros.
      if (nrow(importado) != sum(!is.na(campos)) - 1L) {
        stop(
          "CSV/TSV: no se pudieron leer todos los registros; ",
          "revisa las comillas."
        )
      }
      importado
    },
    sav = {
      necesitar_paquete("haven")
      haven::read_sav(archivo)
    },
    xlsx = ,
    xls = {
      necesitar_paquete("readxl")
      readxl::read_excel(archivo,
        sheet = formato$hoja, na = formato$faltantes_texto,
        .name_repair = "minimal"
      )
    },
    stop(
      "Formato no soportado: .", extension,
      ". Usa RDS, CSV, TSV, SAV, XLSX o XLS."
    )
  )
  if (!is.data.frame(datos) || !ncol(datos)) {
    stop("La entrada debe ser una tabla con columnas.")
  }
  if (is.null(names(datos)) || anyNA(names(datos)) ||
    any(!nzchar(names(datos))) || anyDuplicated(names(datos))) {
    stop("Los nombres de columnas deben ser únicos y no vacíos.")
  }
  if (any(vapply(datos, inherits, logical(1), "haven_labelled"))) {
    necesitar_paquete("haven")
  }
  datos
}

resolver_columna <- function(datos, columnas) {
  if (!is.null(columnas$ingreso)) {
    if (!columnas$ingreso %in% names(datos)) {
      stop(
        "No existe la columna configurada '",
        columnas$ingreso,
        "'. Ajusta columnas$ingreso o --columna. Disponibles: ",
        paste(names(datos), collapse = ", ")
      )
    }
    elegida <- columnas$ingreso
  } else {
    candidatas <- intersect(columnas$aliases, names(datos))
    if (length(candidatas) != 1) {
      stop(
        "Los alias deben resolver exactamente una columna; encontrados: ",
        paste(candidatas, collapse = ", "),
        ". Configura el nombre explícitamente."
      )
    }
    elegida <- candidatas[1]
  }
  sin_tipo_observable <- is.logical(datos[[elegida]]) &&
    all(is.na(datos[[elegida]]))
  if (!is.numeric(datos[[elegida]]) && !sin_tipo_observable) {
    stop(
      "La columna '", elegida,
      "' no es numérica. Revisa separador, decimal o formato de origen; ",
      "no se convierten factores ni moneda automáticamente."
    )
  }
  elegida
}
