# Configuración parcial ECE; rutas relativas al repositorio.
list(
  entrada = NULL, # NULL busca las candidatas en el orden indicado.
  candidatas = c(
    "data/procesados/ece_2024_2025_preparado.rds",
    "data/procesados/ece_2024_2025_consolidado_crudo.rds"
  ),
  salida = NULL, # NULL crea Resultados_TASK_04/FECHA_HORA con nombre único.
  estado = paste0(
    "Diagnóstico preliminar de la base ",
    "recibida; población definida por el ", "equipo"
  ),
  columnas = list(
    ingreso = "Ingreso_principal", # Nombre exacto de la variable.
    aliases = c("Ingreso_principal", "ingreso_principal"),
    grupos = c(
      "Sexo", "Nivel_educativo", "Educacion", "ID_AMO",
      "ID_TRIMESTRE"
    ),
    identificadores = c(
      "ID_AMO", "ID_TRIMESTRE", "ID_VIVIENDA", "ID_HOGAR",
      "ID_LINEA", "Consecutivo"
    )
  ),
  formato = list(
    separador = ",", decimal = ".", codificacion = "UTF-8",
    faltantes_texto = c("", "NA"),
    columnas_texto = character(), # CSV/TSV: IDs con ceros iniciales.
    hoja = 1 # Para XLSX/XLS: nombre o número de hoja.
  ),
  analisis = list(
    codigos_faltantes = c(0, 99999999), # Específicos de esta fuente.
    solo_positivos = TRUE,
    multiplicador_iqr = 1.5, corte_z = 3,
    tipo_cuantil = 7L, # Interpolación de R; Horgan (2009), pp. 16–17.
    etiqueta_variable = "Ingreso mensual neto", unidad = "colones",
    generar_graficos = TRUE
  ),
  # Control nacional independiente de la población de la entrada.
  validacion = list(
    habilitada = TRUE, tolerancia_colones = 1,
    referencia = "info/inec/seempleoiiitri2010-jja2026-04.xlsx",
    carpeta_microdatos = "data", anios = c(2024, 2025), trimestres = 1:4,
    hojas = c(Total = "C4 total", Hombre = "C4 hombre", Mujer = "C4 mujer"),
    codigos_grupo = c(Hombre = 1, Mujer = 2),
    columnas = list(
      anio = "ID_AMO", trimestre = "ID_TRIMESTRE",
      actividad = "Condicion_actividad", grupo = "Sexo",
      valor = "Ingreso_bruto_principal", peso = "Factor_ponderacion"
    ),
    ocupado = 1, codigos_faltantes = c(0, 99999999),
    fila_periodos = 5, fila_valor = 8, columna_rotulo = 1,
    rotulo_esperado = "1. Rama de actividad", fila_nota = 45,
    nota_esperada = "ingreso bruto corriente",
    prefijos_periodo = c("EFM", "AMJ", "JAS", "OND"),
    romanos = c("I", "II", "III", "IV"),
    patron_archivo = "{trimestre} Trimestre {anio}.sav"
  )
)
