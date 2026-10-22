# Configuración parcial ECE; rutas relativas al repositorio.
list(
  entrada = "data/procesados/base_companero.csv",
  estado = "Base del compañero: verificar población y decisiones previas",
  columnas = list(
    ingreso = "salario_neto", aliases = character(),
    grupos = c("genero", "escolaridad"), identificadores = c("id_persona")
  ),
  formato = list(separador = ";", decimal = ",", columnas_texto = "id_persona"),
  analisis = list(
    codigos_faltantes = c(-999), solo_positivos = TRUE,
    tipo_cuantil = 2L,
    etiqueta_variable = "Salario neto", unidad = "colones"
  ),
  validacion = list(habilitada = FALSE)
)
