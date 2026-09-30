# ETAPA 3 | Análisis exploratorio de la brecha de ingresos
# Requisitos: objeto `datos` preparado en las etapas anteriores y paquetes
# dplyr, ggplot2 y readr cargados; scales se utiliza mediante su namespace.
# Columnas: Educacion, Ingreso_principal, Horas_trabajadas, Sexo y Zona.
# Los resultados no aplican ponderadores y se guardan en el directorio de trabajo.

# 1. Resumen por nivel educativo ---------------------------------------------
# n() cuenta todas las filas del grupo, incluso si falta el ingreso; las otras
# estadísticas omiten ingresos NA. El conteo no representa personas únicas.
resumen <- datos %>%
  group_by(Educacion) %>%
  summarise(
    Observaciones = n(),
    Media = mean(Ingreso_principal, na.rm = TRUE),
    Mediana = median(Ingreso_principal, na.rm = TRUE),
    Desviacion_estandar = sd(Ingreso_principal, na.rm = TRUE),
    # Devuelve una tabla sin agrupación para su uso posterior.
    .groups = "drop"
  )
print(resumen)


# Exporta las cifras sin redondear; el informe presenta valores redondeados.
write_csv(resumen, "Tabla_Descriptiva.csv")

# 2. Distribución del ingreso por educación ---------------------------------
# La caja resume el centro y la dispersión; los puntos fuera de los bigotes
# son valores atípicos según el boxplot, no necesariamente errores.
# log10 requiere ingresos positivos: verificar ceros y negativos en la base.
# La transformación ocurre antes del cálculo estadístico del boxplot.
grafico <- ggplot(
  datos,
  aes(x = Educacion, y = Ingreso_principal, fill = Educacion)
) +
  geom_boxplot(alpha = 0.7) +
  scale_y_log10(labels = scales::label_comma()) +
  labs(
    title = "Ingreso mensual según nivel educativo",
    subtitle = "Muestra agrupada de 2024–2025, sin ponderar",
    x = NULL,
    y = "Ingreso mensual (escala logarítmica)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Exporta el gráfico explícito a 300 dpi; width y height se expresan en pulgadas.
ggsave(
  "Boxplot_Ingresos.png",
  plot = grafico,
  width = 9,
  height = 6,
  dpi = 300
)


# 3. Relación entre horas e ingreso -----------------------------------------
# Cada punto representa una observación. La transparencia reduce la saturación
# en zonas con superposición; los colores distinguen los niveles educativos.
grafico_horas <- ggplot(
  datos,
  aes(
    x = Horas_trabajadas,
    y = Ingreso_principal,
    color = Educacion
  )
) +
  geom_point(alpha = 0.2, size = 1) +
  scale_y_log10(labels = scales::label_comma()) +
  labs(
    title = "Ingreso mensual y horas habituales de trabajo",
    x = "Horas habituales del empleo principal",
    y = "Ingreso mensual (escala logarítmica)",
    color = "Nivel educativo"
  ) +
  theme_minimal()

ggsave(
  "Dispersión_Ingreso_Horas.png",
  plot = grafico_horas,
  width = 10, height = 6, dpi = 300
)

# 4. Comparación por sexo ---------------------------------------------------
# Reutiliza el gráfico general y crea un panel por sexo. facet_wrap conserva
# escalas comunes de forma predeterminada para facilitar la comparación.
grafico_sexo <- grafico +
  facet_wrap(~Sexo) +
  labs(title = "Ingreso por nivel educativo y sexo")

ggsave(
  "Boxplot_Sexo.png",
  plot = grafico_sexo,
  width = 10, height = 6, dpi = 300
)

# 5. Comparación por zona ---------------------------------------------------
# Mantiene el diseño y la escala del gráfico general en los paneles de zona.
# Estas comparaciones son descriptivas y no controlan por otras variables.
grafico_zona <- grafico +
  facet_wrap(~Zona) +
  labs(title = "Ingreso por nivel educativo y zona")

ggsave(
  "Boxplot_Zona.png",
  plot = grafico_zona,
  width = 10, height = 6, dpi = 300
)
