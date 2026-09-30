# Estadísticas descriptivas por nivel educativo
resumen <- datos %>%
  group_by(Educacion) %>%
  summarise(
    Observaciones = n(),
    Media = mean(Ingreso_principal, na.rm = TRUE),
    Mediana = median(Ingreso_principal, na.rm = TRUE),
    Desviacion_estandar = sd(Ingreso_principal, na.rm = TRUE),
    .groups = "drop"
  )
print(resumen)


write_csv(resumen, "Tabla_Descriptiva.csv")

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

ggsave(
  "Boxplot_Ingresos.png",
  plot = grafico,
  width = 9,
  height = 6,
  dpi = 300
)


# Relación entre horas e ingreso
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

# Comparación por sexo: reutiliza el boxplot general
grafico_sexo <- grafico +
  facet_wrap(~Sexo) +
  labs(title = "Ingreso por nivel educativo y sexo")

ggsave(
  "Boxplot_Sexo.png",
  plot = grafico_sexo,
  width = 10, height = 6, dpi = 300
)

# Comparación por zona
grafico_zona <- grafico +
  facet_wrap(~Zona) +
  labs(title = "Ingreso por nivel educativo y zona")

ggsave(
  "Boxplot_Zona.png",
  plot = grafico_zona,
  width = 10, height = 6, dpi = 300
)
