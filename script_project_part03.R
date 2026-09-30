library(tidyverse)

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