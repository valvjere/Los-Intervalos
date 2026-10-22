# TASK 04 — Atípicos, escalado y validación

Responsable: Daniel. El módulo recibe la base del equipo y agrega diagnósticos;
conserva todas las filas y columnas originales. Los resultados son preliminares
hasta integrar la limpieza de los compañeros.

## Ejecución

Dependencias de R: `haven`, `readxl` y `ggplot2`.

```r
install.packages(c("haven", "readxl", "ggplot2"))
```

Desde la raíz del repositorio:

```bash
Rscript scripts/04_tecnicas_5_6_10.R
```

También admite **Source** en RStudio. El perfil
[`task_04_ece.R`](../../config/task_04_ece.R) busca la base preparada en
`data/procesados/ece_2024_2025_preparado.rds`; si falta, usa
`ece_2024_2025_consolidado_crudo.rds`. Este último se genera con
`Rscript scripts/01_ingesta_y_fusion.R`.

Para otra entrega, la configuración define variable, grupos, códigos faltantes,
unidades y formato. Hay un
[ejemplo](../../config/task_04_ejemplo_base_nueva.R) para CSV.

```bash
Rscript scripts/04_tecnicas_5_6_10.R \
  --config config/mi_base.R --entrada data/procesados/nueva_base.csv
```

Formatos: RDS, CSV, TSV, SAV, XLSX y XLS. La variable debe ser numérica.
`--salida carpeta` establece una carpeta vacía o nueva; `--sin-graficos` omite
las imágenes y `--sin-validacion` deja la técnica 10 pendiente.
`--ayuda` muestra las opciones.

Prioridad: argumentos > entorno (`DANIEL_CONFIG`, `DANIEL_ENTRADA`,
`DANIEL_SALIDA`, conservados por compatibilidad) > configuración > perfil ECE.
Las rutas de argumentos y entorno parten de la carpeta actual; las del archivo
de configuración, de la raíz del repositorio.

## Técnicas

| Técnica | Cálculo y alcance |
| --- | --- |
| **5: atípicos** | IQR: límites `Q1 - 1.5*IQR` y `Q3 + 1.5*IQR`. Complemento `abs(Z) > 3`, con media y desviación muestral. Marca casos; no los elimina. |
| **6: escalado** | `ln(x)` para valores utilizables positivos y Z muestral. Compara media, mediana, desviación y asimetría `g1`. No demuestra normalidad. |
| **10: validación** | Reconstruye medias de ingreso **bruto** con factores de expansión de los SAV originales y las coteja con INEC. Es un control nacional independiente del ingreso neto de la base recibida. |

Los umbrales y el tipo de cuantil son configurables. El perfil usa `type=7`.
Los boxplots por grupo calculan sus propios cuartiles; las banderas usan los
límites globales. `NA` significa no evaluable. La validación externa informa
cobertura y coincidencias por separado; una referencia ausente queda pendiente.

## Salidas

Cada ejecución crea una carpeta en `Resultados_TASK_04/`:

- `datos_diagnosticados.rds`: entrada más columnas `d5_` y `d6_`.
- `tecnica_05_*.csv`, `tecnica_06_escalas.csv`, `tecnica_10_*.csv`: resultados.
- `casos_para_revision.csv`: observaciones señaladas o no utilizables.
- `resultado_antes_despues.csv`, `retencion.csv`: conservación del módulo;
  la retención final del equipo queda pendiente.
- Gráficas PNG, `aporte_plantilla.txt` y `avisos.txt`.
- Configuración resuelta, trazabilidad MD5, versiones y referencias.

## Organización y pruebas

`flujo.R` coordina lectura → T5 → T6 → T10 → resultados. Los archivos
`tecnica_*.R` contienen el análisis; los demás separan lectura, gráficas,
configuración del INEC y exportación. Cada archivo tiene menos de 300 líneas.

```bash
Rscript tests/task_04/test_configuracion_y_flujo.R
Rscript tests/task_04/test_tecnicas_5_6_10.R --integracion
```

Fórmulas, páginas y referencias:
[fundamento en los libros](../../docs/Fundamento_Libros_TASK_04.adoc).
Alcance del informe: [aporte TASK 04](../../docs/Aporte_TASK_04_Limpieza.adoc).
