# Aporte TASK 03 — Faltantes, tipografía y codificación categórica

Responsable: Lucas. Técnicas 1, 2 y 7 de T5. Texto listo para pegar en la plantilla
`CI0115_PlantillaProyecto_LimpiezaDatos.docx`. Las cifras son las que imprime
`scripts/03_faltantes_y_categorias.R` al terminar.

> **Cifras preliminares.** Mientras no exista TASK 02 (Jeremy), el script aplica un filtro
> poblacional provisional (ocupados, 18 a 65 años, sexo 1 o 2). Si TASK 02 elimina además
> registros por incoherencias lógicas, hay que volver a correr el script y actualizar los números.

---

## Sección 2 — Filas de la tabla de problemas identificados

| Tipo de problema | Columna afectada | Cantidad | Descripción |
|---|---|---|---|
| Valores faltantes | `Ingreso_principal`, `Nivel_educativo`, `Amos_educacion` | 4 933 en ingreso; 7 en nivel educativo; 7 en años de educación | En ingreso: 1 160 con código 99999999 ("No especificado"), 195 con código 0, 2 994 sin dato en origen y 584 auxiliares no remunerados (sin salario por definición, fuera del alcance). Tasa de no respuesta real: 6,83 %. En educación, el código 9 / 99 significa "No especificado". |
| Errores tipográficos | Etiquetas de `Educacion_titulo` | 1 etiqueta; 1 380 registros | Etiqueta "Maestria" sin tilde y categoría redundante "Maestría y doctorado" (código 6, sin registros) que se solapa con "Maestría" (7) y "Doctorado" (8). |
| Codificación de variables categóricas | `Sexo`, `Nivel_educativo`, `Educacion_titulo`, `Edad` | 4 variables | Variables almacenadas como códigos numéricos de SPSS. El nivel educativo no distingue posgrado y el grupo de edad del INEC empieza en 15 años, por lo que no se ajusta a la población del estudio. |

---

## Sección 3 — Técnicas aplicadas

### Manejo de valores faltantes (Técnica 1)

La población de estudio son las 64 279 personas ocupadas de 18 a 65 años. La variable
respuesta del proyecto es el ingreso mensual neto del empleo principal (`Ingreso_principal`).
Según el diccionario oficial de la ECE, el INEC codifica la falta de dato en esta variable con
dos valores reservados: 99999999 ("No especificado") y 0.

Durante la exploración se detectó que la función `read_sav()` del paquete `haven` convierte
estos códigos en `NA` sin avisar, lo que impide saber cuántos casos corresponden a cada uno.
Por eso los archivos `.sav` se volvieron a leer con el argumento `user_na = TRUE`, que conserva
los códigos originales. Además, se cruzó el ingreso con la variable `Posicion_empleo` para
distinguir la no respuesta de la ausencia legítima de ingreso:

| Situación del ingreso | Casos | % de la población |
|---|---:|---:|
| Válido (mayor que 0) | 59 346 | 92,33 |
| Sin dato en origen (asalariados) | 2 994 | 4,66 |
| Código 99999999 "No especificado" | 1 160 | 1,80 |
| Auxiliar no remunerado (fuera de alcance) | 584 | 0,91 |
| Código 0 (cuenta propia y empleadores) | 195 | 0,30 |

Los 584 auxiliares no remunerados no son una no respuesta: trabajan sin recibir pago, por lo
que su ingreso no existe y quedan fuera de un análisis salarial. Sin ellos, la tasa de no
respuesta en ingreso es de **6,83 %** (4 349 de 63 695 personas).

**Tratamiento.** Se eliminaron las filas sin ingreso utilizable (eliminación por lista). No se
imputó, por dos razones. Primero, el ingreso es la variable que el proyecto busca explicar:
imputarlo con la media o la mediana fabricaría salarios, reduciría artificialmente la varianza
y alteraría la brecha que se quiere medir. Segundo, la pérdida es pequeña (menos del 7 %) y
la muestra restante sigue siendo amplia.

Para revisar si la eliminación introduce sesgo se comparó la tasa de no respuesta entre grupos.
Por sexo es similar (hombres 7,20 %, mujeres 6,29 %). Por nivel educativo disminuye conforme
aumenta la escolaridad (9,95 % en "Ninguno" frente a 5,08 % en "Universitario con título"). Esto
se reporta como **limitación**: la base limpia subrepresenta levemente a las personas con menor
escolaridad, lo que debe considerarse al interpretar la brecha por nivel educativo.

En las variables educativas, el código 9 de `Nivel_educativo` y el 99 de `Amos_educacion`
significan "No especificado" (7 casos en cada una). Como el nivel educativo es la variable
explicativa principal, se eliminaron sus 7 casos. En `Amos_educacion`, el 99 se convirtió a `NA`
sin eliminar filas, porque no es una variable central del análisis. Los vacíos de
`Educacion_titulo` no son faltantes: la pregunta solo se aplica a quienes cursaron educación
postsecundaria.

**Resultado:** se eliminaron 4 933 filas por ingreso no utilizable y 7 por nivel educativo no
especificado. La población pasó de 64 279 a 59 339 personas.

### Corrección de errores tipográficos o inconsistencias (Técnica 2)

Los datos de la ECE vienen codificados numéricamente, por lo que el texto susceptible de errores
son las etiquetas de valor de SPSS de las variables categóricas (`Sexo`, `Condicion_actividad`,
`Nivel_educativo`, `Educacion_titulo`, `Grupo_edad` y `Posicion_empleo`). En la exploración se
revisaron espacios sobrantes, ortografía y categorías que solo difieren en tildes o mayúsculas
(por ejemplo, "Sí", "si" o "SÍ").

El único error encontrado fue la etiqueta **"Maestria"** sin tilde (código 7 de
`Educacion_titulo`, 1 380 registros). Se corrigió a "Maestría" directamente en las etiquetas de
la variable, eliminando también espacios sobrantes con `str_squish()` del paquete `stringr`.

La revisión manual detectó además una **inconsistencia de categorías** en `Educacion_titulo`. La
variable conserva el código 6 "Maestría y doctorado", de un esquema anterior, junto con los
códigos 7 "Maestria" y 8 "Doctorado" del esquema vigente. Se verificó que el código 6 no tiene
registros en el período 2024-2025, por lo que ambos esquemas se homologaron sin perder
información.

Se decidió no renombrar las columnas (por ejemplo, a minúsculas), porque los scripts del resto
del equipo dependen de los nombres oficiales del INEC.

### Codificación de variables categóricas (Técnica 7)

En los archivos `.sav` las variables categóricas son números con etiquetas (por ejemplo,
1 = Hombre, 2 = Mujer). Se convirtieron a factores de R, conservando las columnas originales
para trazabilidad:

- **Sexo:** factor nominal `Sexo_cat` (Hombre, Mujer) y variable binaria `Mujer`
  (0 = Hombre, 1 = Mujer), útil para modelos posteriores.
- **Nivel educativo:** codificación ordinal (*label encoding*) en `Nivel_educativo_ord` (0 a 7)
  y factor ordenado `Nivel_educativo_cat`. La variable original del INEC llega solo hasta
  "Universitario con título" y no distingue posgrado. Ese dato se tomó de `Educacion_titulo`
  (5 = Especialización, 6 y 7 = Maestría, 8 = Doctorado), con lo que se obtuvo una escala de
  ocho niveles:

  | Orden | Nivel | Personas |
  |---:|---|---:|
  | 0 | Ninguno | 914 |
  | 1 | Primaria incompleta | 4 178 |
  | 2 | Primaria completa | 13 233 |
  | 3 | Secundaria incompleta | 11 952 |
  | 4 | Secundaria completa | 13 262 |
  | 5 | Universitario sin título | 2 614 |
  | 6 | Universitario con título de grado | 11 700 |
  | 7 | Posgrado | 1 486 |

  Se usó codificación ordinal y no *one-hot* porque los niveles tienen un orden natural, que es
  justamente el que el proyecto quiere relacionar con el salario.
- **Grupo de edad:** la variable `Grupo_edad` del INEC empieza en 15 años e incluye "60 y más",
  por lo que no se ajusta al rango del estudio. Se creó `Grupo_edad_estudio`, un factor ordenado
  con los tramos 18-24 (5 534), 25-34 (15 179), 35-49 (23 601) y 50-65 (15 025), sin sobrescribir
  la variable original.

---

## Aporte a la Sección 4 — Herramientas utilizadas

R 4.6.1 con los paquetes `haven` 2.5.5 (lectura de archivos SPSS conservando códigos de no
respuesta), `dplyr` 1.2.1 (transformación de datos), `purrr` 1.2.2 (lectura de los ocho
trimestres) y `stringr` 1.6.0 (limpieza de etiquetas de texto).

## Aporte a la Sección 6 — Referencias (APA 7)

- Instituto Nacional de Estadística y Censos. (2024–2025). *Encuesta Continua de Empleo:
  microdatos I trimestre 2024 a IV trimestre 2025* [Conjunto de datos]. INEC.
- R Core Team. (2026). *R: A language and environment for statistical computing* (Versión 4.6.1)
  [Software]. R Foundation for Statistical Computing. https://www.R-project.org/
- Ramírez Benavides, K. (s. f.). *Limpieza de datos* [Presentación T5, CI-0115 Probabilidad y
  Estadística]. Escuela de Ciencias de la Computación e Informática, Universidad de Costa Rica.
- Walpole, R. E., Myers, R. H., Myers, S. L., & Ye, K. (2007). *Probabilidad y estadística para
  ingeniería y ciencias* (8.ª ed.). Pearson Educación.
- Wickham, H. (2025). *stringr: Simple, consistent wrappers for common string operations*
  (Versión 1.6.0) [Paquete de R]. https://doi.org/10.32614/CRAN.package.stringr
- Wickham, H., François, R., Henry, L., Müller, K., & Vaughan, D. (2026). *dplyr: A grammar of
  data manipulation* (Versión 1.2.1) [Paquete de R]. https://doi.org/10.32614/CRAN.package.dplyr
- Wickham, H., & Henry, L. (2026). *purrr: Functional programming tools* (Versión 1.2.2)
  [Paquete de R]. https://doi.org/10.32614/CRAN.package.purrr
- Wickham, H., Miller, E., & Smith, D. (2025). *haven: Import and export 'SPSS', 'Stata' and
  'SAS' files* (Versión 2.5.5) [Paquete de R]. https://doi.org/10.32614/CRAN.package.haven
