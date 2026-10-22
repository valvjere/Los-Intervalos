# Aporte TASK 03 — Faltantes, tipografía y codificación categórica

Responsable: Lucas. Técnicas 1, 2 y 7 de T5. Las cifras salen de
`scripts/03_faltantes_y_categorias.R`, que trabaja sobre la población de estudio (ocupados de
18 a 65 años).

---

## Sección 2 — Tabla consolidada de problemas identificados

Atípicos y escalado: cifras de `scripts/04_tecnicas_5_6_10.R` (Daniel) sobre la base de este
script, pendientes de que Daniel las confirme.

| Tipo de problema | Columna afectada | Cantidad | Descripción |
|---|---|---|---|
| Datos duplicados | Llave `ID_AMO`, `ID_TRIMESTRE`, `Consecutivo`, `ID_VIVIENDA`, `ID_HOGAR`, `ID_LINEA` | 0 | Sin duplicados exactos ni por llave. Sin `Consecutivo` la llave no es única, porque `ID_VIVIENDA` solo va de 1 a 12. |
| Valores faltantes | `Ingreso_principal`, `Nivel_educativo`, `Amos_educacion` | 4 933 en ingreso; 7 en nivel; 7 en años | Ingreso: 1 160 con 99999999, 195 con 0, 2 994 vacíos y 584 auxiliares no remunerados (fuera de alcance). No respuesta real: 6,83 %. Educación: código 9 / 99 = "No especificado". |
| Errores tipográficos | Etiquetas de `Educacion_titulo` | 1 etiqueta; 1 380 registros | "Maestria" sin tilde y categoría redundante "Maestría y doctorado" (código 6, sin registros). |
| Formatos incorrectos | *[Pendiente TASK 02 — Jeremy]* | | |
| Codificación de variables categóricas | `Sexo`, `Nivel_educativo`, `Educacion_titulo`, `Edad` | 4 variables | Códigos numéricos de SPSS. El nivel educativo no distingue posgrado y el grupo de edad del INEC empieza en 15 años. |
| Valores atípicos | `Ingreso_principal` | 5 680 por IQR (9,57 %); 783 por \|Z\| > 3 (1,32 %) | Fuera de [−168 750; 947 917] colones. Se marcan para revisión, no se eliminan. |
| Normalización y escalado | `Ingreso_principal` | 59 339 valores | Asimetría positiva fuerte (g1 = 6,77); con ln(x) baja a −0,78. También se estandarizó con Z. |
| Incoherencias lógicas | *[Pendiente TASK 02 — Jeremy]* | | |
| Reducción de dimensionalidad | Conjunto completo (332 variables) | 313 descartadas | Se conservaron 19 variables: identificadores, demográficas, educativas, de actividad, ingreso y horas. |

---

## Sección 3 — Técnicas aplicadas

### Manejo de valores faltantes (Técnica 1)

La población son 64 279 personas ocupadas de 18 a 65 años y la variable respuesta es
`Ingreso_principal` (ingreso mensual neto). El INEC marca la falta de dato con dos códigos:
99999999 ("No especificado") y 0.

`read_sav()` convierte esos códigos en `NA` sin avisar, así que los `.sav` se releyeron con
`user_na = TRUE` para poder contarlos. Además, se cruzó el ingreso con `Posicion_empleo`:

| Situación del ingreso | Casos | % |
|---|---:|---:|
| Válido (mayor que 0) | 59 346 | 92,33 |
| Vacío en origen (asalariados) | 2 994 | 4,66 |
| Código 99999999 | 1 160 | 1,80 |
| Auxiliar no remunerado | 584 | 0,91 |
| Código 0 (cuenta propia y empleadores) | 195 | 0,30 |

Los auxiliares no remunerados trabajan sin pago: no es no respuesta, sino población fuera del
análisis salarial. Sin ellos, la no respuesta es de **6,83 %** (4 349 de 63 695).

**Tratamiento.** De las estrategias de T5 (Ramírez Benavides, s. f., diap. 6) se usó la
eliminación por lista. Las demás se descartaron:

- **Media, mediana o moda:** el ingreso es la variable a explicar; imputarlo inventa salarios,
  reduce la varianza y altera la brecha.
- **Valor constante:** solo aplica a variables categóricas.
- **Interpolación:** es para series de tiempo, y cada registro es una persona en un trimestre.
- **Regresión o k-NN:** predeciría el ingreso con sexo, edad y educación, justo la relación que se
  quiere estudiar.

La pérdida es menor al 7 %. Por sexo la no respuesta es parecida (7,20 % hombres, 6,29 %
mujeres), pero baja con la escolaridad (9,95 % en "Ninguno", 5,08 % en "Universitario con
título"). **Limitación:** la base limpia subrepresenta un poco a las personas con menos estudios.

En educación, `Nivel_educativo` = 9 y `Amos_educacion` = 99 significan "No especificado" (7 casos
cada uno). Se eliminaron los 7 de nivel educativo, por ser la variable explicativa principal; en
años de educación el 99 pasó a `NA` sin eliminar filas. Los vacíos de `Educacion_titulo` no son
faltantes: esa pregunta solo se hace a quien tiene estudios postsecundarios.

**Resultado:** −4 933 filas por ingreso y −7 por nivel educativo; quedan 59 339 personas.

### Corrección de errores tipográficos o inconsistencias (Técnica 2)

Como los datos son numéricos, se revisaron las etiquetas de valor de `Sexo`,
`Condicion_actividad`, `Nivel_educativo`, `Educacion_titulo`, `Grupo_edad` y `Posicion_empleo`,
buscando espacios sobrantes, errores de ortografía y categorías que solo difieren en tildes o
mayúsculas (Ramírez Benavides, s. f., diap. 7).

- **"Maestria"** sin tilde (código 7 de `Educacion_titulo`, 1 380 registros): se corrigió a
  "Maestría", y además se quitaron espacios sobrantes con `str_squish()`.
- **Categoría redundante:** el código 6 "Maestría y doctorado" (esquema anterior) convive con
  7 "Maestría" y 8 "Doctorado". No tiene registros en 2024-2025, así que se homologó sin perder
  información.

No se renombraron columnas porque los scripts del equipo usan los nombres oficiales del INEC.

### Codificación de variables categóricas (Técnica 7)

T5 presenta *one-hot*, *label encoding* y *binary/target encoding* (Ramírez Benavides, s. f.,
diap. 12). Se usaron las dos primeras, conservando las columnas originales:

- **Sexo:** factor `Sexo_cat` (Hombre, Mujer) y binaria `Mujer` (0 = Hombre, 1 = Mujer).
- **Nivel educativo:** entero ordinal `Nivel_educativo_ord` (0 a 7) y factor ordenado
  `Nivel_educativo_cat`. El INEC no distingue posgrado en esta variable; se tomó de
  `Educacion_titulo` (5 = Especialización, 6 y 7 = Maestría, 8 = Doctorado).

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

  Es ordinal y no *one-hot* porque el orden de los niveles es justo lo que se quiere relacionar
  con el salario.
- **Grupo de edad:** el `Grupo_edad` del INEC empieza en 15 años, así que se creó
  `Grupo_edad_estudio`: 18-24 (5 534), 25-34 (15 179), 35-49 (23 601) y 50-65 (15 025).

---

## Aporte a la Sección 4 — Herramientas utilizadas

R 4.6.1 con `haven` 2.5.5 (lectura de SPSS), `dplyr` 1.2.1 (transformación), `purrr` 1.2.2
(lectura de los ocho trimestres) y `stringr` 1.6.0 (limpieza de etiquetas).

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
