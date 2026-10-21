# ==============================================================================
# PROYECTO CI-0115 PROBABILIDAD Y ESTADÍSTICA - UCR
# EQUIPO: Los Intervalos
# TAREA: TASK_01_Ingesta_y_Fusion
# Responsable: Ignacio (Coordinador & Arquitectura de Datos)
# ==============================================================================
# Versión Python equivalente
# ==============================================================================

import os
import glob
import time
import pandas as pd
import pyreadstat

def main():
    print("=" * 66)
    print("   INICIANDO TASK 01: INGESTA Y FUSIÓN DE DATOS (ECE 2024-2025)   ")
    print("=" * 66)

    data_dir = "data"
    sav_files = sorted(glob.glob(os.path.join(data_dir, "*.sav")))

    if not sav_files:
        raise FileNotFoundError("No se encontraron archivos .sav en 'data/'")

    print(f"Archivos detectados ({len(sav_files)} trimestres):")
    for f in sav_files:
        print(f" - {os.path.basename(f)}")

    # Variables clave seleccionadas (Técnica 9: Reducción de Dimensionalidad)
    variables_seleccionadas = [
        "ID_AMO", "ID_TRIMESTRE", "ID_VIVIENDA", "ID_HOGAR", "ID_LINEA", "Consecutivo",
        "Sexo", "Edad", "Grupo_edad",
        "Condicion_actividad",
        "Nivel_educativo", "Amos_educacion", "Educacion_titulo", "Educacion_nivel_grado",
        "Ingreso_principal", "Ingreso_total", "Salario_bruto",
        "Horas_normales_principal", "Horas_efectivas_principal"
    ]

    t0 = time.time()
    dfs = []
    resumen_list = []

    for f in sav_files:
        base = os.path.basename(f)
        print(f"Cargando {base} ... ", end="", flush=True)
        # Lectura con pyreadstat
        df_quarter, meta = pyreadstat.read_sav(f, usecols=None)
        filas_orig, cols_orig = df_quarter.shape
        
        # Filtrado de columnas
        cols_presentes = [c for c in variables_seleccionadas if c in df_quarter.columns]
        df_sub = df_quarter[cols_presentes].copy()
        dfs.append(df_sub)
        
        print(f"OK ({filas_orig:,} filas, {cols_orig} cols -> {df_sub.shape[1]} cols seleccionadas)")
        resumen_list.append({
            "Archivo": base,
            "Filas_Iniciales": filas_orig,
            "Columnas_Iniciales": cols_orig
        })

    # Fusión consolidada
    print("\nFusionando microdatos en un dataset consolidado...")
    df_consolidado = pd.concat(dfs, ignore_index=True)
    n_total_filas, n_total_cols = df_consolidado.shape

    os.makedirs("data/procesados", exist_ok=True)
    out_csv = "data/procesados/ece_2024_2025_consolidado_crudo.csv"
    print(f"Guardando dataset consolidado en {out_csv} ... ", end="")
    df_consolidado.to_csv(out_csv, index=False)
    print("OK")

    print("\n" + "=" * 66)
    print("                RESUMEN DE INGESTA Y FUSIÓN                      ")
    print("=" * 66)
    resumen_df = pd.DataFrame(resumen_list)
    print(resumen_df.to_string(index=False))
    print("-" * 66)
    print(f"TOTAL FILAS INICIALES:     {n_total_filas:,}")
    print(f"TOTAL COLUMNAS ORIGINALES: {resumen_list[0]['Columnas_Iniciales']}")
    print(f"COLUMNAS SELECCIONADAS:    {n_total_cols}")
    print(f"TIEMPO TOTAL:              {time.time() - t0:.2f} segundos")
    print("=" * 66)

if __name__ == "__main__":
    main()
