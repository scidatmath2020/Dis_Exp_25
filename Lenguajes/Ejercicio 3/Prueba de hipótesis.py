#===========================================================================#
# Pruebas de hipótesis                                                      #
# Autor: Alexis Adonai Morales Alberto                                      #
# SCIDATA                                                                   # 
# Diplomado: Diseño de experimentos                                         #
# Código de Python para clase                                               #
#===========================================================================#

reticulate::repl_python()

# Llamar / Importar los modulos 

import pandas as pd
import numpy as np 
from scipy import stats
from datetime import datetime

# Importación de los datos 

Datos = pd.read_excel("Bases de datos/datos_adonai.xlsx")

# Función para calcular edades a partir de una fecha 

## En caso de que la columna se detecte como str 

def calcular_edad(fecha_nac_str):
  fecha_nac = datetime.strptime(fecha_nac_str, "%Y-%m-%d")
  hoy = datetime.today()
  edad = hoy.year - fecha_nac.year
  return edad

## En caso de que la columna se detecte como strtime 

def calcular_edad2(fecha_nac):
  hoy = datetime.today()
  edad = hoy.year - fecha_nac.year
  return edad


print(calcular_edad("1998-07-27"))

# Filtro de datos para México 

Edad_mexico = Datos[(Datos['pais'] == "México") & (~Datos['pais'].isna())].copy()

# Cálculo de la edad 

Edad_mexico['edad'] = Edad_mexico['fecha nacimiento'].apply(lambda x: calcular_edad2(x) if pd.notnull(x) else None)

# Media de la edad 

np.mean(Edad_mexico['edad'])

# Prueba t para la media 

## H0: mu = 29 
## HA: mu =/ 29 

t_resultado = stats.ttest_1samp(Edad_mexico['edad'].dropna(), popmean = 29)

print("\nPrueba t:")
print(f"Estadístico t: {t_resultado.statistic:.4f}")
print(f"Valor-p: {t_resultado.pvalue:.4f}")


# Prueba para la media 2 

Estatura_hombres = Datos[(Datos['sexo'] == "Masculino")].copy()

# Media de la edad 

np.mean(Estatura_hombres['estatura'])

# Prueba t para la media 

## H0: mu = 170
## HA: mu =/ 170 

t_resultado = stats.ttest_1samp(Estatura_hombres['estatura'].dropna(), popmean = 170)

print("\nPrueba t:")
print(f"Estadístico t: {t_resultado.statistic:.4f}")
print(f"Valor-p: {t_resultado.pvalue:.4f}")
