#===========================================================================#
# Pruebas de hipótesis  de dos medias                                       #
# Autor: Alexis Adonai Morales Alberto                                      #
# SCIDATA                                                                   # 
# Diplomado: Diseño de experimentos                                         #
# Código de R para clase                                                    #
#===========================================================================#

# Carga de modulos 

import pandas as pd 
import numpy as np
import seaborn as sns 
import matplotlib.pyplot as plt
from datetime import datetime 
from scipy import stats 
from statsmodels.stats.diagnostic import normal_ad
from statsmodels.stats.weightstats import ttest_ind

# Configuración del estilo 

sns.set_theme(style = "darkgrid")

# Cargar datos 

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

Datos['edad'] = Datos ['fecha nacimiento'].apply(lambda x: calcular_edad2(x) if pd.notnull(x) else None)

# Limpieza de los datos 

Datos = Datos[~Datos['estatura'].isna()]
Datos = Datos[Datos['estatura'] != 10]
Datos['estatura'] = Datos['estatura'].apply(lambda x: x*100 if x < 100 else x)
Datos['sexo'] = Datos['sexo'].astype('category')

## Corroborar limpieza 

list(set(Datos['estatura']))

## Verificar tipo de dato

print(Datos['estatura'].dtypes)
print(Datos['sexo'].dtypes)

# Gráfico boxplot 

plt.figure(figsize = (8,6), dpi = 500)
sns.boxplot(x = 'sexo', y = 'estatura', data = Datos,
            palette = {'Femenino': '#5b2c6f',
                       'Masculino': '#0e6251'})
sns.stripplot(x = 'sexo', y = 'estatura', data = Datos, jitter = True,
              alpha = 0.2, color = 'gray')
media_estaturas = Datos.groupby('sexo')['estatura'].mean()
for i, sexo in enumerate(media_estaturas.index):
  plt.scatter(i, media_estaturas[sexo], color = {'Femenino': '#f4d03f',
                                                 'Masculino': '#e59866'}[sexo],
              s = 100, zorder = 5)
plt.title('Distribución de la estatura de los alumnos de SciData\npor sexo\n2025')
plt.xlabel('Sexo')
plt.ylabel('Estatura en cm.')
plt.tight_layout()
plt.show()

# Pruebas de normalidad 

print(stats.jarque_bera(Datos['estatura']))
print(stats.jarque_bera(Datos[Datos['sexo'] == "Masculino"]['estatura']))
print(stats.jarque_bera(Datos[Datos['sexo'] == "Femenino"]['estatura']))

# Prueba de igualdad de varianzas (levene) ----

## Levene 

print(stats.levene(
  Datos[Datos['sexo'] == "Masculino"]['estatura'],
  Datos[Datos['sexo'] == "Femenino"]['estatura'],
  center = 'mean'
))

## Fligner - Kileen

print(stats.fligner(
  Datos[Datos['sexo'] == "Masculino"]['estatura'],
  Datos[Datos['sexo'] == "Femenino"]['estatura'],
  center = 'mean'
))

# Prueba t de dos medias 

t_stat, p_value, df = ttest_ind(
  Datos[Datos['sexo'] == "Masculino"]['estatura'],
  Datos[Datos['sexo'] == "Femenino"]['estatura'],
  usevar = 'pooled',
  alternative = 'two-sided'
) 

print(f"Estadístico t: {t_stat:.4f}")
print(f"p-value: {p_value:.4f}")
