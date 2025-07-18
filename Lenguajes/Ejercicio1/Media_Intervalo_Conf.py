#===========================================================================#
# Estimación puntual, media e intervalos de confianza                       #
# Autor: Alexis Adonai Morales Alberto                                      #
# SCIDATA                                                                   #
# Código de Python para clase                                               #
#===========================================================================#

import numpy as np
from scipy import stats
import pandas as pd

# Datos de prueba ----

Mediciones = np.array([
  3.6, 6.9, 5.1, 4.2, 5.5, 7.2, 3.0, 5.8,
  4.9, 9.9, 7.1, 5.4, 6.2, 4.5, 6.3, 8.2,
  5.7, 4.4, 7.9, 3.2
])

# Media ----

## Manual

X_MediaM = np.sum(Mediciones) / len(Mediciones)
print("Media (manual):", X_MediaM)

## Función

X_Media = np.mean(Mediciones)
print("Media (función):", X_Media)

# Desviación estándar ----

## Manual

Sigma_manual = np.sqrt(np.sum((Mediciones - X_Media) ** 2) / (len(Mediciones) - 1))

print("Desviación estándar (manual):", Sigma_manual)

## Función

SD = np.std(Mediciones, ddof=1)

print("Desviación estándar (función):", SD)

# Grados de libertad ----

GL = len(Mediciones) - 1
print("Grados de libertad:", GL)

# Nivel de confianza ----

alfa2 = (1 - 0.95) / 2

# Cálculo de t al 5% ----

t_critico = stats.t.ppf(1 - alfa2, df=GL)
print("Valor crítico t al 95%:", t_critico)

# Cálculo de límites ----

error_tipico = SD / np.sqrt(len(Mediciones))

## Inferior

LI = X_Media - t_critico * error_tipico

## Superior

LS = X_Media + t_critico * error_tipico

print("Límite inferior:", LI)
print("Límite superior:", LS)

# Función general para intervalo de confianza ----

def intervalo_confianza(x, alpha=0.05):
  n = len(x)
  media = np.mean(x)
  sd = np.std(x, ddof=1)
  error_tipico = sd / np.sqrt(n)
  t_crit = stats.t.ppf(1 - alpha/2, df=n - 1)
  LI = media - t_crit * error_tipico
  LS = media + t_crit * error_tipico
  salida = pd.DataFrame({
    "Estadísticos": ["Media", "Límite inferior", "Límite superior"],
    "Valores": [media, LI, LS]
    })
  return salida


# Uso de la función

resultado = intervalo_confianza(Mediciones, alpha=0.05)

print(resultado)


# Estimación de la variación ----

## Manual
VarianzaM = np.sum((Mediciones - np.mean(Mediciones)) ** 2) / (len(Mediciones) - 1)
print("Varianza (manual):", VarianzaM)

## Función
Varianza = np.var(Mediciones, ddof=1)
print("Varianza (función):", Varianza)

# Grados de libertad
gl = len(Mediciones) - 1

# Obtenemos los valores críticos de Chi-cuadrada
alpha = 0.05
ChiS = stats.chi2.ppf(alpha / 2, df=gl)
ChisI = stats.chi2.ppf(1 - (alpha / 2), df=gl)
print("Chi2 inferior (percentil 2.5%):", ChiS)
print("Chi2 superior (percentil 97.5%):", ChisI)

# Cálculo del intervalo ----
## Superior
LSV = (gl * Varianza) / ChiS
print("Límite superior del IC de la varianza:", LSV)

## Inferior
LIV = (gl * Varianza) / ChisI
print("Límite inferior del IC de la varianza:", LIV)

# Construcción de función para cálculo directo ----
def intervalo_varianza(x, alpha=0.05):
    n = len(x)
    gl = n - 1
    var_muestral = np.var(x, ddof=1)
    chi_inf = stats.chi2.ppf(alpha / 2, df=gl)
    chi_sup = stats.chi2.ppf(1 - (alpha / 2), df=gl)
    LS = (gl * var_muestral) / chi_inf
    LI = (gl * var_muestral) / chi_sup
    salida = pd.DataFrame({
        "Estadísticos": ["Varianza muestral", "Límite superior", "Límite inferior"],
        "Valores": [var_muestral, LS, LI]
    })
    return salida

# Uso de la función
resultado_var = intervalo_varianza(Mediciones, alpha=0.05)
print(resultado_var)
