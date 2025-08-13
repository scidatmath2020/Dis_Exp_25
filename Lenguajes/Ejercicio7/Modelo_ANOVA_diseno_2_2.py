#==============================================================================#
# Modelo ANOVA (Diseño factorial 2^2) - Versión Python                         #
#==============================================================================#

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats as stats
import pingouin as pg
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm
from statsmodels.stats.multicomp import pairwise_tukeyhsd

# Lectura de datos -----
Salarios = pd.read_csv("Bases de datos/Sal_Sexo_Formal_ENOET125.csv")

# Conversión de columnas al tipo factor ------
Salarios['Sexo'] = pd.Categorical(Salarios['Sexo'], categories=["Hombre", "Mujer"])
Salarios['Empleo'] = pd.Categorical(Salarios['Empleo'], categories=["Formal", "Informal"])

# Tema personalizado para gráficos -----
plt.style.use('seaborn')
sns.set_palette("pastel")
plt.rcParams.update({
    'figure.facecolor': '#E3F2FD',
    'axes.facecolor': '#E3F2FD',
    'axes.grid': True,
    'grid.color': 'grey',
    'axes.titlelocation': 'center',
    'axes.titlesize': 16,
    'axes.titlecolor': 'blue',
    'axes.labelsize': 11,
    'xtick.color': 'grey',
    'ytick.color': 'grey',
    'legend.facecolor': '#E3F2FD',
    'legend.frameon': False,
    'legend.fontsize': 11
})

# Gráfico de cajas de los factores sexo por tipo de empleo -----
Etiquetas = Salarios.groupby(['Sexo', 'Empleo'])['Salario_trim'].mean().round(2).values

plt.figure(figsize=(10,6))
ax = sns.boxplot(x='Sexo', y='Salario_trim', hue='Empleo', data=Salarios, 
                 palette={"Formal": "#4CAF50", "Informal": "#FF8A65"})

# Añadir medias
sns.pointplot(x='Sexo', y='Salario_trim', hue='Empleo', data=Salarios,
              estimator=np.mean, dodge=0.5, markers='D', linestyles='',
              palette={"Formal": "#4CAF50", "Informal": "#FF8A65"},
              scale=0.8, legend=False)

# Añadir etiquetas de medias
for i, val in enumerate(Etiquetas):
    ax.text(i//2 + (-0.2 if i%2==0 else 0.2), val+500, f"Media: {val}", 
            ha='center', va='bottom')

plt.title("Salario mensual según sexo, por tipo de empleo\npromedio mensual - T1 2025")
plt.ylabel("Salario promedio mensual")
plt.legend(title="Tipo de empleo", bbox_to_anchor=(1,1))
plt.ylim(0, 15000)
plt.yticks(range(0, 15001, 1000))
plt.tight_layout()
plt.show()

# Gráfico de cajas de los factores tipo de empleo por sexo -----
Etiquetas2 = Salarios.groupby(['Empleo', 'Sexo'])['Salario_trim'].mean().round(2).values

plt.figure(figsize=(10,6))
ax = sns.boxplot(x='Empleo', y='Salario_trim', hue='Sexo', data=Salarios,
                 palette={"Hombre": "#4CAF50", "Mujer": "#FF8A65"})

# Añadir medias
sns.pointplot(x='Empleo', y='Salario_trim', hue='Sexo', data=Salarios,
              estimator=np.mean, dodge=0.5, markers='D', linestyles='',
              palette={"Hombre": "#4CAF50", "Mujer": "#FF8A65"},
              scale=0.8, legend=False)

# Añadir etiquetas de medias
for i, val in enumerate(Etiquetas2):
    ax.text(i//2 + (-0.2 if i%2==0 else 0.2), val+500, f"Media: {val}", 
            ha='center', va='bottom')

plt.title("Salario mensual según tipo de empleo, por sexo\npromedio mensual - T1 2025")
plt.ylabel("Salario promedio mensual")
plt.legend(title="Sexo", bbox_to_anchor=(1,1))
plt.ylim(0, 15000)
plt.yticks(range(0, 15001, 1000))
plt.tight_layout()
plt.show()

# Validación de supuestos ----

## Normalidad ----
### Para sexo ----
print("\nNormalidad por sexo:")
print(Salarios.groupby('Sexo')['Salario_trim'].apply(lambda x: stats.jarque_bera(x)))

### Para empleo ----
print("\nNormalidad por empleo:")
print(Salarios.groupby('Empleo')['Salario_trim'].apply(lambda x: stats.jarque_bera(x)))

### Interacción sexo y empleo ----
print("\nNormalidad por interacción:")
print(Salarios.groupby(['Sexo', 'Empleo'])['Salario_trim'].apply(lambda x: stats.jarque_bera(x)))

## Igualdad en varianzas -----
### Levene ----
print("\nTest de Levene:")
print(stats.levene(*[group['Salario_trim'] for name, group in Salarios.groupby(['Sexo', 'Empleo'])]))

### Bartlett ----
print("\nTest de Bartlett:")
print(stats.bartlett(*[group['Salario_trim'] for name, group in Salarios.groupby(['Sexo', 'Empleo'])]))

### Fligner-Killeen ----
print("\nTest de Fligner:")
print(stats.fligner(*[group['Salario_trim'] for name, group in Salarios.groupby(['Sexo', 'Empleo'])]))

# Validación con log-transform ----
Salarios['log_Salario'] = np.log(Salarios['Salario_trim'])

## Normalidad con log ----
print("\nNormalidad con log-transform:")
print(Salarios.groupby(['Sexo', 'Empleo'])['log_Salario'].apply(lambda x: stats.jarque_bera(x)))

## Homocedasticidad con log ----
print("\nHomocedasticidad con log-transform:")
print(stats.levene(*[group['log_Salario'] for name, group in Salarios.groupby(['Sexo', 'Empleo'])]))

# Modelo ANOVA -----
modelo = ols('log_Salario ~ Sexo * Empleo', data=Salarios).fit()
print("\nResultados ANOVA:")
print(anova_lm(modelo, typ=2))

# Gráfico de interacción ----
interaccion = Salarios.groupby(['Sexo', 'Empleo'], as_index=False).agg(
    Media=('Salario_trim', 'mean'),
    SE=('Salario_trim', lambda x: np.std(x)/np.sqrt(len(x)))
)

plt.figure(figsize=(10,6))
sns.pointplot(x='Empleo', y='Media', hue='Sexo', data=interaccion,
              palette={"Hombre": "#4CAF50", "Mujer": "#FF8A65"},
              markers='o', linestyles='-', dodge=0.1)

plt.errorbar(x=np.arange(2)-0.05, 
             y=interaccion[interaccion['Sexo']=='Hombre']['Media'],
             yerr=interaccion[interaccion['Sexo']=='Hombre']['SE'],
             fmt='none', c='#4CAF50', capsize=5)

plt.errorbar(x=np.arange(2)+0.05, 
             y=interaccion[interaccion['Sexo']=='Mujer']['Media'],
             yerr=interaccion[interaccion['Sexo']=='Mujer']['SE'],
             fmt='none', c='#FF8A65', capsize=5)

plt.title("Interacción del factor sexo y empleo\nSalario promedio mensual - T1 2025")
plt.ylabel("Salario Trimestral Medio")
plt.xlabel("Tipo de empleo")
plt.legend(title="Sexo")
plt.tight_layout()
plt.show()

# Comparación de medias ----
print("\nComparaciones post-hoc:")
mc = pairwise_tukeyhsd(Salarios['log_Salario'], 
                       Salarios['Sexo'].astype(str) + "_" + Salarios['Empleo'].astype(str))
print(mc)

# Verificación de supuestos del modelo ----
residuos = modelo.resid

## Test de normalidad ----
print("\nNormalidad de residuos:")
print("Shapiro:", stats.shapiro(residuos))
print("Jarque-Bera:", stats.jarque_bera(residuos))

## Gráfico Q-Q ----
plt.figure(figsize=(10,6))
stats.probplot(residuos, plot=plt)
plt.title("Gráfico Q-Q de residuales del modelo ANOVA")
plt.tight_layout()
plt.show()
