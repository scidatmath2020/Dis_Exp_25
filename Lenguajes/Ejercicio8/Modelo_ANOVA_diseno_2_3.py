#==============================================================================#
# Modelo ANOVA (Diseño factorial 2^3) - Versión Python                         #
#==============================================================================#

# Borrar variables existentes
%reset -f

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats as stats
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm
from statsmodels.stats.multicomp import pairwise_tukeyhsd
from scipy.stats import jarque_bera, levene, bartlett, fligner, shapiro
import pingouin as pg
import warnings
warnings.filterwarnings('ignore')

# Cargar datos
Salarios = pd.read_csv("Bases de datos/Sal_Sector_Jornada_ENOET125.csv")

# Convertir a factores
Salarios['Sector'] = pd.Categorical(Salarios['Sector'], 
                                   categories=["Primario", "Secundario", "Terciario"])
Salarios['Jornada'] = pd.Categorical(Salarios['Jornada'], 
                                    categories=["Menos de 15", "De 15 a 48", "Más de 48"])

# Configurar tema gráfico
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

# Gráfico de cajas: Sector por Jornada
Etiquetas = Salarios.groupby(['Sector', 'Jornada'])['Salario_trim'].mean().round(2).values

plt.figure(figsize=(12,8))
ax = sns.boxplot(x='Sector', y='Salario_trim', hue='Jornada', data=Salarios,
                 palette={"Menos de 15": "#4CAF50", "De 15 a 48": "#FF8A65", "Más de 48": "#D98880"})

sns.pointplot(x='Sector', y='Salario_trim', hue='Jornada', data=Salarios,
              estimator=np.mean, dodge=0.5, markers='D', linestyles='',
              palette={"Menos de 15": "#4CAF50", "De 15 a 48": "#FF8A65", "Más de 48": "#D98880"},
              scale=0.8, legend=False)

for i, val in enumerate(Etiquetas):
    ax.text(i//3 + (-0.25 + 0.25*(i%3)), val+500, f"Media: {val}", ha='center', va='bottom')

plt.title("Salario mensual según sector, por duración de la jornada\npromedio mensual - T1 2025")
plt.ylabel("Salario promedio mensual")
plt.legend(title="Duración de jornada", bbox_to_anchor=(1,1))
plt.ylim(0, 15000)
plt.yticks(range(0, 15001, 1000))
plt.tight_layout()
plt.show()

# Gráfico de cajas: Jornada por Sector
Etiquetas2 = Salarios.groupby(['Jornada', 'Sector'])['Salario_trim'].mean().round(2).values

plt.figure(figsize=(12,8))
ax = sns.boxplot(x='Jornada', y='Salario_trim', hue='Sector', data=Salarios,
                 palette={"Primario": "#4CAF50", "Secundario": "#FF8A65", "Terciario": "#D98880"})

sns.pointplot(x='Jornada', y='Salario_trim', hue='Sector', data=Salarios,
              estimator=np.mean, dodge=0.5, markers='D', linestyles='',
              palette={"Primario": "#4CAF50", "Secundario": "#FF8A65", "Terciario": "#D98880"},
              scale=0.8, legend=False)

for i, val in enumerate(Etiquetas2):
    ax.text(i//3 + (-0.25 + 0.25*(i%3)), val+500, f"Media: {val}", ha='center', va='bottom')

plt.title("Salario mensual según jornada, por sector\npromedio mensual - T1 2025")
plt.ylabel("Salario promedio mensual")
plt.legend(title="Sector", bbox_to_anchor=(1,1))
plt.ylim(0, 15000)
plt.yticks(range(0, 15001, 1000))
plt.tight_layout()
plt.show()

# Validación de supuestos - Normalidad
print("\nNormalidad por Sector:")
print(Salarios.groupby('Sector')['Salario_trim'].apply(lambda x: jarque_bera(x)))

print("\nNormalidad por Jornada:")
print(Salarios.groupby('Jornada')['Salario_trim'].apply(lambda x: jarque_bera(x)))

print("\nNormalidad por Interacción:")
print(Salarios.groupby(['Sector', 'Jornada'])['Salario_trim'].apply(lambda x: jarque_bera(x)))

# Homocedasticidad
print("\nTest de Levene:")
print(levene(*[group['Salario_trim'] for name, group in Salarios.groupby(['Sector', 'Jornada'])]))

print("\nTest de Bartlett:")
print(bartlett(*[group['Salario_trim'] for name, group in Salarios.groupby(['Sector', 'Jornada'])]))

print("\nTest de Fligner:")
print(fligner(*[group['Salario_trim'] for name, group in Salarios.groupby(['Sector', 'Jornada'])]))

# Transformación raíz cuadrada
Salarios['sqrt_Salario'] = np.sqrt(Salarios['Salario_trim'])

# Validación con transformación
print("\nNormalidad con sqrt por Interacción:")
print(Salarios.groupby(['Sector', 'Jornada'])['sqrt_Salario'].apply(lambda x: jarque_bera(x)))

print("\nHomocedasticidad con sqrt:")
print(levene(*[group['sqrt_Salario'] for name, group in Salarios.groupby(['Sector', 'Jornada'])]))

# Modelo ANOVA
modelo = ols('sqrt_Salario ~ Sector * Jornada', data=Salarios).fit()
print("\nResultados ANOVA:")
print(anova_lm(modelo, typ=2))

# Gráficos de interacción
interaccion = Salarios.groupby(['Sector', 'Jornada'], as_index=False).agg(
    Media=('Salario_trim', 'mean'),
    SE=('Salario_trim', lambda x: np.std(x)/np.sqrt(len(x)))
)

plt.figure(figsize=(12,6))
sns.pointplot(x='Jornada', y='Media', hue='Sector', data=interaccion,
              palette={"Primario": "#4CAF50", "Secundario": "#FF8A65", "Terciario": "#D98880"},
              markers='o', linestyles='-', dodge=0.1)

for i, sector in enumerate(["Primario", "Secundario", "Terciario"]):
    sector_data = interaccion[interaccion['Sector'] == sector]
    plt.errorbar(x=np.arange(3)-0.1 + i*0.1, y=sector_data['Media'],
                 yerr=sector_data['SE'], fmt='none', 
                 c=["#4CAF50", "#FF8A65", "#D98880"][i], capsize=5)

plt.title("Interacción Sector y Jornada\nSalario promedio mensual - T1 2025")
plt.ylabel("Salario Trimestral Medio")
plt.xlabel("Tipo de Jornada")
plt.legend(title="Sector")
plt.tight_layout()
plt.show()

# Comparaciones post-hoc
groups = Salarios['Sector'].astype(str) + "_" + Salarios['Jornada'].astype(str)
mc = pairwise_tukeyhsd(Salarios['sqrt_Salario'], groups)
print("\nComparaciones post-hoc:")
print(mc)

# Verificación de residuos
residuos = modelo.resid

print("\nNormalidad de residuos:")
print("Shapiro:", shapiro(residuos))
print("Jarque-Bera:", jarque_bera(residuos))

# Gráfico Q-Q
plt.figure(figsize=(10,6))
stats.probplot(residuos, plot=plt)
plt.title("Gráfico Q-Q de residuales del modelo ANOVA")
plt.tight_layout()
plt.show()
