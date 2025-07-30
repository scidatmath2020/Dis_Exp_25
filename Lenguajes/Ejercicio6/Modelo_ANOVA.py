#===========================================================================#
# Modelo ANOVA                                                              #
# Autor: Alexis Adonai Morales Alberto                                      #
# SCIDATA                                                                   # 
# Diplomado: Diseño de experimentos                                         #
# Código de Python para clase                                               #
#===========================================================================#

# Carga de modulos 

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pingouin as pg
import statsmodels.api as sm

# Carga del submodulo stats de numpy 

from scipy import stats

# Carga de clases directas 

from scipy.stats import boxcox, levene, jarque_bera
from statsmodels.stats.multicomp import pairwise_tukeyhsd
import scikit_posthocs as sp
from statsmodels.formula.api import ols
from statsmodels.stats.multicomp import MultiComparison

# Importación de datos 

Salarios = pd.read_csv("Bases de datos/Salarios_educacion_ENOET125.csv")

## Ejemplo (en caso de que venga en formato ancho) 

# Salarios = Salarios.melt(id_vars = ["ent"], var_name = "Educación", value_name = "Salario")

# Visualización de las medias 

plt.figure(figsize = (10,5), dpi = 500)
sns.boxplot(data = Salarios, x = "Escolaridad", 
            y = "Salario_trim", palette = "Set2")
sns.pointplot(data = Salarios, x = "Escolaridad", 
              y = "Salario_trim", color = "black", 
              markers = "D", join = False, label = "Media") 
plt.title("Niveles de los salarios trimestrales\nde la PEA según nivel de escolaridad\nen pesos\nprimer trimestre del 2025")
plt.xlabel("Nivel educativo")
plt.ylabel("Salario trimestral")
plt.tight_layout()
plt.show()

# Diagnóstico de los supuestos 

## Normalidad (usando Jarque-Bera)

norm_results = []
for t in Salarios['Escolaridad'].unique():
  group = Salarios[Salarios['Escolaridad'] == t]['Salario_trim']
  stat, p, *_ = jarque_bera(group)
  conclusion = "Normalidad" if p > 0.05 else "No normalidad"
  norm_results.append({'Escolaridad': t, 'p_value': p, 'Conclusión': conclusion})

norm_df = pd.DataFrame(norm_results)
print(norm_df)

## Igualdad en varianzas 

levene_post_stat, levene_post_p = levene(*[Salarios[Salarios['Escolaridad'] == t]['Salario_trim'] for t in Salarios['Escolaridad'].unique()])
print(f" Estadístico: {levene_post_stat}, p-value:{levene_post_p}")

# Modelo ANOVA 

Salarios['Escolaridad'] = Salarios['Escolaridad'].astype('category')
modelo = sm.formula.ols("Salario_trim ~ C(Escolaridad)", data = Salarios).fit()
anova_tabla = sm.stats.anova_lm(modelo, typ=2)
print(anova_tabla)

stats.f.ppf(1-0.05, 5, 184) # Si quisieras hacer constrastes con el valor de F, usa esta función

## Pruebas post hoc

### Tukey 

tukey = pairwise_tukeyhsd(endog = Salarios['Salario_trim'], groups = Salarios['Escolaridad'], alpha = 0.05)
print(tukey.summary())


## Duncan 

duncan = pg.pairwise_tukey(data = Salarios, dv = 'Salario_trim', between = 'Escolaridad')
print(duncan)

# Tabla de medias por grupos

## Función para grupos de letras 

def obtener_grupos_letras(tukey_result):
    from collections import defaultdict

    res_df = pd.DataFrame(data=tukey_result._results_table.data[1:], columns=tukey_result._results_table.data[0])
    grupos = np.unique(np.concatenate([res_df['group1'], res_df['group2']]))
    grupos.sort()
    letras = defaultdict(set)
    
    letra_actual = 'A'
    for i, g1 in enumerate(grupos):
        letras[g1].add(letra_actual)
        for g2 in grupos[i+1:]:
            pval = res_df[((res_df['group1'] == g1) & (res_df['group2'] == g2)) |
                          ((res_df['group1'] == g2) & (res_df['group2'] == g1))]['p-adj'].values
            if len(pval) > 0 and pval[0] >= 0.05:  # No significativa → mismo grupo
                letras[g2].add(letra_actual)
        letra_actual = chr(ord(letra_actual)+1)
    return {k: ''.join(sorted(v)) for k, v in letras.items()}


## Se replica Tukey

mc = MultiComparison(Salarios["Salario_trim"], Salarios["Escolaridad"])
tukey_result = mc.tukeyhsd()
print(tukey_result.summary())

## Letras de grupos

letras = obtener_grupos_letras(tukey_result)

## Gráfico

plt.figure(figsize=(10, 6), dpi = 500)
sns.barplot(data=Salarios, x='Escolaridad', y='Salario_trim', palette='Set2',
            errorbar = None)
media_grupo = Salarios.groupby('Escolaridad')['Salario_trim'].mean().reset_index()
for i, row in media_grupo.iterrows():
    grupo = row['Escolaridad']
    media = row['Salario_trim']
    letra = letras[grupo]
    plt.text(i, media + 100, letra, ha='center', va='bottom', fontsize=12, fontweight='bold')

plt.title('Salario trimestral promedio por escolaridad')
plt.xlabel('Escolaridad')
plt.ylabel('Salario trimestral')
plt.tight_layout()
plt.show()

# Welch's ANOVA 

welch = pg.welch_anova(dv = 'Salario_trim', between = 'Escolaridad', data = Salarios)
print(welch)

## Games_Howell 

gh = pg.pairwise_gameshowell(dv = 'Salario_trim', between = 'Escolaridad', data = Salarios)
print(gh)


