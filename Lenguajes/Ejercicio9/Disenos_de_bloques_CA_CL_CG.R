#==============================================================================#
# Dieños de bloques (Completos aleatorizados, cuadrado latino y grecolatino)   #
# Alexis Adonai Morales Alberto                                                #
# SciData                                                                      #
# Diplomado: Diseño de experimentos                                            #
# Código de R                                                                  #
#==============================================================================#

# Borrar la memoria del  programa -----

rm(list = ls())

# Verificar la existencia de Pacman en nuestra versión de R ----

if(require("pacman", quietly = T)){
  cat("El paquete de pacman se encuentra instalado en la versión de R")
} else{
  install.packages("pacman", dependencies = T)
}

# Cargar de paquetes ----

pacman::p_load(
  "tidyverse", 
  "ggpubr", 
  "agricolae",
  "car", 
  "broom",
  "DescTools"
)

# Diseño de bloques completos aleatorizados -----


DBCA <- read.csv("Bases de datos/DBCA.csv")

DBCA$Ejemplar <- factor(DBCA$Ejemplar)
DBCA$Punta <- factor(DBCA$Punta)

## Gráficos de análisis prelimnar ----

### Tema ----

Tema <- theme(plot.title = element_text(size = 16,
                                        hjust = 0.5,
                                        face = "bold",
                                        color = "blue4"),
              plot.subtitle = element_text(size = 14,
                                           hjust = 0.5,
                                           color = "blue4"),
              plot.caption = element_text(size = 12,
                                          hjust = 0,
                                          color = "blue4"),
              axis.text = element_text(size = 11,
                                       color = "grey25"),
              axis.title = element_text(size = 11,
                                        color = "grey25"),
              axis.ticks = element_blank(),
              legend.text = element_text(size = 11,
                                         color = "grey25"),
              legend.title = element_text(size = 11,
                                          color = "grey25"),
              legend.position = "bottom",
              panel.background = element_rect(fill = "#E3F2FD"),
              plot.background = element_rect(fill = "#E3F2FD"),
              legend.background = element_rect(fill = "#E3F2FD"),
              panel.grid.major = element_line(color = "grey",
                                              lineend = "butt"),
              panel.grid.minor = element_blank())


### Boxplot de la variable explicativa y factor -----

DBCA %>% 
  ggplot(aes(x = Punta, y = Dureza))+
  geom_boxplot(fill = "#FF8A65")+
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean,
               geom = "point",
               shape = 23,
               size = 4,
               position = position_dodge(width = 0.75))+
  labs(title = "Dureza del material según el tipo de punta",
       subtitle = "promedio de la dureza",
       caption = "",
       x = "Punta",
       y = "Dureza",
       fill = "")+
  Tema

### Boxplot de la variable explicativa y bloque ----


DBCA %>% 
  ggplot(aes(x = Ejemplar, y = Dureza))+
  geom_boxplot(fill = "#FF8A65")+
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean,
               geom = "point",
               shape = 23,
               size = 4,
               position = position_dodge(width = 0.75))+
  labs(title = "Dureza del material según el ejemplar",
       subtitle = "promedio de la dureza",
       caption = "",
       x = "Ejemplar",
       y = "Dureza",
       fill = "")+
  Tema

### Puntos conectados por bloque ----

DBCA %>% 
  ggplot(aes(x = Punta, y = Dureza, group = Ejemplar))+
  geom_point(aes(color = Ejemplar),
             size = 3) +
  geom_line(aes(color = Ejemplar),
            linewidth = 1)+
  scale_color_manual(values = c(
    "#497D15",
    "#C81CDE",
    "#C71D36",
    "#E1712B"
  ))+
  labs(title = "Tendencia de dureza por punta en cada ejemplar",
       x = "Punta",
       y = "Dureza",
       fill = "Ejemplar")+
  Tema

## Validación de supuestos (a priori al modelo) ----

### Normalidad ----

DBCA %>% 
  group_by(Punta) %>% 
  summarise(P_value = JarqueBeraTest(Dureza)$p.value)

DBCA %>% 
  group_by(Ejemplar) %>% 
  summarise(P_value = JarqueBeraTest(Dureza)$p.value)

### Igualdad en varianzas ----

LeveneTest(Dureza ~ Punta, data = DBCA)

LeveneTest(Dureza ~ Ejemplar, data = DBCA)

## Modelo ANOVA ----

ANOVA <- aov(Dureza ~ Punta + Ejemplar, data = DBCA)
summary(ANOVA)

## Estimación y residuales -----

Estimados <- predict(ANOVA)

Residuales <- resid(ANOVA)

### Diagnostico de los residuales ----

data.frame(Residuales) %>% 
  ggplot(aes(sample = Residuales))+
  geom_qq(color = "#FB8C00",
          size = 3)+
  geom_qq_line(color = "#80CBC4",
               linewidth = 1.1)+
  labs(title = "Gráfico cuantil-cuantil para detectar distribución teórica",
       subtitle = "residuales del modelo ANOVA",
       y = "Residual",
       x = "Cuantiles teóricos")+
  Tema

DBCA %>% 
  mutate(Residuales = Residuales) %>% 
  ggplot(aes(x = Punta, y = Residuales))+
  geom_point(color = "red4", size = 3)+
  geom_hline(yintercept = 0, linewidth = 1.1, color = "green4")+
  labs(title = "Tendencia de los residuales según el valor de la punta",
       x = "Punta",
       y = "Residual")+
  Tema

DBCA %>% 
  mutate(Residuales = Residuales) %>% 
  ggplot(aes(x = Ejemplar, y = Residuales))+
  geom_point(color = "red4", size = 3)+
  geom_hline(yintercept = 0, linewidth = 1.1, color = "green4")+
  labs(title = "Tendencia de los residuales según el valor de la punta",
       x = "Ejemplar",
       y = "Residual")+
  Tema


ggplot(data.frame(Estimados, Residuales),
       aes(x = Estimados, y = Residuales))+
  geom_point(color = "red4", size = 3)+
  geom_hline(yintercept = 0, linewidth = 1.1, color = "green4")+
  labs(title = "Tendencia de los residuales según el valor estimado",
       x = "Estimado de Dureza",
       y = "Residual")+
  Tema


### Normalidad en residuales ----

JarqueBeraTest(Residuales)

### Comparaciones multiples -----

#### Tukey ----

tukey <- TukeyHSD(ANOVA)
tukey

data.frame(tukey$Punta) %>% 
  rownames_to_column(., "Comparación") %>% 
  ggplot(aes(y = Comparación, x = diff))+
  geom_point(size =3) +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 2) +
  labs(title = "Comparaciones múltiples (Tukey HSD)",
       x = "Diferencia de medias", y = "Comparación con niveles de puntas")+
  Tema


data.frame(tukey$Ejemplar) %>% 
  rownames_to_column(., "Comparación") %>% 
  ggplot(aes(y = Comparación, x = diff))+
  geom_point(size =3) +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 2) +
  labs(title = "Comparaciones múltiples (Tukey HSD)",
       x = "Diferencia de medias", y = "Comparación con bloques de ejemplar")+
  Tema


#### LSD con ajuste en p-values por Bonferroni (considerando el bloque) -----

cme <- deviance(ANOVA)/df.residual(ANOVA)

Fisher <- LSD.test(DBCA$Dureza, DBCA$Punta, df.residual(ANOVA), cme,
                   p.adj = "bonferroni",
                   group = T,
                   main = "Dureza")

Fisher

data.frame(Fisher$groups) %>% 
  rownames_to_column(., var = "Punta") %>% 
  left_join(data.frame(Fisher$means) %>% 
              select(`DBCA.Dureza`,
                     std),
            by = c("DBCA.Dureza")) %>% 
  select(all_of(c(
    "Punta",
    "groups",
    "DBCA.Dureza",
    "std"
  ))) %>% 
  set_names(nm = c("Punta",
                   "Grupos",
                   "Media",
                   "se")) %>% 
  ggplot(aes(x = reorder(Punta, -Media), y = Media, color = Grupos)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Media - se, ymax = Media + se), width = 0.2) +
  geom_text(aes(label = Grupos), vjust = -1, size = 5) +
  scale_color_manual(values = c(
    "#497D15",
    "#E1712B"
  ))+
  labs(title = "Medias de Dureza con Letras (LSD Bonferroni)",
       x = "Punta", y = "Media de Dureza")+
  Tema+
  theme(legend.position = "none")


# Diseño en cuadrado latino ----

rm(list=ls())

DCL <- read.csv("Bases de datos/DCL.csv")

## Gráficos preliminares ----

Tema <- theme(plot.title = element_text(size = 16,
                                        hjust = 0.5,
                                        face = "bold",
                                        color = "blue4"),
              plot.subtitle = element_text(size = 14,
                                           hjust = 0.5,
                                           color = "blue4"),
              plot.caption = element_text(size = 12,
                                          hjust = 0,
                                          color = "blue4"),
              axis.text = element_text(size = 11,
                                       color = "grey25"),
              axis.title = element_text(size = 11,
                                        color = "grey25"),
              axis.ticks = element_blank(),
              legend.text = element_text(size = 11,
                                         color = "grey25"),
              legend.title = element_text(size = 11,
                                          color = "grey25"),
              legend.position = "bottom",
              panel.background = element_rect(fill = "#E3F2FD"),
              plot.background = element_rect(fill = "#E3F2FD"),
              legend.background = element_rect(fill = "#E3F2FD"),
              panel.grid.major = element_line(color = "grey",
                                              lineend = "butt"),
              panel.grid.minor = element_blank())


### Boxplot de Carga por Tratamiento (Formula) ----

DCL$Formula <- factor(DCL$Formula)

DCL$Operador <- factor(DCL$Operador)

DCL$Lote <- factor(DCL$Lote)


DCL %>%
  ggplot(aes(x = Formula, y = Carga)) +
  geom_boxplot(fill = "orange2") +
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean,
               geom = "point",
               shape = 23,
               size = 4,
               position = position_dodge(width = 0.75))+
  labs(title = "Distribución de Carga por Fórmula",
       x = "Fórmula", y = "Carga")+
  Tema

### Boxplot por bloque de filas (operador) ----

DCL %>%
  ggplot(aes(x = Operador, y = Carga)) +
  geom_boxplot(fill = "lightgreen") +
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean,
               geom = "point",
               shape = 23,
               size = 4,
               position = position_dodge(width = 0.75))+
  labs(title = "Carga por Operador (Bloque Fila)",
       x = "Operador", y = "Carga")+
  Tema

### Boxplot pot bloque de columnas (Lote) ----

DCL %>%
  ggplot(aes(x = Lote, y = Carga)) +
  geom_boxplot(fill = "violet") +
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean,
               geom = "point",
               shape = 23,
               size = 4,
               position = position_dodge(width = 0.75))+
  labs(title = "Carga por Lote (Bloque Columna)",
       x = "Lote", y = "Carga")+
  Tema

### Hetmap del diseño experimental -----

DCL %>%
  ggplot(aes(x = Lote, y = Operador, fill = Formula)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Formula), size = 5) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Distribución de Fórmulas en el Cuadrado Latino")+
  Tema

### Gráfico de puntos  por bloques -----

DCL %>%
  ggplot(aes(x = Formula, y = Carga, group = Operador, color = Operador)) +
  geom_point() +
  geom_line()+
  labs(title = "Carga por Fórmula en cada combinación de operador")+
  Tema

DCL %>%
  ggplot(aes(x = Formula, y = Carga, group = Lote, color = Lote)) +
  geom_point() +
  geom_line()+
  labs(title = "Carga por Fórmula en cada combinación de lote")+
  Tema


## Modelo ANOVA ----

modelo_cl <- aov(Carga ~ Formula + Operador + Lote, data = DCL)
summary(modelo_cl)

## Estimación y residuales -----

Estimados <- predict(modelo_cl)

Residuales <- resid(modelo_cl)

### Diagnostico de los residuales ----

data.frame(Residuales) %>% 
  ggplot(aes(sample = Residuales))+
  geom_qq(color = "#FB8C00",
          size = 3)+
  geom_qq_line(color = "#80CBC4",
               linewidth = 1.1)+
  labs(title = "Gráfico cuantil-cuantil para detectar distribución teórica",
       subtitle = "residuales del modelo modelo_cl",
       y = "Residual",
       x = "Cuantiles teóricos")+
  Tema

DCL %>% 
  mutate(Residuales = Residuales) %>% 
  ggplot(aes(x = Formula, y = Residuales))+
  geom_point(color = "red4", size = 3)+
  geom_hline(yintercept = 0, linewidth = 1.1, color = "green4")+
  labs(title = "Tendencia de los residuales según formula",
       x = "Punta",
       y = "Residual")+
  Tema

DCL %>% 
  mutate(Residuales = Residuales) %>% 
  ggplot(aes(x = Operador, y = Residuales))+
  geom_point(color = "red4", size = 3)+
  geom_hline(yintercept = 0, linewidth = 1.1, color = "green4")+
  labs(title = "Tendencia de los residuales según el operador",
       x = "Ejemplar",
       y = "Residual")+
  Tema

DCL %>% 
  mutate(Residuales = Residuales) %>% 
  ggplot(aes(x = Lote, y = Residuales))+
  geom_point(color = "red4", size = 3)+
  geom_hline(yintercept = 0, linewidth = 1.1, color = "green4")+
  labs(title = "Tendencia de los residuales según lote",
       x = "Ejemplar",
       y = "Residual")+
  Tema


ggplot(data.frame(Estimados, Residuales),
       aes(x = Estimados, y = Residuales))+
  geom_point(color = "red4", size = 3)+
  geom_hline(yintercept = 0, linewidth = 1.1, color = "green4")+
  labs(title = "Tendencia de los residuales según el valor estimado",
       x = "Carga",
       y = "Residual")+
  Tema


### Normalidad en residuales ----

JarqueBeraTest(Residuales)


### Igualdas de varianzas ----

leveneTest(DCL$Carga, DCL$Formula)

### Comparaciones multiples -----

#### Tukey ----

tukey <- TukeyHSD(modelo_cl)
tukey

data.frame(tukey$Formula) %>% 
  rownames_to_column(., "Comparación") %>% 
  ggplot(aes(y = Comparación, x = diff))+
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Comparaciones múltiples (Tukey HSD)",
       x = "Diferencia de medias", y = "Comparación con niveles de formula")+
  Tema


data.frame(tukey$Operador) %>% 
  rownames_to_column(., "Comparación") %>% 
  ggplot(aes(y = Comparación, x = diff))+
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Comparaciones múltiples (Tukey HSD)",
       x = "Diferencia de medias", y = "Comparación con bloques de operador")+
  Tema

data.frame(tukey$Lote) %>% 
  rownames_to_column(., "Comparación") %>% 
  ggplot(aes(y = Comparación, x = diff))+
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Comparaciones múltiples (Tukey HSD)",
       x = "Diferencia de medias", y = "Comparación con bloques de lote")+
  Tema


#### LSD con ajuste en p-values por Bonferroni (considerando el bloque) -----

cme <- deviance(modelo_cl)/df.residual(modelo_cl)

Fisher <- LSD.test(DCL$Carga, DCL$Formula, df.residual(modelo_cl), cme,
                   p.adj = "bonferroni",
                   group = T,
                   main = "Dureza")

Fisher

data.frame(Fisher$groups) %>% 
  rownames_to_column(., var = "Formula") %>% 
  left_join(data.frame(Fisher$means) %>% 
              select(`DCL.Carga`,
                     std),
            by = c("DCL.Carga")) %>% 
  select(all_of(c(
    "Formula",
    "groups",
    "DCL.Carga",
    "std"
  ))) %>% 
  set_names(nm = c("Formula",
                   "Grupos",
                   "Media",
                   "se")) %>% 
  ggplot(aes(x = reorder(Formula, -Media), y = Media, color = Grupos)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Media - se, ymax = Media + se), width = 0.2) +
  geom_text(aes(label = Grupos), vjust = -1, size = 5) +
  labs(title = "Medias de Carga con Letras (LSD Bonferroni)",
       x = "Formula", y = "Media de Carga")+
  Tema+
  theme(legend.position = "none")


#### Duncan con ajuste en p-values (considerando el bloque) -----

cme <- deviance(modelo_cl)/df.residual(modelo_cl)

Duncan <- duncan.test(DCL$Carga, DCL$Formula, df.residual(modelo_cl), cme,
                      group = T)


data.frame(Duncan$groups) %>% 
  rownames_to_column(., var = "Formula") %>% 
  left_join(data.frame(Duncan$means) %>% 
              select(`DCL.Carga`,
                     std),
            by = c("DCL.Carga")) %>% 
  select(all_of(c(
    "Formula",
    "groups",
    "DCL.Carga",
    "std"
  ))) %>% 
  set_names(nm = c("Formula",
                   "Grupos",
                   "Media",
                   "se")) %>% 
  ggplot(aes(x = reorder(Formula, -Media), y = Media, color = Grupos)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Media - se, ymax = Media + se), width = 0.2) +
  geom_text(aes(label = Grupos), vjust = -1, size = 5) +
  labs(title = "Medias de Carga con Letras (Duncan)",
       x = "Punta", y = "Media de Carga")+
  Tema+
  theme(legend.position = "none")


# Diseño cuadrado grecolatino ----

DCG <- read.csv("Bases de datos/DCG.csv")

DCG$Montaje[16] <- "M4"

DCG$Montaje <- factor(DCG$Montaje)
DCG$Operador <- factor(DCG$Operador)
DCG$Lote <- factor(DCG$Lote)
DCG$Forma <- factor(DCG$Forma)

## Gráficos preliminares ----

Tema <- theme(plot.title = element_text(size = 16,
                                        hjust = 0.5,
                                        face = "bold",
                                        color = "blue4"),
              plot.subtitle = element_text(size = 14,
                                           hjust = 0.5,
                                           color = "blue4"),
              plot.caption = element_text(size = 12,
                                          hjust = 0,
                                          color = "blue4"),
              axis.text = element_text(size = 11,
                                       color = "grey25"),
              axis.title = element_text(size = 11,
                                        color = "grey25"),
              axis.ticks = element_blank(),
              legend.text = element_text(size = 11,
                                         color = "grey25"),
              legend.title = element_text(size = 11,
                                          color = "grey25"),
              legend.position = "bottom",
              panel.background = element_rect(fill = "#E3F2FD"),
              plot.background = element_rect(fill = "#E3F2FD"),
              legend.background = element_rect(fill = "#E3F2FD"),
              panel.grid.major = element_line(color = "grey",
                                              lineend = "butt"),
              panel.grid.minor = element_blank())

### Boxplot de Car por tratamiento Forma ----

DCG %>%
  ggplot(aes(x = Forma, y = Car)) +
  geom_boxplot(fill = "skyblue") +
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean,
               geom = "point",
               shape = 23,
               size = 4,
               position = position_dodge(width = 0.75))+
  labs(title = "Distribución de Car por Tratamiento (Forma)",
       x = "Forma", y = "Car")+
  Tema

### Boxplot por bloque de filas montaje ----

DCG %>%
  ggplot(aes(x = Montaje, y = Car)) +
  geom_boxplot(fill = "lightgreen") +
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean,
               geom = "point",
               shape = 23,
               size = 4,
               position = position_dodge(width = 0.75))+
  labs(title = "Distribución de Car por Montaje (Bloque Fila)",
       x = "Montaje", y = "Car")+
  Tema

### Boxplot por bloque de columnas Lote ----


DCG %>%
  ggplot(aes(x = Lote, y = Car)) +
  geom_boxplot(fill = "orange") +
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean,
               geom = "point",
               shape = 23,
               size = 4,
               position = position_dodge(width = 0.75))+
  labs(title = "Distribución de Car por Lote (Bloque Columna)",
       x = "Lote", y = "Car")+
  Tema


### Boxplot por bloque de columnas Operador ----


DCG %>%
  ggplot(aes(x = Operador, y = Car)) +
  geom_boxplot(fill = "yellow4") +
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean,
               geom = "point",
               shape = 23,
               size = 4,
               position = position_dodge(width = 0.75))+
  labs(title = "Distribución de Car por Operador (Bloque Letra)",
       x = "Operador", y = "Car")+
  Tema

### Heatmap de tratamientos de forma -----

DCG %>%
  ggplot(aes(x = Lote, y = Montaje, fill = Forma)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Forma), size = 5) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Distribución de Tratamientos (Forma) en el Diseño Grecolatino")+
  Tema

DCG %>%
  ggplot(aes(x = Lote, y = Operador, fill = Forma)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Forma), size = 5) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Distribución de Tratamientos (Forma) en el Diseño Grecolatino")+
  Tema

DCG %>%
  ggplot(aes(x = Montaje, y = Operador, fill = Forma)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Forma), size = 5) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Distribución de Tratamientos (Forma) en el Diseño Grecolatino")+
  Tema

### Gráficos de puntos con facetas por Montaje y Lote ----

DCG %>%
  ggplot(aes(x = Forma, y = Car)) +
  geom_point(size = 3) +
  facet_grid(Montaje ~ Lote) +
  labs(title = "Carga por Forma en cada combinación Montaje-Lote")+
  Tema


## Estimación del modelo ANOVA ----

modelo_gl <- aov(Car ~ Forma + Montaje + Lote + Operador, data = DCG)
summary(modelo_gl)

## Estimación y residuales -----

Estimados <- predict(modelo_gl)

Residuales <- resid(modelo_gl)

### Diagnostico de los residuales ----

data.frame(Residuales) %>% 
  ggplot(aes(sample = Residuales))+
  geom_qq(color = "#FB8C00",
          size = 3)+
  geom_qq_line(color = "#80CBC4",
               linewidth = 1.1)+
  labs(title = "Gráfico cuantil-cuantil para detectar distribución teórica",
       subtitle = "residuales del modelo modelo_gl",
       y = "Residual",
       x = "Cuantiles teóricos")+
  Tema

DCG %>% 
  mutate(Residuales = Residuales) %>% 
  ggplot(aes(x = Forma, y = Residuales))+
  geom_point(color = "red4", size = 3)+
  geom_hline(yintercept = 0, linewidth = 1.1, color = "green4")+
  labs(title = "Tendencia de los residuales según forma",
       x = "Forma",
       y = "Residual")+
  Tema

DCG %>% 
  mutate(Residuales = Residuales) %>% 
  ggplot(aes(x = Montaje, y = Residuales))+
  geom_point(color = "red4", size = 3)+
  geom_hline(yintercept = 0, linewidth = 1.1, color = "green4")+
  labs(title = "Tendencia de los residuales según el montaje",
       x = "Montaje",
       y = "Residual")+
  Tema

DCG %>% 
  mutate(Residuales = Residuales) %>% 
  ggplot(aes(x = Lote, y = Residuales))+
  geom_point(color = "red4", size = 3)+
  geom_hline(yintercept = 0, linewidth = 1.1, color = "green4")+
  labs(title = "Tendencia de los residuales según lote",
       x = "Lote",
       y = "Residual")+
  Tema

DCG %>% 
  mutate(Residuales = Residuales) %>% 
  ggplot(aes(x = Operador, y = Residuales))+
  geom_point(color = "red4", size = 3)+
  geom_hline(yintercept = 0, linewidth = 1.1, color = "green4")+
  labs(title = "Tendencia de los residuales según operador",
       x = "Operador",
       y = "Residual")+
  Tema


ggplot(data.frame(Estimados, Residuales),
       aes(x = Estimados, y = Residuales))+
  geom_point(color = "red4", size = 3)+
  geom_hline(yintercept = 0, linewidth = 1.1, color = "green4")+
  labs(title = "Tendencia de los residuales según el valor estimado",
       x = "Carga",
       y = "Residual")+
  Tema


### Normalidad en residuales ----

JarqueBeraTest(Residuales)


### Igualdas de varianzas ----

leveneTest(DCG$Car, DCG$Forma)

### Comparaciones multiples -----

#### Tukey ----

tukey <- TukeyHSD(modelo_gl)
tukey

data.frame(tukey$Forma) %>% 
  rownames_to_column(., "Comparación") %>% 
  ggplot(aes(y = Comparación, x = diff))+
  geom_point() +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Comparaciones múltiples (Tukey HSD)",
       x = "Diferencia de medias", y = "Comparación con niveles de forma")+
  Tema


data.frame(tukey$Montaje) %>% 
  rownames_to_column(., "Comparación") %>% 
  ggplot(aes(y = Comparación, x = diff))+
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Comparaciones múltiples (Tukey HSD)",
       x = "Diferencia de medias", y = "Comparación con bloques de montaje")+
  Tema

data.frame(tukey$Lote) %>% 
  rownames_to_column(., "Comparación") %>% 
  ggplot(aes(y = Comparación, x = diff))+
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Comparaciones múltiples (Tukey HSD)",
       x = "Diferencia de medias", y = "Comparación con bloques de lote")+
  Tema


data.frame(tukey$Operador) %>% 
  rownames_to_column(., "Comparación") %>% 
  ggplot(aes(y = Comparación, x = diff))+
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Comparaciones múltiples (Tukey HSD)",
       x = "Diferencia de medias", y = "Comparación con bloques de Operador")+
  Tema

#### LSD con ajuste en p-values por Bonferroni (considerando el bloque) -----

cme <- deviance(modelo_gl)/df.residual(modelo_gl)

Fisher <- LSD.test(DCG$Car, DCG$Forma, df.residual(modelo_gl), cme,
                   p.adj = "bonferroni",
                   group = T,
                   main = "Carga")

Fisher

data.frame(Fisher$groups) %>% 
  rownames_to_column(., var = "Forma") %>% 
  left_join(data.frame(Fisher$means) %>% 
              select(`DCG.Car`,
                     std),
            by = c("DCG.Car")) %>% 
  select(all_of(c(
    "Forma",
    "groups",
    "DCG.Car",
    "std"
  ))) %>% 
  set_names(nm = c("Forma",
                   "Grupos",
                   "Media",
                   "se")) %>% 
  ggplot(aes(x = reorder(Forma, -Media), y = Media, color = Grupos)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Media - se, ymax = Media + se), width = 0.2) +
  geom_text(aes(label = Grupos), vjust = -1, size = 5) +
  labs(title = "Medias de Carga con Letras (LSD Bonferroni)",
       x = "Forma", y = "Media de Car")+
  Tema+
  theme(legend.position = "none")


#### Duncan con ajuste en p-values (considerando el bloque) -----

cme <- deviance(modelo_gl)/df.residual(modelo_gl)

Duncan <- duncan.test(DCG$Car, DCG$Forma, df.residual(modelo_gl), cme,
                      group = T)


data.frame(Duncan$groups) %>% 
  rownames_to_column(., var = "Forma") %>% 
  left_join(data.frame(Duncan$means) %>% 
              select(`DCG.Car`,
                     std),
            by = c("DCG.Car")) %>% 
  select(all_of(c(
    "Forma",
    "groups",
    "DCG.Car",
    "std"
  ))) %>% 
  set_names(nm = c("Forma",
                   "Grupos",
                   "Media",
                   "se")) %>% 
  ggplot(aes(x = reorder(Forma, -Media), y = Media, color = Grupos)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Media - se, ymax = Media + se), width = 0.2) +
  geom_text(aes(label = Grupos), vjust = -1, size = 5) +
  labs(title = "Medias de Carga con Letras (LSD Bonferroni)",
       x = "Forna", y = "Media de Carga")+
  Tema+
  theme(legend.position = "none")


