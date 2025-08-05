#==============================================================================#
# Modelo ANOVA (Diseño factorial 2^3)                                          #
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
  "data.table",
  "car",
  "MASS",
  "agricolae",
  "ggpubr",
  "tseries",
  "ggthemes",
  "DescTools",
  "rstatix"
)

# Cargar datos ----

Salarios <- read.csv("Bases de datos/Sal_Sector_Jornada_ENOET125.csv")

# Conversión de columnas al tipo factor ------

unique(Salarios$Sector)
unique(Salarios$Jornada)

Salarios <- Salarios %>% 
  mutate(Sector = factor(Sector, levels = c("Primario",
                                            "Secundario",
                                            "Terciario")),
         Jornada = factor(Jornada, levels = c("Menos de 15",
                                              "De 15 a 48",
                                              "Más de 48")))

# Tema personalizado para la visualización de los datos -----

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

# Gráfico de cajas de los factores sector por jornada-----

Etiquetas <- Salarios %>% 
  group_by(Sector, Jornada) %>% 
  summarise(Promedios = round(mean(Salario_trim), 2)) %>% 
  pull(Promedios)

Salarios %>% 
  ggplot(aes(x = Sector, y = Salario_trim, fill = Jornada))+
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  geom_boxplot()+
  stat_summary(aes(fill = Jornada),
               fun = mean,
               geom = "point",
               shape = 23,
               size = 4,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean,
               geom = "text",
               label = paste0("Media:", Etiquetas),
               size = 3,
               position = position_dodge(width = 0.75),
               vjust = 3)+
  scale_y_continuous(breaks = seq(0,15000,1000))+
  scale_fill_manual(values = c(
    "Menos de 15" = "#4CAF50",
    "De 15 a 48" = "#FF8A65",
    "Más de 48" = "#D98880"
  ))+
  labs(title = "Salario mensual de la población económicamente activa y ocupada según sector, por duración de la jornada",
       subtitle = "promedio mensual\nT1 2025",
       x = "Sector",
       y = "Salario promedio mensual",
       fill = "Duración de jornada")+
  Tema


# Gráfico de cajas de los factores según jornada por tipo de sector -----

Etiquetas2 <- Salarios %>% 
  group_by(Jornada, Sector) %>% 
  summarise(Promedios = round(mean(Salario_trim), 2)) %>% 
  pull(Promedios)

Salarios %>% 
  ggplot(aes(x = Jornada, y = Salario_trim, fill = Sector))+
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  geom_boxplot()+
  stat_summary(aes(fill = Sector),
               fun = mean,
               geom = "point",
               shape = 23,
               size = 4,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean,
               geom = "text",
               label = paste0("Media:", Etiquetas2),
               size = 3,
               position = position_dodge(width = 0.75),
               vjust = 3)+
  scale_y_continuous(breaks = seq(0,15000,1000))+
  scale_fill_manual(values = c(
    "Primario" = "#4CAF50",
    "Secundario" = "#FF8A65",
    "Terciario" = "#D98880"
  ))+
  labs(title = "Salario mensual de la población económicamente activa y ocupada según duración de jornada semanal, por sector",
       subtitle = "promedio mensual\nT1 2025",
       x = "Duración de jornada",
       y = "Salario promedio mensual",
       fill = "Sector")+
  Tema


# Validación de supuestos ----

## Normalidad ----

Test_norm <- bind_rows(
  Salarios %>% 
    group_by(Sector) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim)$p.value) %>% 
    ungroup() %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  Salarios %>% 
    group_by(Jornada) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim)$p.value) %>% 
    ungroup() %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  Salarios %>% 
    group_by(Sector, Jornada) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim)$p.value) %>% 
    ungroup() %>% 
    mutate(Nivel_factor = paste(Sector, "-", Jornada)) %>% 
    select(Nivel_factor, p_value) %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB"))
  
)

Test_norm


## Homogeniedad de varianzas (igualdas en varianzas) -----

### Levene ----

leveneTest(Salario_trim ~ Sector * Jornada, data = Salarios)

### Bartlett---

bartlett.test(Salario_trim ~ interaction(Sector,Jornada), data = Salarios)

## Flinger-Killen ----

fligner.test(Salario_trim ~ interaction(Sector,Jornada), data = Salarios)


# Validación de supuestos (con transformación de raíz) ----

## Normalidad ----

Test_norm_sqrt <- bind_rows(
  Salarios %>% 
    group_by(Sector) %>% 
    summarise(p_value = jarque.bera.test(sqrt(Salario_trim))$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  Salarios %>% 
    group_by(Jornada) %>% 
    summarise(p_value = jarque.bera.test(sqrt(Salario_trim))$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  Salarios %>% 
    group_by(Sector, Jornada) %>% 
    summarise(p_value = jarque.bera.test(sqrt(Salario_trim))$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    mutate(Nivel_factor = paste(Sector, "-", Jornada)) %>% 
    select(Nivel_factor, p_value) %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB"))
  
)

Test_norm_sqrt


## Homogeniedad de varianzas (igualdas en varianzas) -----

### Levene ----

leveneTest(sqrt(Salario_trim) ~ Sector * Jornada, data = Salarios)

### Bartlett---

bartlett.test(sqrt(Salario_trim) ~ interaction(Sector,Jornada), data = Salarios)

## Flinger-Killen ----

fligner.test(sqrt(Salario_trim) ~ interaction(Sector,Jornada), data = Salarios)


# Estimación del modelo ANOVA -----

## Con AOV -----

Modelo <- aov(sqrt(Salario_trim) ~ Sector * Jornada, data = Salarios)
summary(Modelo)

## Con lm y posteriorimente anova ----

Modelo <- lm(sqrt(Salario_trim) ~ Sector * Jornada, data = Salarios)
summary(Modelo)
Modelo <- anova(Modelo)
Modelo


# Gráfico de interacción con ggplot2 ----

Interacciones <- Salarios %>% 
  group_by(Sector, Jornada) %>% 
  summarise(Media = mean(Salario_trim, na.rm = TRUE),
            SE    = sd(Salario_trim, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

Interacciones %>% 
  ggplot(aes(x = Jornada, y = Media,
             group = Sector, colour = Sector)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),
                width = .1) +
  scale_color_manual(values = c(
    "Primario" = "#4CAF50",
    "Secundario" = "#FF8A65",
    "Terciario" = "#D98880"
  ))+
  labs(title = "Interacción del factor Sector y Jornada",
       subtitle = "Salario promedio mensual\nT1 2025",
       y = "Salario Trimestral Medio",
       x = "Tipo de Jornada",
       colour = "Sector")+
  Tema

Interacciones %>% 
  ggplot(
    aes(x = Sector, y = Media,
        group = Jornada, colour = Jornada)
  ) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),
                width = .1) +
  scale_color_manual(values = c(
    "Menos de 15" = "#4CAF50",
    "De 15 a 48" = "#FF8A65",
    "Más de 48" = "#D98880"
  ))+
  labs(title = "Interacción del factor Jornada y Sector",
       subtitle = "Salario promedio mensual\nT1 2025",
       y = "Salario Trimestral Medio",
       x = "Sector",
       colour = "Tipo de Jornada")+
  Tema


#  Comparación de medias con LSD por factor (ignorando la interacción) ----

## Método lsd ----

PostHocTest(Modelo, method = "lsd")

## Método Duncan ----

PostHocTest(Modelo, method = "duncan")

#  Verificación de supuestos ---- 

residuos <- residuals(Modelo)

## Test de Shapiro ----

shapiro.test(residuos)

## Test de jarque-bera ----

tseries::jarque.bera.test(residuos)

## Gráfico Q-Q ----

data.frame(residuos) %>% 
  ggplot(aes(sample = residuos))+
  geom_qq(color = "#FB8C00",
          size = 3)+
  geom_qq_line(color = "#80CBC4",
               linewidth = 1.1)+
  labs(title = "Gráfico Cuartil-Cuartil para detectar distribución teórica",
       subtitle = "residuales del Modelo ANOVA",
       y = "Residual",
       x = "Cuartiles teóricos")+
  Tema
