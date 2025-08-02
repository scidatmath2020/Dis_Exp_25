#==============================================================================#
# Modelo ANOVA (Diseño factorial 2^2)                                          #
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

# Lectura de datos -----

Salarios <- read.csv("Bases de datos/Sal_Sexo_Formal_ENOET125.csv")

# Conversión de columnas al tipo factor ------

Salarios <- Salarios %>% 
  mutate(Sexo = factor(Sexo, levels = c("Hombre", "Mujer")),
         Empleo = factor(Empleo, levels = c("Formal", "Informal")))

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

# Gráfico de cajas de los factores sexo por tipo de empleo -----

Etiquetas <- Salarios %>% 
  group_by(Sexo, Empleo) %>% 
  summarise(Promedios = round(mean(Salario_trim), 2)) %>% 
  pull(Promedios)

Salarios %>% 
  ggplot(aes(x = Sexo, y = Salario_trim, fill = Empleo))+
  geom_boxplot()+
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  stat_summary(aes(fill = Empleo),
               fun = mean,
               geom = "point",
               shape = 23,
               size = 4,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean,
               geom = "text",
               label = paste0("Media:", Etiquetas),
               size = 4,
               position = position_dodge(width = 0.75),
               vjust = -1.2,
               hjust = -1.1)+
  scale_y_continuous(breaks = seq(0,15000,1000))+
  scale_fill_manual(values = c(
    "Formal" = "#4CAF50",
    "Informal" = "#FF8A65"
  ))+
  labs(title = "Salario mensual de la población económicamente activa y ocupada según sexo, por tipo de empleo",
       subtitle = "promeido mensual\nT1 2025",
       x = "Sexo",
       y = "Salario promedio mensual",
       fill = "Tipo de empleo")+
  Tema


# Gráfico de cajas de los factores sexo por tipo de empleo -----

Etiquetas2 <- Salarios %>% 
  group_by(Empleo, Sexo) %>% 
  summarise(Promedios = round(mean(Salario_trim), 2)) %>% 
  pull(Promedios)

Salarios %>% 
  ggplot(aes(x = Empleo, y = Salario_trim, fill = Sexo))+
  geom_boxplot()+
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  stat_summary(aes(fill = Sexo),
               fun = mean,
               geom = "point",
               shape = 23,
               size = 4,
               position = position_dodge(width = 0.75))+
  stat_summary(fun = mean,
               geom = "text",
               label = paste0("Media:", Etiquetas2),
               size = 4,
               position = position_dodge(width = 0.75),
               vjust = -1.2,
               hjust = -1.1)+
  scale_y_continuous(breaks = seq(0,15000,1000))+
  scale_fill_manual(values = c(
    "Hombre" = "#4CAF50",
    "Mujer" = "#FF8A65"
  ))+
  labs(title = "Salario mensual de la población económicamente activa y ocupada según tipo de empleo, por sexo",
       subtitle = "promeido mensual\nT1 2025",
       x = "Sexo",
       y = "Salario promedio mensual",
       fill = "Tipo de empleo")+
  Tema


# Validación de supuestos ----

## Normalidad ----

### Para sexo ----

Salarios %>% 
  group_by(Sexo) %>% 
  summarise(p_value = jarque.bera.test(Salario_trim)$p.value)

### Para empleo ----

Salarios %>% 
  group_by(Empleo) %>% 
  summarise(p_value = jarque.bera.test(Salario_trim)$p.value)

### Interacción sexo y empleo ----

Salarios %>% 
  group_by(Sexo, Empleo) %>% 
  summarise(p_value = jarque.bera.test(Salario_trim)$p.value)

## Igualdad en varianzas -----

### Levene ----

leveneTest(Salario_trim ~ Sexo * Empleo, data = Salarios)

### Bartlett---

bartlett.test(Salario_trim ~ interaction(Sexo,Empleo), data = Salarios)

## Flinger-Killen ----

fligner.test(Salario_trim ~ interaction(Sexo,Empleo), data = Salarios)


# Validación de supuestos con Variable explicativa en log----

## Normalidad ----

### Para sexo ----

Salarios %>% 
  group_by(Sexo) %>% 
  summarise(p_value = jarque.bera.test(log(Salario_trim))$p.value)

### Para empleo ----

Salarios %>% 
  group_by(Empleo) %>% 
  summarise(p_value = jarque.bera.test(log(Salario_trim))$p.value)

### Interacción sexo y empleo ----

Salarios %>% 
  group_by(Sexo, Empleo) %>% 
  summarise(p_value = jarque.bera.test(log(Salario_trim))$p.value)

## Igualdad en varianzas -----

### Levene ----

leveneTest(log(Salario_trim) ~ Sexo * Empleo, data = Salarios)

### Bartlett---

bartlett.test(log(Salario_trim) ~ interaction(Sexo,Empleo), data = Salarios)

## Flinger-Killen ----

fligner.test(log(Salario_trim) ~ interaction(Sexo,Empleo), data = Salarios)

# Estimación del modelo ANOVA -----

Modelo <- aov(log(Salario_trim) ~ Sexo * Empleo, data = Salarios)

summary(Modelo)

# Gráfico de interacción con ggplot2 ----

Interacciones <- Salarios %>% 
  group_by(Sexo, Empleo) %>% 
  summarise(Media = mean(Salario_trim, na.rm = TRUE),
            SE    = sd(Salario_trim, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

Interacciones %>% 
  ggplot(aes(x = Empleo, y = Media,
             group = Sexo, colour = Sexo)) +
  geom_line(position = position_dodge(0.1), size = 1.2) +
  geom_point(position = position_dodge(0.1), size = 3) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),
                width = .1, position = position_dodge(0.1)) +
  scale_color_manual(values = c(
    "Hombre" = "#4CAF50",
    "Mujer" = "#FF8A65"
  ))+
  labs(title = "Interacción del factor sexo y empleo",
       subtitle = "Salario promedio mensual\nT1 2025",
       y = "Salario Trimestral Medio",
       x = "Tipo de empleo",
       colour = "Sexo")+
  Tema

Interacciones %>% 
  ggplot(
    aes(x = Sexo, y = Media,
        group = Empleo, colour = Empleo)
  ) +
  geom_line(position = position_dodge(0.1), size = 1.2) +
  geom_point(position = position_dodge(0.1), size = 3) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),
                width = .1, position = position_dodge(0.1)) +
  scale_color_manual(values = c(
    "Formal" = "#4CAF50",
    "Informal" = "#FF8A65"
  ))+
  labs(title = "Interacción del factor empleo y sexo",
       subtitle = "Salario promedio mensual\nT1 2025",
       y = "Salario Trimestral Medio",
       x = "Sexo",
       colour = "Tipo de empleo")+
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

