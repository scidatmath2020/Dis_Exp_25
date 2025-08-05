#==============================================================================#
# Modelo ANOVA (Diseño factorial 3^2)                                          #
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

Salarios <- read.csv("Bases de datos/Sal_Sexo_Formal_Seguro_ENOET125.csv")

# Conversión de columnas al tipo factor ------

unique(Salarios$Empleo)
unique(Salarios$Sexo)
unique(Salarios$Seguro)

Salarios <- Salarios %>% 
  mutate(Sexo = factor(Sexo, levels = c("Hombre", "Mujer")),
         Empleo = factor(Empleo, levels = c("Formal", "Informal")),
         Seguro = factor(Seguro, levels = c("Con acceso", "Sin acceso")))


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
              strip.text = element_text(size = 11,
                                        color = "grey25"),
              legend.position = "bottom",
              panel.background = element_rect(fill = "#E3F2FD"),
              plot.background = element_rect(fill = "#E3F2FD"),
              legend.background = element_rect(fill = "#E3F2FD"),
              strip.background = element_rect(fill = "#E3F2FD"),
              panel.grid.major = element_line(color = "grey",
                                              lineend = "butt"),
              panel.grid.minor = element_blank())

# Gráfico de cajas de los factores sector por jornada-----

Etiquetas <- Salarios %>% 
  group_by(Sexo, Empleo, Seguro) %>% 
  summarise(Promedios = round(mean(Salario_trim), 2)) %>% 
  pull(Promedios)

Salarios %>% 
  ggplot(aes(x = Sexo, y = Salario_trim, fill = Empleo))+
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.75))+
  geom_boxplot()+
  stat_summary(aes(fill = Empleo),
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
  scale_fill_manual(values = c(
    "Formal" = "#4CAF50",
    "Informal" = "#FF8A65"
  ))+
  facet_wrap(~Seguro)+
  labs(title = "Salario mensual de la población económicamente activa y ocupada \nsegún Sexo, por tip empleo y con seguridad social",
       subtitle = "promedio mensual\nT1 2025",
       x = "Sexo",
       y = "Salario promedio mensual",
       fill = "Empleo")+
  Tema


# Validación de supuestos ----

## Normalidad ----

Test_norm <- bind_rows(
  Salarios %>% 
    group_by(Sexo) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  Salarios %>% 
    group_by(Empleo) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  Salarios %>% 
    group_by(Seguro) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  Salarios %>% 
    group_by(Sexo, Empleo) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    mutate(Nivel_factor = paste(Sexo, "-", Empleo)) %>% 
    select(Nivel_factor, p_value) %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  
  Salarios %>% 
    group_by(Sexo, Seguro) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    mutate(Nivel_factor = paste(Sexo, "-", Seguro)) %>% 
    select(Nivel_factor, p_value) %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  
  Salarios %>% 
    group_by(Seguro, Empleo) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    mutate(Nivel_factor = paste(Seguro, "-", Empleo)) %>% 
    select(Nivel_factor, p_value) %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  
  Salarios %>% 
    group_by(Sexo, Empleo, Seguro) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    mutate(Nivel_factor = paste(Sexo, "-", Empleo, "-", Seguro)) %>% 
    select(Nivel_factor, p_value) %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB"))
  
)

Test_norm


## Homogeniedad de varianzas (igualdas en varianzas) -----

### Levene ----

leveneTest(Salario_trim ~ Sexo * Empleo * Seguro, data = Salarios)

### Bartlett---

bartlett.test(Salario_trim ~ interaction(Sexo,Empleo,Seguro), data = Salarios)

## Flinger-Killen ----

fligner.test(Salario_trim ~ interaction(Sexo,Empleo,Seguro), data = Salarios)


# Validación de supuestos transformación box - cox ----

Box_shift <- function(x){
  A <- 1-min(x)
  shift <- x+A
  bc <- boxcox(shift ~ 1, plotit = F)
  lambda <- bc$x[which.max(bc$y)]
  if (abs(lambda) < 1e-4) {
    y_bc <- log(x)
  } else {
    y_bc <- (x^lambda - 1) / lambda
  }
  return(y_bc)
}

Salarios$Salario_trim_BC <- Box_shift(Salarios$Salario_trim)

## Normalidad ----

Test_norm <- bind_rows(
  Salarios %>% 
    group_by(Sexo) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim_BC)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  Salarios %>% 
    group_by(Empleo) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim_BC)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  Salarios %>% 
    group_by(Seguro) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim_BC)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  Salarios %>% 
    group_by(Sexo, Empleo) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim_BC)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    mutate(Nivel_factor = paste(Sexo, "-", Empleo)) %>% 
    select(Nivel_factor, p_value) %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  
  Salarios %>% 
    group_by(Sexo, Seguro) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim_BC)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    mutate(Nivel_factor = paste(Sexo, "-", Seguro)) %>% 
    select(Nivel_factor, p_value) %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  
  Salarios %>% 
    group_by(Seguro, Empleo) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim_BC)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    mutate(Nivel_factor = paste(Seguro, "-", Empleo)) %>% 
    select(Nivel_factor, p_value) %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB")),
  
  Salarios %>% 
    group_by(Sexo, Empleo, Seguro) %>% 
    summarise(p_value = jarque.bera.test(Salario_trim_BC)$p.value,
              p_value = round(p_value, 3)) %>% 
    ungroup() %>% 
    mutate(Nivel_factor = paste(Sexo, "-", Empleo, "-", Seguro)) %>% 
    select(Nivel_factor, p_value) %>% 
    set_names(nm = c("Nivel de factor",
                     "P-value JB"))
  
)

Test_norm


## Homogeniedad de varianzas (igualdas en varianzas) -----

### Levene ----

leveneTest(Salario_trim_BC ~ Sexo * Empleo * Seguro, data = Salarios)

### Bartlett---

bartlett.test(Salario_trim_BC ~ interaction(Sexo,Empleo,Seguro), data = Salarios)

## Flinger-Killen ----

fligner.test(Salario_trim_BC ~ interaction(Sexo,Empleo,Seguro), data = Salarios)

# ANOVA -----

## Con AOV -----

Modelo <- aov(Salario_trim_BC ~ Sexo * Empleo * Seguro, data = Salarios)
summary(Modelo)

#  Comparación de medias con LSD por factor (ignorando la interacción) ----

## Método lsd ----

PostHocTest(Modelo, method = "lsd")


# Gráfico de interacción con ggplot2 ----

Interacciones <- Salarios %>% 
  group_by(Sexo, Empleo, Seguro) %>% 
  summarise(Media = mean(Salario_trim, na.rm = TRUE),
            SE    = sd(Salario_trim, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

Interacciones %>% 
  ggplot(aes(x = Empleo, y = Media,
             group = Sexo, colour = Sexo)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),
                width = .1) +
  scale_color_manual(values = c(
    "Hombre" = "#4CAF50",
    "Mujer" = "#FF8A65"
  ))+
  facet_wrap(~Seguro)+
  labs(title = "Interacción del factor sexo, empleo y seguro",
       subtitle = "Salario promedio mensual\nT1 2025",
       y = "Salario Trimestral Medio",
       x = "Tipo de empleo",
       colour = "Sexo")+
  Tema


Interacciones %>% 
  ggplot(aes(x = Sexo, y = Media,
             group = Empleo, colour = Empleo)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Media - SE, ymax = Media + SE),
                width = .1) +
  scale_color_manual(values = c(
    "Formal" = "#4CAF50",
    "Informal" = "#FF8A65"
  ))+
  facet_wrap(~Seguro)+
  labs(title = "Interacción del factor sexo, empleo y seguro",
       subtitle = "Salario promedio mensual\nT1 2025",
       y = "Salario Trimestral Medio",
       x = "Sexo",
       colour = "Empleo")+
  Tema
