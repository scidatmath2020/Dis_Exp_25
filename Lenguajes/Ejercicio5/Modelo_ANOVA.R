#==============================================================================#
# Modelo ANOVA (Para diseño de un factor)                                      #
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

# Lectura de datos ----

Robos <- read.csv("Bases de datos/Robos_2024EF.csv",
                  check.names = F,
                  encoding = "utf-8") %>% 
  data.table() %>% 
  melt(id.vars = c("Entidad"),
       value.name = "Robos",
       variable.name = "Tipo")


Robos <- read.csv("Bases de datos/Robos_2024EF.csv",
                  check.names = F,
                  encoding = "utf-8") %>% 
  pivot_longer(!Entidad, 
               names_to = "Tipo", 
               values_to = "Robos")

# Visualización de las medias -----

Robos %>% 
  ggplot(aes(x = Tipo, y = Robos, fill = Tipo))+
  geom_boxplot(alpha = 0.6)+
  stat_summary(fun = mean, geom = "point",
               size = 3, aes(color = Tipo))+
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar", width = 0.3)+
  labs(title = "Robos cometidos a nivel estatal según tipo de robo",
       subtitle = "2024\nmedia de los robos registrados por entidad federativa",
       caption = "Elaborado por SciData con datos del Secretariado Ejecutivo de la Defensa Nacional",
       x = "Tipo de robo",
       y = "Robos registrados")+
  theme_solarized()+
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        legend.position = "bottom")


Robos %>% 
  ggplot(aes(x = Tipo, y = Robos, fill = Tipo))+
  geom_bar(alpha = 0.6, stat = "summary", fun = mean, alpha = 0.7)+
  stat_summary(fun.data = mean_sdl,
               geom = "errorbar",
               width = 0.3,
               position = position_dodge(width = 0.9))+
  labs(title = "Robos cometidos a nivel estatal según tipo de robo",
       subtitle = "2024\nmedia de los robos registrados por entidad federativa",
       caption = "Elaborado por SciData con datos del Secretariado Ejecutivo de la Defensa Nacional",
       x = "Tipo de robo",
       y = "Robos registrados")+
  theme_solarized()+
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        legend.position = "bottom")


# Diagnostico de los supuestos -----

## Normalidad ----

norm_test <- Robos %>% 
  group_by(Tipo) %>% 
  summarise(p_value = jarque.bera.test(Robos)$p.value) %>% 
  ungroup() %>% 
  mutate(Conclusión = ifelse(p_value < 0.05,
                             "No normalidad",
                             "Normalidad"))

norm_test

## Igualdad en varianzas ----

Robos$Tipo <- factor(Robos$Tipo)

levene_test <- leveneTest(Robos ~ Tipo, data = Robos)
levene_test

# Transformación Box-Cox desplazada (cuando hay 0's) ----

# NOTA: Si los valores son positivos y no hay 0's se pueden hacer las
# siguientes transformaciones, logarítimica o raíz cuadrada 

## Función ----

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


## Transformación ----

Robos <- Robos %>% 
  mutate(Robos_BC = Box_shift(Robos))

# Rediagnóstico post-transformación ----

## Normalidad por grupo (Jarque-Bera) ----

norm_test <- Robos %>%
  group_by(Tipo) %>%
  summarise(p_value = jarque.bera.test(Robos_BC)$p.value) %>% 
  ungroup() %>% 
  mutate(Conclusión = ifelse(p_value < 0.05,
                             "No normalidad",
                             "Normalidad"))

norm_test


## Homogeneidad de varianzas (Levene) ---

levene_test <- leveneTest(Robos_BC ~ Tipo, data = Robos)
levene_test

# Modelo ANOVA ----

modelo_anova <- aov(Robos_BC ~ Tipo, data = Robos)
summary(modelo_anova)


# Pruebas host-poc ----

## LSD ----

lsd <- LSD.test(modelo_anova, "Tipo", p.adj = "none")
print(lsd$groups)

## Tukey ----

tukey <- TukeyHSD(modelo_anova)
print(tukey)


## Duncan ----

duncan <- duncan.test(modelo_anova, "Tipo")
print(duncan$groups)


# Crear tabla de medias y letras ----

medias <- Robos %>%
  group_by(Tipo) %>%
  summarise(media = mean(Robos_BC, na.rm = TRUE))

letras <- duncan$groups %>%
  rownames_to_column("Tipo") %>%
  dplyr::select(Tipo, letras = groups)

medias_letras <- left_join(medias, letras, by = "Tipo")

## Gráfico ----

ggplot(medias_letras, aes(x = Tipo, y = media, fill = Tipo)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(aes(label = letras), vjust = -0.5, size = 5) +
  labs(title = "Medias post-transformación Box-Cox",
       subtitle = "Letras según test de Duncan (α = 0.05)",
       y = "Media transformada", x = "Grupo") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


# Crear tabla de medias y letras ----

medias <- Robos %>%
  group_by(Tipo) %>%
  summarise(media = mean(Robos_BC, na.rm = TRUE))

letras <- duncan$groups %>%
  rownames_to_column("Tipo") %>%
  dplyr::select(Tipo, letras = groups)

medias_letras <- left_join(medias, letras, by = "Tipo")

## Gráfico ----

ggplot(medias_letras, aes(x = Tipo, y = media, fill = Tipo)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(aes(label = letras), vjust = -0.5, size = 5) +
  labs(title = "Medias post-transformación Box-Cox",
       subtitle = "Letras según test de Duncan (α = 0.05)",
       y = "Media transformada", x = "Grupo") +
  theme_solarized()+
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.5),
        plot.caption = element_text(size = 12,
                                    hjust = 0),
        legend.position = "none")


# Welch's ANOVA -----

welch_res <- oneway.test(Robos_BC ~ Tipo, data = Robos, var.equal = FALSE)
welch_res

## Pruebas post-hoc: Games-Howell ----

gh <- games_howell_test(Robos_BC ~ Tipo, 
                        data = Robos, conf.level = 0.95,
                        detailed = F)
print(gh)
