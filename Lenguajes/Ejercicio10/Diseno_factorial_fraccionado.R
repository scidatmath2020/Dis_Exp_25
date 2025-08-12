#==============================================================================#
# Diseños factoriales fraccionados                                             #
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
  "DescTools",
  "FrF2",
  "ggthemes"
)

# Generación de diseños fraccionarios -----

frac_desing1 <- FrF2(nfactors = 7, resolution = 4, randomize = F)

# Diseño factorial fraccionado 2^{5-1} con resolución V -----

rendimientos_factor <- read.csv('Bases de datos/yield_factors.csv',
                                encoding = "latin1") %>% 
  set_names(c("Factor", "-1", "+1"))


datos_rendimiento <- read.csv("Bases de datos/yield_data.csv") 

datos_rendimiento2 <- read.csv("Bases de datos/yield_data.csv") %>% 
  mutate(across(c(
    "FeedRate",
    "Catalyst",
    "AgitationSpeed",
    "Temperature",
    "Concentration"
  ), ~factor(.)))

# Ajustar regresión lineal solo con efectos principales e interacción de dos factores ----

rendimiento_lm <- lm(Yield ~ (.)^2, data = datos_rendimiento)

summary(rendimiento_lm)

## Efectos alias ----

aliases(rendimiento_lm)

## Anpalisis gráfico ------

### Gráfico de Daniel ----

DanielPlot(rendimiento_lm)

### Diagrama de pareto -----

coeficientes_lm <- coefficients(rendimiento_lm)[-1]

efectos_rendimiento <- data.frame(
  Efecto = names(coeficientes_lm),
  Valor = unname(coeficientes_lm),
  Valor_absoluto = abs(unname(coeficientes_lm)),
  Sig = as.character(unname(sign(coeficientes_lm)))
)


efectos_rendimiento %>% 
  ggplot(aes(Valor_absoluto, reorder(Efecto, -Valor_absoluto, abs)))+
  geom_col(aes(fill = Sig))+
  labs(title = "Gráfico de pareto en el experimento 2^{5-1}",
       x = "Magnitud del efecto",
       y = "Efecto")+
  theme_solarized()

## Análisis posterior mediante ANOVA ----

rendimiento_lm2 <- lm(Yield ~ Catalyst*Temperature*Concentration, 
                      data = datos_rendimiento)
anova(rendimiento_lm2)

Modelo <- aov(Yield ~ Catalyst*Temperature*Concentration, 
              data = datos_rendimiento2)

summary(Modelo)

### Comparación de medias con LSD por factor (ignorando la interacción) ----

#### Método lsd ----

PostHocTest(Modelo, method = "lsd")


#### Método Duncan ----

PostHocTest(Modelo, method = "duncan")

#### Verificación de supuestos ---- 

residuos <- residuals(Modelo)

##### Test de Shapiro ----

shapiro.test(residuos)

##### Test de jarque-bera ----

tseries::jarque.bera.test(residuos)


# Diseño factorial fraccionado de 2 ^ {8-4} ----

paint_data <- read.csv("Bases de datos/paint_data.csv")

paint_data2 <- read.csv("Bases de datos/paint_data.csv") %>% 
  mutate(across(c("A", "B", "C", "D", "E","F", "G", "H"), ~factor(.)))

## Estimación del modelo lineal -----

paint_lm <- lm(Brightness ~ (.)^2, data = paint_data)
summary(paint_lm)


## Revisar alias ----

aliases(paint_lm)

## Análisis gráfico ----

### Gráfico de Daniel ----

DanielPlot(paint_lm)

### Diagrama de Pareto ---

paint_coef <- coefficients(paint_lm)[-1]
paint_coef <- paint_coef[!is.na(paint_coef)]

efectos_paint <- data.frame(
  Efecto = names(paint_coef),
  Valor = unname(paint_coef),
  Valor_absoluto = abs(unname(paint_coef)),
  Sig = as.character(unname(sign(paint_coef)))
)


efectos_paint %>% 
  ggplot(aes(Valor_absoluto, reorder(Efecto, -Valor_absoluto, abs)))+
  geom_col(aes(fill = Sig))+
  labs(title = "Gráfico de pareto en el experimento 2^{8-4}",
       x = "Magnitud del efecto",
       y = "Efecto")+
  theme_solarized()

## Análisis posterior mediante ANOVA ----

paint_lm2 <- lm(Brightness ~ A*B, data = paint_data)
anova(paint_lm2)

Modelo <- aov(Brightness ~ A*B, data = paint_data2)

summary(Modelo)

### Comparación de medias con LSD por factor (ignorando la interacción) ----

#### Método lsd ----

PostHocTest(Modelo, method = "lsd")


#### Método Duncan ----

PostHocTest(Modelo, method = "duncan")

#### Verificación de supuestos ---- 

residuos <- residuals(Modelo)

##### Test de Shapiro ----

shapiro.test(residuos)

##### Test de jarque-bera ----

tseries::jarque.bera.test(residuos)






