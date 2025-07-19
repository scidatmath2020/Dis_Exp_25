#===========================================================================#
# Pruebas de hipótesis  de dos medias                                       #
# Autor: Alexis Adonai Morales Alberto                                      #
# SCIDATA                                                                   # 
# Diplomado: Diseño de experimentos                                         #
# Código de R para clase                                                    #
#===========================================================================#

# Eliminar objetos de la consola -----

rm(list=ls())

# Comprobar la existencia de pacman -----

if(require("pacman", quietly = T)){
  cat("El paquete de pacman se encuentra en tu versión de R")
} else {
  install.packages("pacman", dependencies = T)
}

# Llamado de paquetes para el ejercicio ----

pacman::p_load("tidyverse",
               "readxl",
               "openxlsx",
               "ggthemes",
               "tseries",
               "car")

# Llamado de base de datos -----

Datos <- read_excel("Bases de datos/datos_adonai.xlsx")
View(Datos)

# Función para el calculo de edades ----

calcular_edad <- function(fecha_nac) {
  hoy <- Sys.Date()
  edad <- as.numeric(format(hoy, "%Y")) - as.numeric(format(fecha_nac, "%Y"))
  return(edad)
}

# Seleccionar datos que sean de México ----

Datos <- Datos %>% 
  filter(!is.na(estatura)) %>% 
  filter(!estatura == 10) %>% 
  mutate(estatura = ifelse(estatura < 100, 
                           estatura * 100, 
                           estatura),
         sexo = factor(sexo))

# Gráfico de caja para observar la diferencia de estaturas por sexo -----

Datos %>% 
  ggplot(aes(x = sexo, y = estatura, fill = sexo))+
  geom_boxplot(alpha = 0.6)+
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3,
               aes(color = sexo))+
  scale_fill_manual(values = c(
    "Femenino" = "#5b2c6f",
    "Masculino" = "#0e6251"
  ))+
  scale_color_manual(values = c(
    "Femenino" = "#f4d03f",
    "Masculino" = "#e59866"
  ))+
  labs(title = "Distribución de la estatura de los alumnos de SciData",
       subtitle = "por sexo\n2025",
       caption = "Elaborado por SciData con datos propios",
       x = "Sexo",
       y = "Estatura en cm.")+
  theme_solarized()+
  theme(legend.position = "bottom")

# Supuestos a validar ----

## Normalidad (Prueba Jarque-Bera)----

# H0: Normalidad / Datos ~ N(u, sigma)
# HA: No normal / Datos ~/ N(u, sigma)

### Global -----

jarque.bera.test(Datos$estatura)

shapiro.test(Datos$estatura)

### Hombres ----

jarque.bera.test(Datos$estatura[Datos$sexo == "Masculino"])

### Mujeres ----

jarque.bera.test(Datos$estatura[Datos$sexo == "Femenino"])

## Igualdad en varianzas -----

# H0: Igualdad en varianzas 
# Ha: No igualdad en varianzas 

### Fligner-Killeen ----

fligner.test(estatura ~ sexo, data = Datos)

### Levene ----

leveneTest(estatura ~ sexo, data = Datos, center = "mean")

# Prueba t -----

# H0: Mu_m - Mu_f = 0   Mu_m = Mu_f
# HA: Mu_m - Mu_f =/0   Mu_m =/ Mu_f

t.test(
  estatura ~ sexo, 
  data = Datos, 
  alternative = "two.sided",
  var.equeal = T,
  conf.level = 0.95
)

t.test(
  y = Datos$estatura[Datos$sexo == "Masculino"],
  x = Datos$estatura[Datos$sexo == "Femenino"],
  alternative = "two.sided",
  var.equeal = T,
  conf.level = 0.95
)
