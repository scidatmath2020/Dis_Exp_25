#===========================================================================#
# Pruebas de hipótesis                                                      #
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
               "openxlsx")

# Llamado de base de datos -----

Datos <- read_excel("Bases de datos/datos_adonai.xlsx")
View(Datos)

# Función para el calculo de edades ----

calcular_edad <- function(fecha_nac) {
  hoy <- Sys.Date()
  edad <- as.numeric(format(hoy, "%Y")) - as.numeric(format(fecha_nac, "%Y"))
  return(edad)
}

calcular_edad(as.Date("1998-07-27"))

# Seleccionar datos que sean de México ----

Edades_mexico <- Datos[Datos$pais == "México" & !is.na(Datos$pais),]

Edades_mexico$edad <- calcular_edad(Edades_mexico$`fecha nacimiento`)

# ¿Cuál es la media de la edad de los estudiantes de SciData de México? ----

mean(Edades_mexico$edad)
sd(Edades_mexico$edad)

# Prueba t para ver si la muestra es igual a mu0 = 29 años ----

# Según CPyV 2020 la edad promedio del mexicano era de 29 años 

# H0: mu = 29
# HA: mu =/ 29

t.test(Edades_mexico$edad, mu = 29, 
       alternative = "two.sided", conf.level = 0.95)
