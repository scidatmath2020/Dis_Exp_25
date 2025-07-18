#===========================================================================#
# Estimación puntual, media e intervalos de confianza                       #
# Autor: Alexis Adonai Morales Alberto                                      #
# SCIDATA                                                                   #
# Código de R para clase                                                    #
#===========================================================================#

# Datos de prueba -----

Mediciones <-  c(3.6, 6.9, 5.1, 4.2, 5.5, 7.2, 3.0, 5.8,
                4.9, 9.9, 7.1, 5.4, 6.2, 4.5, 6.3, 8.2, 
                5.7, 4.4, 7.9, 3.2)

# Media ----

## Manual ----

X_MediaM <-  sum(Mediciones)/length(Mediciones)
X_MediaM 

## Función ----

X_Media <-  mean(Mediciones)
X_Media

# Desviación estándar ----

## Manual ----

Sigma <- sqrt((sum((Mediciones-mean(Mediciones))^2)/(length(Mediciones)-1)))
Sigma

## Función ----

SD <- sd(Mediciones)
SD

# Grados de libertad ----

GL <- length(Mediciones)-1

# Nivel de confianza ----

alfa2 <- (1-0.95)/2

# Cálculo de t al 5% ----

t <- qt(1-alfa2, df = GL)
t

# Cálculo de limites ----

## Superior ----

LI1 <- X_Media + t*(SD/sqrt(length(Mediciones)))

## Inferior ----

LT2 <- X_Media - t*(SD/sqrt(length(Mediciones)))

paste0("(",round(LT2,2), "<=", round(X_Media, 2), "<=", round(LI1,2), ")")

# Programación para cálcular, media y limites ----

Intervalo <- function(x, alpha){
  n <- length(x)
  media <- mean(x)
  sd <- sd(x)
  errortip = sd/sqrt(n)
  t = qt(alpha/2, df = n-1)
  LI = media - (t*errortip)
  LS = media + (t*errortip)
  Salida <- data.frame(
    "Estadísticos" = c("Media", "Limite inferior",
                       "Limite superior"),
    "Valores" = c(media, LI, LS)
  )
  Salida
}

Intervalo(Mediciones, alpha = 0.05)


# Estimación de la variación ----

## Manual ----

VarianzaM <- sum((Mediciones-mean(Mediciones))^2)/(length(Mediciones)-1)

VarianzaM

## Función ----

Varianza <- var(Mediciones)
Varianza

## Obtenemos el valor de Chi2 ----

ChiS <- qchisq(0.05/2, df = length(Mediciones)-1)
ChisI <-qchisq(1-(0.05/2), df = length(Mediciones)-1) 

## Calculo de intervalo ----

### Superior -----

LSV <- ((length(Mediciones)-1)*Varianza) / ChiS

LSV

### Inferior ----

LIV <- ((length(Mediciones)-1)*Varianza) / ChisI

LIV

## Construcción de función para cálculo directo ----

Intervalo_var <- function(x, alpha) {
  n <- length(x)
  gl <- n-1
  chis <- qchisq(alpha/2, df = gl)
  chisI <- qchisq(1-(alpha/2), df = gl)
  varm <- var(x)
  LSV <- (gl*varm)/chis
  LIV <- (gl*varm)/chisI
  Salida <- data.frame(
    "Estadísticos" = c("Varianza muestral",
                       "Limite superior",
                       "Limite inferior"),
    "Valores" = c(varm, LSV, LIV)
  )
  Salida
}

Intervalo_var(Mediciones, alpha = 0.05)
