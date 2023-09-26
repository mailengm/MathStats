# Práctica 5
library(ggplot2)
library(dplyr)
## Ejercicio 1 -----------------------------------------------------------------
data <- read.csv("vinos.txt")

### A. Graficar precio en función del tiempo
vinos <- as.data.frame(data)

scatter<-ggplot(data = vinos, aes(x = tiempo, y = precio)) +
    geom_point() +
    labs(title = "Precio vs. Tiempo",
         x = "Tiempo",
         y = "Precio")

### B

modelo <- lm(precio~tiempo, data = vinos)

### C

summary(modelo)
summary(modelo)$sigma

### D

scatter +
    geom_abline(intercept = modelo$coefficients[1], 
                slope = modelo$coefficients[2],
                color = "firebrick",
                linewidth = 2) 
### E
# El coeficiente es significativo pero no tiene mucho sentido porque no repre-
# senta bien los datos

## Ejercicio 2 -----------------------------------------------------------------

datos2 <- read.csv("vinos2.txt") %>%
    mutate(varie = factor(varie))
vinos2 <- as.data.frame(datos2)

### 2A.####

scatter <- ggplot(data = vinos2, aes(x = tiempo, y = precio, color = varie)) +
    geom_point() +
    labs(title = "Precio vs. Tiempo",
         x = "Tiempo",
         y = "Precio")
scatter

### 2B.####
modelo <- lm(precio~., data=vinos2)

### 2C.####
summary(modelo)
summary(model)$sigma

### 2D.####
varietales <- unique(vinos2$varie)
coefs <- coef(modelo)
ordenadas <-c(coefs[1], coefs[1]+coefs[3:5])
pendientes <- rep(coefs[2], 4)

data_rectas<- as.data.frame(cbind(varietales, ordenadas, pendientes))
# Las rectas son paralelas
 
### 2E.####

scatter +
    geom_abline(data = data_rectas,
                aes(intercept = ordenadas, 
                slope = pendientes,
                color = as.factor(varietales)),
                linewidth = 2)
### 2F. ####
summary(modelo)
# El coeficiente que acompania a tiempo es significativo

### 2G. ####


## Ejercicio 3 -----------------------------------------------------------------

### 3.A  ####
modelo3 <- lm(precio~tiempo*varie, vinos2)

### 3.B ####

summary(modelo3)
summary(modelo3)$sigma

### 3.C ####

coefs <- coef(modelo3)
ordenadas <-c(coefs[1], coefs[1]+coefs[3:5])
pendientes <- rep(coefs[2], 4)
data_rectas<- as.data.frame(cbind(varietales, ordenadas, pendientes))
data_rectas

## Ejercicio 4 -----------------------------------------------------------------