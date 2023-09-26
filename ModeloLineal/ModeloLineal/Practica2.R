# Práctica 2

## Ejercicio 4

# Levanto los datos de vapor
data <- read.table("vapor.txt", header=T)

### A: diagrama de dispersión Y vs. X

plot(data$y, data$x1, pch=20)

### B: Media y desvio standar de cada variable

mu_x <- mean(data$x1)
mu_x

sd_x <- sd(data$x1)
sd_x

mu_y <- mean(data$y)
mu_y

sd_y <- sd(data$y)
sd_y

### C: estimadores de mínimos cuadrados para un modelo lineal

model <- lm(y~x1, data = data)
sum_lm <- summary(model)
sum_lm

### D: Cuánto vale el estimador de sigma2
sigma2 <- sum_lm$sigma^2
sigma2

### E: Matriz de covarianza de los estimadores obtenidos
cov_estimadores <- vcov(model)
cov_estimadores

### F: Verificar que la suma de Y-Y_pred = 0
x<-data.frame(x1=data$x1)
y_pred <- predict(model, x)

sum(data$y-y_pred)

### G: Estandarizar las observaciones y calcular todo de nuevo
x_norm <- scale(data$x1)
y <- data$y

model_norm <- lm(y~x_norm)
summary(model_norm)
vcov(model_norm)

## Ejercicio 5
library(openintro)
data(bdims, package="openintro")

### A: Diagrama de dispersión que muestre la relación delwgt y cadera
plot(bdims$hip_gi, bdims$wgt, pch=20, 
     xlab="cadera (cm)", ylab="peso (kg)")

### B: Como cambia la relación si mido el peso en libras: no cambia
plot(bdims$hip_gi, bdims$wgt*2.20, pch=20, 
     xlab="cadera (cm)", ylab="peso (lb)")

### C: Ajustar un modelo lineal
x <- bdims$hip_gi
y <- bdims$wgt

model <-lm(y~x)
summary(model)

### D: Superponer la recta ajustada al scatterplot
plot(bdims$hip_gi, bdims$wgt, pch=20, 
     xlab="cadera (cm)", ylab="peso (kg)")
abline(model, col="firebrick")

### E: Predecir el peso de una persona con 100cm de cadera
new <- data.frame(x=100)
y_pred <- predict(model, newdata = new)
y_pred

### F: calcular el residuo si y_true=81
y_true <- 81
res <- y_true - y_pred
res

### G: 


## Ejercicio 6
library(alr4)
data(Heights)

altura_madre <- Heights$mheight
altura_hija <- Heights$dheight

### 6.a:  Scatterplot

library(ggplot2)

# Sample data
df <- data.frame(X = altura_madre, Y = altura_hija)

# Create a square scatterplot using ggplot2
ggplot(df, aes(X, Y)) +
    geom_point() +
    coord_fixed(ratio = 1)

