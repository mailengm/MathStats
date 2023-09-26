# Parcial: Regresión Lineal
## Mailén Gómez Mayol
library(dplyr)
library(ggplot2)
data <- read.csv("wine.csv", header = T)

# Ejercicio 1. ####
## 1.1 ####
mod1 <- lm(Price~AGST, data=data)
mod1

## 1.2 ####
summary(mod1)

## 1.3 ####
# El coeficiente de AGST es positivo por lo cual la recta es creciente

## 1.4 ####

data <- data %>%
    mutate(Hum = as.numeric(WinterRain >= 600))

mod2 <- lm(Price~AGST+Hum, data = data)

# Involucra 2 variables

## 1.5 ####
# En el papel

## 1.6 ####
summary(mod2)

## 1.7 ####
scatter<-ggplot(data = data, aes(x = Year, y = Age)) +
    geom_point() +
    labs(title = "Year vs. Age",
         x = "Year",
         y = "Age")
scatter

cor(data$Year, data$Age)
# no conviene ponerlas a las dos porque están muy correlacionadas

## 1.8 ####

mod3 <- lm(Price~AGST*Hum, data=data)

## 1.9 ####
summary(mod3)

## 1.10 ####

mod4 <- lm(Price~AGST+Hum+WinterRain+HarvestRain+Age+FrancePop, data=data)
summary(mod4)

## 1.11 ####
mod5 <- lm(Price~AGST+Hum+WinterRain+HarvestRain+Age, data=data)
summary(mod5)

## 1.12 ####
pairs(data)

## 1.13 ####
# Planteo test simultaneo para dos condiciones sobre los betas
# La matriz de condiciones es tal que tengo unos en el lugar de beta2: Hum y de
# beta6 FrancePop

A <- matrix(c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 1), nrow = 2)
A

C <- matrix(c(0, 0), nrow = 2)
C

# La matriz del modelo
X <- model.matrix(mod4)
X
betas <- mod4$coefficients

# Calculo el F
F_val <- t(A %*% betas) %*% solve(A %*% solve(t(X) %*% X) %*% t(A)) %*% (A %*% betas) / (nrow(A)*summary(mod4)$sigma^2)
F_val

# Y ahora el p-value
pf(df1 = nrow(A), df2 = nrow(X)-ncol(X), q = as.numeric(F_val), lower.tail = FALSE)

# Ejercicio 2 ####
## 2.1 ####

b0 = 2
b1 = -3
b2 = 2
b12 = 10

n=60

mu_x = 10
var_x = 25

set.seed(3487)
err <- rnorm(n)

x1 <- round(rnorm(n, mu_x, sqrt(var_x)), 2)
x2 <- round(rnorm(n, mu_x, sqrt(var_x)), 2)

y <- b0 + b1*x1 + b2*x2 +  + b12*x1*x2 + err

### 2.1.a ####
model <- lm(y~x1*x2)
summary(model)
beta1 <- model$coefficients[2]
beta1
# El valor de beta1 obtenido es -2.9071, muy cercano al b1 verdadero.

### 2.1.b ####
# Calculo el intervalo de confianza y me quedo con el intervalo para beta1
CI <- confint(model, level = .9)
CI_beta1 <- CI[2,]
CI_beta1
# El intervalo es [-3.0057, -2.8085]

### 2.1.c ####
between(b1, CI[2,1], CI[2, 2])
# El beta1 encontrado en el punto anterior está contenido en el ic
# El verdadero valor de beta1 es b1 = -3

### 2.1.d ####
# Modelo sin interacciones
modelo_wo_int <- lm(y~x1+x2)
alpha1 <- modelo_wo_int$coefficients[2]
alpha1
# La estimación del alpha1 es 95.4429, no está cerca del valor real porque no 
# estamos teniendo en cuenta el término de la interacción.

### 2.1.e ####
# Igual que antes busco el intervalo de confianza con el nuevo modelo y me quedo
# con el valor para alpha1
CI_wo_int <- confint(modelo_wo_int, level = .9)
CI_alpha1 <- CI_wo_int[2,]

### 2.1.f ####
# Me fijo si el valor real de b1 está en el nuevo intervalo de confianza
between(b1, CI_wo_int[2,1], CI_wo_int[2, 2])
# El valor real no está dentro del intervalo de confianza.

## 2.2 ####
# Repito el ejercicio anterior Nrep veces
Nrep = 1000
## Para A: guardar b1
betas_1 = rep(NA, Nrep)

## Para B
cubrimiento_b1 = rep(NA, Nrep)

## Para C
alphas_1 = rep(NA, Nrep)

## Para D
cubrimiento_a1 = rep(NA, Nrep)

set.seed(3487)
x1 <- round(rnorm(n, mu_x, sqrt(var_x)), 2)
x2 <- round(rnorm(n, mu_x, sqrt(var_x)), 2)

for (i in 1:Nrep){
    # Calculo modelo
    err <- rnorm(n)
    y <- b0 + b1*x1 + b2*x2 + b12*x1*x2 + err
    
    model_betas <- lm(y~x1*x2)
    model_alphas <- lm(y~x1+x2)
    
    # Para A
    betas_1[i]<-model_betas$coefficients[2]
    
    # Para C
    alphas_1[i]<-model_alphas$coefficients[2]
    
    # Para B
    CI_beta <- confint(model_betas, level = .9)
    cubrimiento_b1[i] <- between(b1, CI_beta[2,1], CI_beta[2, 2])
    
    # Para D
    CI_alpha <- confint(model_alphas, level = .9)
    cubrimiento_a1[i] <- between(b1, CI_alpha[2,1], CI_alpha[2, 2])
}

### 2.2.a ####

boxplot(betas_1)
# El boxplot me indica que el estimador de b1 con el modelo con interacción 
#tiene la mediana en -3 (el valor real) y varía entre aprox -3.2 y -2.8.
#Para ver que el estimador es insesgado podemos calcular el bias como E(beta 1)-b1
Bias <- mean(betas_1) - b1
Bias
# El bias está muy cerca de cero, empíricamente podemos decir que es invariante, 
# pero también lo probamos teóricamente en la clase.
### 2.2.b ####
# Este es el modelo que más se parece a la generación de los datos, uno esperaría
# que los beta1 caigan dentro del intervalo de confianza
sum(cubrimiento_b1)
mean(cubrimiento_b1)
# Así es, el cubrimiento es muy bueno, de alrededor del 90%

### 2.2.c ####
# Por otro lado, en el modelo sin interacción no esperaría que el valor real 
# caiga muchas veces dentro del  intervalo
sum(cubrimiento_a1)
mean(cubrimiento_a1)
# No cae nunca
### 2.2.d ####
# el desvío estandar de los coeficientes que acompanian a X1 en cada modelo son:
sd(betas_1) # = 0.05546
sd(alphas_1) # = 0.02868
