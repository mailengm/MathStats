# Práctica 4
library(ggplot2)
library(dplyr)
## Ej 1

### A. Generar 10 datos

b0 = 5
b1 = 1
b2 = 3

n=10

set.seed(3487)
err <- rnorm(n)

x1 <- round(runif(n, 0, 10), 2)
x2 <- round(runif(n, 0, 10), 2)

y <- b0 + b1*x1 + b2*x2 + err

model <- lm(y~x1+x2)
summary(model)

### B.

Nrep = 1000

b1_hat <- rep(NA, Nrep)
b2_hat <- rep(NA, Nrep)
sd_b1 <- rep(NA, Nrep)
sd_b2 <- rep(NA, Nrep)

for (i in 1:Nrep){
    err <- rnorm(n)
    y <- b0 + b1*x1 + b2*x2 + err
    model <- lm(y~x1+x2)
    
    b1_hat[i] <- model$coefficients[2]
    b2_hat[i] <- model$coefficients[3]
    sd_b1[i] <- summary(model)$coefficients[2,2]
    sd_b2[i] <- summary(model)$coefficients[3,2]
}

### C. Histograma para b1

hist(b1_hat)

### D. Valores verdaderos de E(b1_hat) y V(b1_hat)

E_b1_hat <- mean(b1_hat)
E_b1_hat

X <- model.matrix(model)

V_b1_hat <- solve(t(X)%*%X)
V_b1_hat

### E. Estadistico T

t <- (b1_hat - b1)/sd_b1

### F. Histograma de t 

df <- data.frame(t = t)

ggplot(data = df, aes(x = t)) +
    geom_histogram(aes(y = ..density..),binwidth = .1, color = "steelblue", fill = "deepskyblue3", ) +
    stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(1)), color = "red") + ## G. agregar la nromal
    stat_function(fun = dt, n = 101, args = list(df = n-2), color = "blue") + ## H. Agregar la t de student n-3
    labs(title = "Histogram of t",
         x = "Values of t",
         y = "Frequency")

### I. J. Intervalo de convianza 90%

cubrimiento_b1 = rep(NA, Nrep)
cubrimiento_b2 = rep(NA, Nrep)

set.seed(3487)
for (i in 1:Nrep){
    # Calculo modelo
    err <- rnorm(n)
    y <- b0 + b1*x1 + b2*x2 + err
    model <- lm(y~x1+x2)
    
    CI <- confint(model, level = .9)
    
    cubrimiento_b1[i] <- between(b1, CI[2,1], CI[2, 2])
    cubrimiento_b2[i] <- between(b2, CI[3,1], CI[3, 2])
}

# I: proporción de veces que b1 cae en el intervalo
sum(cubrimiento_b1)/Nrep

# J: proporcion de veces que b2 cae en el intervalo
sum(cubrimiento_b2)/Nrep

### K. Cantidad de veces que ambos parámetros caen en el IC
sum(cubrimiento_b1&cubrimiento_b2)/Nrep

## Ejercicio 2

### A. Generar 150 datos

b0 = 5
b1 = 1
b2 = 3

n=150

set.seed(3487)
err <- rnorm(n)

x1 <- round(runif(n, 0, 10), 2)
x2 <- round(runif(n, 0, 10), 2)

y <- b0 + b1*x1 + b2*x2 + err

model <- lm(y~x1+x2)
summary(model)

### B.

Nrep = 1000

b1_hat <- rep(NA, Nrep)
b2_hat <- rep(NA, Nrep)
sd_b1 <- rep(NA, Nrep)
sd_b2 <- rep(NA, Nrep)

for (i in 1:Nrep){
    err <- rnorm(n)
    y <- b0 + b1*x1 + b2*x2 + err
    model <- lm(y~x1+x2)
    
    b1_hat[i] <- model$coefficients[2]
    b2_hat[i] <- model$coefficients[3]
    sd_b1[i] <- summary(model)$coefficients[2,2]
    sd_b2[i] <- summary(model)$coefficients[3,2]
}

### C. Histograma para b1

hist(b1_hat)

### D. Valores verdaderos de E(b1_hat) y V(b1_hat)

E_b1_hat <- mean(b1_hat)
E_b1_hat

X <- model.matrix(model)

V_b1_hat <- solve(t(X)%*%X)
V_b1_hat

### E. Estadistico T

t <- (b1_hat - b1)/sd_b1

### F. Histograma de t 

df <- data.frame(t = t)

ggplot(data = df, aes(x = t)) +
    geom_histogram(aes(y = ..density..),binwidth = .1, color = "steelblue", fill = "deepskyblue3", ) +
    stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(1)), color = "red") + ## G. agregar la nromal
    stat_function(fun = dt, n = 101, args = list(df = n-2), color = "blue") + ## H. Agregar la t de student n-3
    labs(title = "Histogram of t",
         x = "Values of t",
         y = "Frequency")

### I. J. Intervalo de convianza 90%

cubrimiento_b1 = rep(NA, Nrep)
cubrimiento_b2 = rep(NA, Nrep)

set.seed(3487)
for (i in 1:Nrep){
    # Calculo modelo
    err <- rnorm(n)
    y <- b0 + b1*x1 + b2*x2 + err
    model <- lm(y~x1+x2)
    
    CI <- confint(model, level = .9)
    
    cubrimiento_b1[i] <- between(b1, CI[2,1], CI[2, 2])
    cubrimiento_b2[i] <- between(b2, CI[3,1], CI[3, 2])
}

# I: proporción de veces que b1 cae en el intervalo
sum(cubrimiento_b1)/Nrep

# J: proporcion de veces que b2 cae en el intervalo
sum(cubrimiento_b2)/Nrep

### K. Cantidad de veces que ambos parámetros caen en el IC
sum(cubrimiento_b1&cubrimiento_b2)/Nrep

## Ejercicio 3

### A. Generar 150 datos

b0 = 5
b1 = 1
b2 = 3

n=150

set.seed(3487)
err <- rexp(n) -1
mean(err)

x1 <- round(runif(n, 0, 10), 2)
x2 <- round(runif(n, 0, 10), 2)

y <- b0 + b1*x1 + b2*x2 + err

model <- lm(y~x1+x2)
summary(model)

### B.

Nrep = 1000

b1_hat <- rep(NA, Nrep)
b2_hat <- rep(NA, Nrep)
sd_b1 <- rep(NA, Nrep)
sd_b2 <- rep(NA, Nrep)

for (i in 1:Nrep){
    err <- rnorm(n)
    y <- b0 + b1*x1 + b2*x2 + err
    model <- lm(y~x1+x2)
    
    b1_hat[i] <- model$coefficients[2]
    b2_hat[i] <- model$coefficients[3]
    sd_b1[i] <- summary(model)$coefficients[2,2]
    sd_b2[i] <- summary(model)$coefficients[3,2]
}

### C. Histograma para b1

hist(b1_hat)

### D. Valores verdaderos de E(b1_hat) y V(b1_hat)

E_b1_hat <- mean(b1_hat)
E_b1_hat

X <- model.matrix(model)

V_b1_hat <- solve(t(X)%*%X)
V_b1_hat

### E. Estadistico T

t <- (b1_hat - b1)/sd_b1

### F. Histograma de t 

df <- data.frame(t = t)

ggplot(data = df, aes(x = t)) +
    geom_histogram(aes(y = ..density..),binwidth = .1, color = "steelblue", fill = "deepskyblue3", ) +
    stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = sqrt(1)), color = "red") + ## G. agregar la nromal
    stat_function(fun = dt, n = 101, args = list(df = n-2), color = "blue") + ## H. Agregar la t de student n-3
    labs(title = "Histogram of t",
         x = "Values of t",
         y = "Frequency")

### I. J. Intervalo de convianza 90%

cubrimiento_b1 = rep(NA, Nrep)
cubrimiento_b2 = rep(NA, Nrep)

set.seed(3487)
for (i in 1:Nrep){
    # Calculo modelo
    err <- rnorm(n)
    y <- b0 + b1*x1 + b2*x2 + err
    model <- lm(y~x1+x2)
    
    CI <- confint(model, level = .9)
    
    cubrimiento_b1[i] <- between(b1, CI[2,1], CI[2, 2])
    cubrimiento_b2[i] <- between(b2, CI[3,1], CI[3, 2])
}

# I: proporción de veces que b1 cae en el intervalo
sum(cubrimiento_b1)/Nrep

# J: proporcion de veces que b2 cae en el intervalo
sum(cubrimiento_b2)/Nrep

### K. Cantidad de veces que ambos parámetros caen en el IC
sum(cubrimiento_b1&cubrimiento_b2)/Nrep

## Ejercicio 4

## Derivar un test de igualdad de medias y aplicarlo al dataset paralel

paralel <- read.csv("paralel.txt")
names(paralel)
paralel <- as.data.frame(paralel)

n1 <- length(paralel$x1)
n2 <- length(paralel$x2)

y <- c(paralel$y1, paralel$y2)

# fabricamos la matriz de diseño a mano
X <- matrix(0, n1+n2, 2)
X[1:n1,1] <- rep(1, n1)
X[(n1+1):(n1+n2), 2] <- rep(1, n2)

ajuste <- lm(y~X-1) ## No queremos ajustar con intercept
summary(ajuste)

p <- dim(X)[2]
n <- length(y)

s2 <- sum(ajuste$residuals^2)/(n-p)
#s2 <- summary(ajuste)$sigma^2 

# Las hipotesis son H0: mu1=mu2
# Esto también se puede escribir como:

a <- c(1,-1)

#construimos el estadistico del test en este caso
TE <- (t(a)%*%(ajuste$coefficients))/sqrt(s2*t(a)%*%solve(t(X)%*%X)%*%a) 

pvalor <- 2*pt(TE, df=n-p,lower.tail = FALSE)

## Funcion que sirve para testear H_0: a^T beta = c versus H_0: a^T beta != c
test.cl.beta <- function(X,y,a,c)
{
    ajuste <- lm(y~X-1)
    summary(ajuste)
    p <- dim(X)[2]
    n <- length(y)
    s2 <- sum(ajuste$residuals^2)/(n-p)
    
    TE <- (t(a)%*%(ajuste$coefficients)-c)/sqrt(s2*t(a)%*%solve(t(X)%*%X)%*%a)
    
    pvalor <- 2*pt(abs(TE), df=n-p,lower.tail = FALSE)
    pvalor
    
}
test.cl.beta(X, y, c(1,-1), c=0)
t.test(paralel$y1, paralel$y2, var.equal = TRUE)

## Ejercicio 5

### E. obtener los estimadores de minimos cuadrados
paralel <- read.csv("paralel.txt")

X<-matrix(c(rep(1, 20), rep(0,20), paralel$x1, rep(0,20),
         rep(0, 20), rep(1,20), rep(0,20), paralel$x2), ncol=4)

Y <- matrix(c(paralel$y1, paralel$y2), 
            ncol = 1)
model <- lm(Y~X)


