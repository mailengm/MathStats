# Práctica 3

## Ej 3

library(genridge)

data <- prostate

### A. Ajustar un modelo lineal para explicar la variable lpsa

model <- lm(lpsa~lcavol+lweight, x=T, data=data)


### B. Verificar que vale 1b.

X <- model$x[,2:3]

XtX <- t(X)%*%X
XtX

xxt <- matrix(0, nrow=2, ncol=2)

filtered_data <- as.matrix(data[c("lcavol", "lweight")])

for (i in 1:nrow(data)){
    xxti <- filtered_data[i,] %*% t(filtered_data[i,])
    xxt <- xxt + xxti
}

xxt
#Son iguales

### C. Hallar Y_pred y calcular la correlación al cuadrado. Comparar con summary

Y_pred <- model$fitted.values

correlation<-cor(Y_pred, data$lpsa)
correlation^2

summary(model)

# La correlación al cuadrado es el R2

### D. Vector de residuos. Calcular el promedio y ver que da 0

mean(model$residuals)

### E. Interpretar el modelo

"""
El logaritmo del antígeno prostático depende del logaritmo del volumen del 
cancer y del logaritmo del peso de la próstata de manera lineal. Al aumentar una
unidad el log del volumen del cancer aumenta el antígeno en 0.65 unidades, y al 
aumentar 1 unidad el peso de la próstata aumenta un 0.66 el nivel de antígeno.
"""