
data <- read.csv("PBI.csv")

View(data)

# a) poner la data en una matriz
X <- as.matrix(data)

# b) Scatterplots
pairs(X)

# c) calcular el promedio y la matriz de centrado

media_muestral <-colMeans(X)

Id <- diag(rep(1,nrow(X)))
unos <- matrix(1, nrow(X), 1)
# Este procedimiento le resta la media a cada columna
H <- Id -(1/nrow(X)*unos%*%t(unos))

X_centrada <- H%*%X
pairs(datos_centrados)

# d) Calcular Q 
# Q me da una idea de la varianza
Q <- t(X_centrada)%*%X_centrada
Q
# Para que sea la varianza tengo que dividir por n-1
S <- 1/(nrow(X)-1)*Q
S
#Tambien se puede calcular S usando el comando cov
cov(X)

# e) Z

D_12 <- diag(sqrt(1/diag(S)))
D_12
# Con este procedimiento estandarizamos X
Z <- H %*% X %*% D_12
Z

pairs(Z)
# Tambien podemos estandarizar usando scale
pairs(scale(X))
scale(X)

# f)
# cov de Z calcula la correlacion entre las variables
cov(Z)

