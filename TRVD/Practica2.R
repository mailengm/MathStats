# Práctica 2
library('tidyverse')
library('MASS')
library('expm')
library("dplyr")
library("ggplot2")
library("factoextra")
setwd("/home/mailen/Documents/MEM/TRVD/")
# Componentes Principales ####

# PCA 2.1 ####

# Datos
Sigma <- matrix(c(3,1,1,1,3,1,1,1,5), nrow=3)
Sigma

# Busco la descomposición
eig <- eigen(Sigma)

U <- eig$vectors
U

L <- diag(eig$values)
L

# Comparo la transformación
U%*%L%*%t(U)


# PCA 2.2 ####

## 2.D ####



# Simulo los datos
n = 1000
Sigma <- matrix(c(3,1,1,1,3,1,1,1,5), ncol = 3)
mu <- matrix(c(0,0,0))

datos <- mvrnorm(n=n, mu=mu, Sigma=Sigma)

# Los estandarizo
sigma_sqrt <- sqrtm(Sigma)

z <- sweep(datos,2,mu) %*% solve(sigma_sqrt) 

# Los ploteo
pairs(datos)
pairs(z)

# PCA 2.3 ####


## 2.3.A #### 
# Centrar los datos
datos <- read.csv("Datos/constructora.txt", sep=" ")

datos_centrados <- datos %>% 
                    mutate(x1=x1-mean(x1), x2=x2-mean(x2), x3=x3-mean(x3))

# Grafico el scatterplot de pares

pairs(datos_centrados)

## 2.3.B ####
# PCA

Sigma <- cov(as.matrix(datos_centrados[,2:4]))
A <- eigen(Sigma)$vectors[,1:2]
lambda <- eigen(Sigma)$values

componentes <- as.matrix(datos_centrados[2:4]) %*% A

## 2.3.C ####
componentes <- as_tibble(componentes) %>%
    rename(p1=V1, p2=V2)

componentes %>% ggplot(aes(x=p1, y=p2)) +
    geom_point(color="black") +
    theme(aspect.ratio=1)

pairs(componentes)

## 2.3.D ####

S_comp <- cov(componentes)
S_comp


S_comp_3 <- cov(as.matrix(datos_centrados[2:4]) %*% eigen(Sigma)$vectors)
S_comp_3

## 2.3.E ####

A <- eigen(Sigma)
A_cor <- eigen(cor(as.matrix(datos_centrados[,2:4])))

# PCA 2.4 ####

datos4 <- read.csv("Datos/paises_mundo.csv")

## 2.4.1 ####

pairs(datos4[2:6])

## 2.4.2 ####
pca4 <- prcomp(as.matrix(datos4[2:6]), scale = F)
pca4
summary(pca4)

#Busco el score = t(v)(X-mu)
x_mu <- scale(datos4[2:6], center = TRUE, scale = FALSE)

scores <- x_mu %*% t(pca4$rotation)
plot(scores)
plot(pca4$x)

fviz_pca_biplot(pca4,
                label = "var",
                col.ind = "black",
                size.ind = .1)

## 2.4.3 ####
# Proporción  de variabilidad total

prop_var_acumulada <- sum(pca4$sdev[1:2]^2) / sum(pca4$sdev^2)
prop_var_acumulada

## 2.4.4 ####
# Heatmap de la correlación muestral entre los scores y los datos

heatmap(cor(x_mu, scores),  Colv = NA, Rowv = NA, scale="column")

## PCA 2.5 ####

## Ejercicio 5
data <- read.csv("vinos.csv")

x <- as.matrix(scale(data))

## BIPLOT
# Calculo las matrices
S <- cov(x)

eig <- eigen(S)
# Me quedo con los dos primeros
U <- eig$vectors[,1:2]

L <- diag(eig$values[1:2])

G <- x %*% U %*% sqrt(solve(L))
plot(G, pch=20)


H <- U %*% sqrt(L)

x_star <- G%*%t(H)


plot(G, pch=20, col='grey60')
arrows(rep(0,4), rep(0,4), H[,1], H[,2], length=0.1)
text(H, labels=colnames(data), pos=2)

## Si no multiplico por L tengo el biplot de PCA


# PCA 2.6 ####

# Correlaciones Canónicas ####

## CC 2.7 ####
S11 <- matrix(c(8,2,2,5), nrow=2)
S11

S22 <- matrix(c(6,-2,-2,7), nrow=2)
S22

S12 <- matrix(c(3,-1,1,3), nrow=2)
S12

S21 <- t(S12)


### 2.7.A ####
U <- solve(S11) %*% S12 %*% solve(S22) %*% S21

# Alpha 1 es el autovector de autovalor máximo, entonces

eig <- eigen(U)
alpha_1 <- eig$vectors[,1]

# Chequeo que se cumpla que alphaT Sigma11 alpha =1

t(alpha_1) %*% S11 %*% alpha_1 # No es 1, entonces tengo que escalar alpha

vec_1 <- eig$vectors[, 1]

k <- 1/sqrt(t(vec_1) %*% S11 %*% vec_1)

alpha_1 <- as.numeric(k) * vec_1 # y si corro la linea de arriba cumple la condición 

# Beta 1 es una escala de alpha 1
lambda1 <- eig$values[1]

beta_1_raw <- as.numeric(1/lambda1) * solve(S22) %*% S21 %*% alpha_1
kb <- 1/sqrt(t(beta_1_raw) %*% S22 %*% beta_1_raw)

beta_1 <- as.numeric(kb) * beta_1_raw

t(beta_1) %*% S22 %*% beta_1

## Calcular var(U1)

var_u1 <- t(alpha_1) %*% S11 %*% alpha_1
var_u1

var_v1 <- t(beta_1) %*% S22 %*% beta_1
var_v1

var_u1v1 <- t(alpha_1) %*% S12 %*% beta_1
var_u1v1

# la covarianza entre u1 y v1 es la raiz cuadrada del primer ava
sqrt(eig$values[1])

### 2.7.B ####
# Repito lo mismo para i=2

alpha_2_raw <- eig$vectors[,2]
ka2 <- 1/sqrt(t(alpha_2_raw)%*%S11)

alpha_2 <- as.numeric(ka2) * alpha_2_raw
alpha_2

beta_2_raw <- 1/eig$values[2] *solve(S22) %*% S21 %*% alpha_2
kb2 <- 1/sqrt(t(beta_2_raw)%*%S22%*%beta_2_raw)

beta_2 <- as.numeric(kb2) * beta_2_raw
beta_2

t(beta_2) %*% S22 %*% beta_2

### 2.7.C ####

u <- cbind(alpha_1, alpha_2)
v <- cbind(beta_1, beta_2)



## CC 2.8 ####


# Coordenadas Discriminantes ####
## CD 2.9 ####
# Vamos a hacer las cuentas en R para comprobar las propiedades que sugiere
#el ejercicio teórico

library(expm)

#Definimos las matrices, vectores y magnitudes del problema
sigma_w = matrix(c(1,1/2,1/2,1),ncol = 2, nrow = 2)

mu_1 = c(1,1/2)
mu_2 = c(-1/2,1)

pi_1 = 3/4
pi_2 = 1/4

mu_x = pi_1*mu_1 + pi_2*mu_2

sigma_b = pi_1*((mu_1 - mu_x) %*% t(mu_1 - mu_x)) + pi_2*((mu_2 - mu_x) %*% t(mu_2 - mu_x))

#definimos como la matriz C triangular tal que t(C) %*% C recupere sigma_w:
C = sqrtm(sigma_w)
#compruebo:
t(C) %*% C
#me da igual que sigma_w!

#calculo los autovalores y autovectores de B C^(-1) simga_b C que me define la
#magnitud F que quiero maximizar y adopta los valores maximos en sus autovectores:
B = t(solve(C)) %*% sigma_b %*% solve(C)

#calculo ave y ava:
b = eigen(B)

#a continuación defino la matriz A que esta compuesta por los alpha definidos 
#como C^(-1)*beta_j con beta_j autovector de B:
A_aux = b$vectors

A = solve(C) %*% A_aux 
A

#calculo las cordenadas de los centroides según definicion:
#centroides
v_1 = t(A)%*%mu_1
v_2 = t(A)%*%mu_2

#la ditancia euclidea entre los centroides transformados al espacio de coordenadas
#discriminantes está dado por la norma 2 euclidea:

#distancia euclidea
norm(v_1 - v_2, "2")

#comparo esta magnitud con la distancia de los centroides en el espacio
#original pero calculada con la métrica definida por las covarianzas
#de las variables aleatorias originales:

#distancia mahalanobis
sqrt(t(mu_1-mu_2)%*%solve(sigma_w)%*%(mu_1 - mu_2))

#me da exactamente lo mismo!!

#realizo las simulaciones para probar la version estimada de lo hecho arriba:
## simulación:



n_1 = rbinom(1,500,3/4)
n_2 = 500 - n_1
x_group_1 = mvrnorm(n_1, mu = mu_1, Sigma = sigma_w)
x_group_2 = mvrnorm(n_2, mu = mu_2, Sigma = sigma_w)
plot(x_group_1, col = 'red')
points(x_group_2, col = 'green')


x_1_cent = sweep(x_group_1,2,mu_x)
x_2_cent = sweep(x_group_2,2,mu_x)
z_1 =t( t(A)%*%t(x_1_cent)) #el quilombo de transpuestas es culpa de R

z_2 =t( t(A)%*%t(x_2_cent)) #el quilombo de transpuestas es culpa de R

plot(z_1, col = 'red')
points(z_2, col = 'green')
#se observa que la varianza esta "alineada" horizontalmente. Esto ocurre porque
#el segundo autovalor es 0, indicándonos que toda la varianza se explica a partir
#de la primer coordenada discriminante

## CD 2.10 ####
#Vamos primero a implementar las funciones que calculan las matrices H y U, 
#"proxys" de sigma_w y sigma_b a menos de una normalización.

#lo hacemos "a mano" haciendo literalmente las sumas en gurpos y muestras
#para comprobar que esté bien:

#metodo manual
U = matrix(0,nrow = 4, ncol = 4)
for (level in unique(data$Species)){
    x_i = data[data$Species ==level,1:4]
    x_i_raya = colMeans(x_i)
    x_new = sweep(x_i,2,x_i_raya)
    U_aux = matrix(0,nrow = ncol(x_i), ncol = ncol(x_i))
    n_k = nrow(x_new)
    for(i in 1:n_k){
        matrix_aux = data.matrix(x_new[i,])
        element_ = t(matrix_aux) %*% (matrix_aux)
        U_aux = U_aux + element_
    }
    print(U_aux)
    print(c('Termino el nivel: ',level))
    U = U + U_aux
    
}
U

#ahora hacemos una función un poco más compacta que hace lo mismo:

ss.split = function(data,grouping){
    U = matrix(0,nrow = ncol(data), ncol = ncol(data))
    H = matrix(0,nrow = ncol(data), ncol = ncol(data))
    data = data.matrix(data)
    x_raya =  colMeans(data)
    for (level in unique(grouping)){
        x_i = data[grouping == level,]
        x_i_raya = colMeans(x_i)
        x_new = sweep(x_i,2,x_i_raya)
        U_aux = t(x_new) %*% x_new
        print(c('Termino el nivel:',level))
        U = U + U_aux
        n_k = nrow(x_i)
        
        H_aux = n_k * (x_i_raya - x_raya) %*% t(x_i_raya - x_raya)
        
        H = H + H_aux
    }
    return(list(U,H,U+H))
}

#probemos nuestra función en algunas simulaciones!

#dado que las simulaciones se comparan en el contexto del ejercicio anterior, 
#defino las mismas magnitudes y objetos:

library(MASS)

sigma_w = matrix(c(1,1/2,1/2,1),ncol = 2, nrow = 2)

mu_1 = c(1,1/2)
mu_2 = c(-1/2,1)

pi_1 = 3/4
pi_2 = 1/4

mu_x = pi_1*mu_1 + pi_2*mu_2
sigma_b = pi_1*((mu_1 - mu_x) %*% t(mu_1 - mu_x)) + pi_2*((mu_2 - mu_x) %*% t(mu_2 - mu_x))


n_1 = 150
n_2 = 50
x_group_1 = mvrnorm(n_1, mu = mu_1, Sigma = sigma_w)
x_group_2 = mvrnorm(n_2, mu = mu_2, Sigma = sigma_w)
X = rbind(x_group_1,x_group_2)


grupos = c()
for(i in 1:200){
    if(i>150){
        grupos[i] = 'g_1'
    }
    else {
        grupos[i] = 'g_2'
    }
}
grupos = factor(grupos)
matrices = ss.split(X,grupos)

n = 200
k = 2

#defino las matrices sigma_within y between a partir de el output de ss.split
#y agregando el factor de escala dado, en el caso bewteen, por la diferencia
#entre cantidad de muetras y cantidad de grupos y, en el caso between, por la cantidad
#de muestras dado que H se calculo usando el tamaño del grupo y no las probabilidades
#prioir. Si fuera calculado a partir de las prioir, no hace falta esta normalizacion

sigma_w_est = matrix(unlist(matrices[1]), ncol = 2) / (n-k)
mean(abs(sigma_w_est - sigma_w))

#vemos que da muy parecida!


sigma_b_est =  matrix(unlist(matrices[2]), ncol = 2) / (n - 1)
mean(abs(sigma_b_est - sigma_b))

#también parece estar bien estimada!


#calculamos los centroides estimados con los datos:
mu_1_est = colMeans(x_group_1)
mu_2_est = colMeans(x_group_2)

mu_x_est = 3/4*mu_1_est + 1/4*mu_2_est

#Por ultimo, calculamos los alfa. En este caso no lo hacemos igual al ejercicio
# anterior porque no tenemos interes en obtener alphas no normalizados para hacer
#coincidir la distancia de mahalanobis con la euclidea. En este sentido, directamente
#calculamos los alpha con la condicion que si deben cumplir: ser autovectores
#de sigma_w^(-1) * sigma_b:


b =  eigen(solve(sigma_w)%*%sigma_b)
b_est = eigen(solve(sigma_w_est)%*%sigma_b_est)

A_est = b_est$vectors

A = b$vectors


#centroides muestrales de las coord disc:
v_1_est = t(A_est)%*%mu_1_est
v_2_est = t(A_est)%*%mu_2_est

v_1 = t(A)%*%mu_1
v_2 = t(A)%*%mu_2

v_1_est
v_2_est
v_1
v_2

#observo que se parecen mucho!

#por ultimo defino una nueva funcion ss.split que agrega la posibilidad
#de, en caso de conocerlas, calcular sigma_b a partir de la probabilidad
#prior de pertenecer a un dado grupo G_i:

ss.split_2 = function(data,grouping, priors = NULL){
    U = matrix(0,nrow = ncol(data), ncol = ncol(data))
    H = matrix(0,nrow = ncol(data), ncol = ncol(data))
    data = data.matrix(data)
    x_raya =  colMeans(data)
    contador = 0
    for (level in unique(grouping)){
        contador = contador + 1
        x_i = data[grouping == level,]
        x_i_raya = colMeans(x_i)
        x_new = sweep(x_i,2,x_i_raya)
        U_aux = t(x_new) %*% x_new
        print(c('Termino el nivel:',level))
        U = U + U_aux
        n_k = nrow(x_i)
        
        if(is.null(priors)){
            H_aux = n_k * (x_i_raya - x_raya) %*% t(x_i_raya - x_raya)
        }
        else {
            H_aux = priors[contador] * (x_i_raya - x_raya) %*% t(x_i_raya - x_raya)
        }
        
        
        H = H + H_aux
    }
    return(list(U,H,U+H))
}

## CD 2.11 ####

cocos <- read.csv("Datos/cocodrilos.csv")

## 2.11.A ####

Xcocos <- cocos[,1:11]
especies <- cocos[,12]

Xcocos <- scale(Xcocos)

grupos <- as.factor(especies)
mats <- ss.split(Xcocos, grupos)
 
n = nrow(Xcocos) 
k = 4

Sw = matrix(unlist(mats[1]), ncol = 11) / (n-k)
Sb <- matrix(unlist(mats[2]), ncol = 11)/ n-1

eig.cocos <- eigen(solve(Sw)%*% Sb)

Xcd <- Xcocos%*% eig.cocos$vectors[,1:3]

library("rgl")
options(rgl.printRglwidget = TRUE)
plot3d(Xcd, 
       col = especies, 
       cube=T, 
       size = 10)

### 2.11.c ####
# Lo mismo con PCA



sigma_cocos <- matrix(unlist(mats[3]), ncol = 11)/(n-1)
PCA <- eigen(var(sigma_cocos))
rot <- PCA$vectors[,1:3]

Xpca <- Xcocos%*%rot

library("scatterplot3d")
scatterplot3d(Xcd, color = especies)
scatterplot3d(Xpca, color = especies)

## CD 2.12 ####
olmos <- read.csv("Datos/olmos.csv")
Xolmos <- scale(olmos[1:7])
cats <- olmos
n = nrow(unique(cats))
k = nrow(Xolmos)

mats <- ss.split(Xolmos, cats)

Sw = matrix(unlist(mats[1]), ncol = 7) / (n-k)
Sb <- matrix(unlist(mats[2]), ncol = 7)/ n-1

eig.olmos <- eigen(solve(Sw)%*% Sb)

Xcd <- Xolmos%*% eig.olmos$vectors[,1:2]

vecs <- eig.olmos$vectors[,1:2]
CD <- as.dataframe(cbind(Xcd, cats))

plot(Xcd[,1], Xcd[,2], col=factor(cats$cat))
arrows(0,0,as.numeric(vecs[1,1]), as.numeric(vecs[1,2]))
arrows(0,0,as.numeric(vecs[2,1]), as.numeric(vecs[2,2]))
arrows(0,0,as.numeric(vecs[3,1]), as.numeric(vecs[3,2]))
arrows(0,0,as.numeric(vecs[4,1]), as.numeric(vecs[4,2]))
arrows(0,0,as.numeric(vecs[5,1]), as.numeric(vecs[5,2]))
arrows(0,0,as.numeric(vecs[6,1]), as.numeric(vecs[6,2]))
arrows(0,0,as.numeric(vecs[7,1]), as.numeric(vecs[7,2]))
## CD 2.13 ####

clasif_fisher <- function(X, grouping){
    n = nrow(unique(grouping))
    k = nrow(X)
    
    mats <- ss.split(X, grouping)
    
    Sw = matrix(unlist(mats[1]), ncol = 7) / (n-k)
    Sb <- matrix(unlist(mats[2]), ncol = 7)/ n-1
    
    eig.cd <- eigen(solve(Sw)%*% Sb)
    
    
}


# Projection Pursuit ####
## PP 2.14 ####

