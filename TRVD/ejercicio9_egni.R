#Ejercicio 9)
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
library(MASS)
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
b$values

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

t(A)%*%sigma_b%*%A

t(A)%*%sigma_w%*%A

t(A)%*%(sigma_w+sigma_b)%*%A


#realizo las simulaciones para probar la version estimada de lo hecho arriba:
## simulación:

library(MASS)

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

data <- rbind(z_1, z_2)
zz <- data.frame(x=data[,1], y=data[,2], grupo=rep(c("G=1", "G=2"), c(nrow(z_1), nrow(z_2))))

library(ggplot2)
scatter_plot<-ggplot(zz, aes(x = x, y = y, color = grupo)) +
  geom_point() +
  labs(x = "z1", y = "z2", title = "Dispersión de las variables transformadas") +
  scale_color_manual(values = c("G=1" = "#EE349c", "G=2" = "#07004D"))
scatter_plot
ggsave("scatter_plot_trans.pdf", plot = scatter_plot, width = 8, height = 3)
