#Ejercicio 9)
# Vamos a hacer las cuentas en R para comprobar las propiedades que sugiere
#el ejercicio teórico

install.packages('expm')
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

## Bonus Track: Ejercicio 10
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

