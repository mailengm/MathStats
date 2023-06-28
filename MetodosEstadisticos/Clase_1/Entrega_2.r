## Entrega 2

datos <- scan()
n <- length(datos)
## 1. Estimar mu

mu <- mean(datos)
mu
## 2. Estimar sigma2

sigma2 <- mean((datos-mu)^2)
sigma2

## 3. sigma 2 insesgado

S2 <- (1/(n-1))*sum((datos-mu)^2)
S2

## 4 SD
sigma_verdadero = 2
SE <- sqrt(sigma_verdadero/n)
SE

## 5. sesgo de sigma2

B_sigma2 <- -sigma_verdadero/n
B_sigma2

## 6. sesgo de s2

B_S2 <-0


## 7. SE mu

se_mu <- sqrt(S2/n)
se_mu
