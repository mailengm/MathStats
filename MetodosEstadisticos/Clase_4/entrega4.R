

datos <- scan()

## A IC para mu con sigma2 desconocido

nivel = 0.9

alpha <- 1-nivel

mu_hat <- mean(datos)

S2 <- var(datos)

n <- length(datos)

z <- qt(1-alpha/2, df= n-1) 

IC_L <- mu_hat - z * sqrt(S2/n)
print(IC_L)

IC_R <- mu_hat + z * sqrt(S2/n)
print(IC_R)

## B IC para sigma con mu desconocido.

ICs_L <- (n-1)*S2/qchisq(1-alpha/2, df=n-1)
print(ICs_L)
ICs_R <- (n-1)*S2/qchisq(alpha/2, df=n-1)
print(ICs_R)


# Entrega 3

datos <- scan()
nivel = 0.94

suma <- sum(datos)
n <-length(datos)

alpha <- 1-nivel

IC_L <- qchisq(alpha/2, df=2*n)/(2*suma)
IC_R <- qchisq(1-alpha/2, df=2*n)/(2*suma)

print(c(IC_L, IC_R))
