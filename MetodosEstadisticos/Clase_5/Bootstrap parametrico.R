# Analizamos los datos del ingreso total familiar en Argentina en 2019
ingresos <- read.table("~/MEM/Métodos estimación/datos_ingresos.txt")
ingresos <- ingresos[,1]
ingresos_pos <- ingresos[ingresos>0]
library(MASS)
emvlnorm <- fitdistr(ingresos_pos, densfun = "lognormal")
emvlnorm

par(mfrow = c(1,2))
hist(ingresos_pos, freq=FALSE, main = "Ingreso total familar")
lines(density(ingresos_pos))
curve(dlnorm(x, meanlog = emvlnorm$estimate[1], sdlog = emvlnorm$estimate[2]), add=TRUE, col = 3)
hist(ingresos_pos, prob = TRUE, main = "Ingreso total familar(zoom)", xlim = c(0,300000), breaks =100)
lines(density(ingresos_pos))
curve(dlnorm(x, meanlog = emvlnorm$estimate[1], sdlog = emvlnorm$estimate[2]), add=TRUE, col = 3)


###Estimamos la varianza y el desvío estandar de los estimadores por el método bootsrap paramétrico asumiendo el modelo lognormal
B <-1000
n<-length(ingresos_pos)
estmu <- estsigma <- 0
for(i in 1:B){
  ingresosboot <-
    rlnorm(n, meanlog = emvlnorm$estimate[1], sdlog = emvlnorm$estimate[2])
  emvboot <- fitdistr(ingresosboot, densfun = "lognormal")
  estmu[i] <- emvboot$estimate[1]
  estsigma[i] <- emvboot$estimate[2]
}

vbootmu <- mean((estmu - mean(estmu))^2)
vbootsigma <- mean((estsigma - mean(estsigma))^2)

vbootmu
vbootsigma
sqrt(vbootmu)
sqrt(vbootsigma)

#Estimación de de la mediana y la media sin asumir ninguna distribución
mean(ingresos_pos)
median(ingresos_pos)


#Estimación de de la mediana y la media asumiendo un modelo lognormal
exp(emvlnorm$estimate[1])

exp(emvlnorm$estimate[1] + emvlnorm$estimate[2]^2/2)


#Estimación del error estandar de la mediana y la media asumiendo un modelo lognormal

B <- 1000
n <- length(ingresos_pos)
estmed_p <- estmed_np <- estmedia_p <- estmedia_np <- 0
for(i in 1:B){
  ingresosboot <-
    rlnorm(n, meanlog = emvlnorm$estimate[1], sdlog = emvlnorm$estimate[2])
  emvboot <- fitdistr(ingresosboot, densfun = "lognormal")
  estmu <- emvboot$estimate[1]
  estsigma <- emvboot$estimate[2]
  estmed_p[i] <- exp(estmu)
  estmed_np[i] <- median(ingresosboot)
  estmedia_p[i] <- exp(estmu + estsigma^2/2)
  estmedia_np[i] <- mean(ingresosboot)
}

sebootmed_p <- sqrt(mean((estmed_p-mean(estmed_p))^2))
sebootmed_np <- sqrt(mean((estmed_np-mean(estmed_np))^2))
sebootmedia_p <-sqrt(mean((estmedia_p-mean(estmedia_p))^2))
sebootmedia_np <- sqrt(mean((estmedia_np-mean(estmedia_np))^2))

sebootmed_p
sebootmed_np
sebootmedia_p
sebootmedia_np

#Eficiencia de los estimadores no paramétricos de la media y la mediana con respecto a MV asumiendo un modelo lognormal
sebootmed_p^2/sebootmed_np^2

sebootmedia_p^2/sebootmedia_np^2

# Estimacion de el error estandar por metodo delta
sqrt(exp(2*emvlnorm$estimate[1])*emvlnorm$estimate[2]^2/n)
sebootmed_p















