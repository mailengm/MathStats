rm(list=ls())

library(MASS)


alfa_MO_gamma<-function(datos){
  estimacion<- (mean(datos))^2/(mean(datos^2)-(mean(datos))^2)
  return(estimacion)
}

lambda_MO_gamma<-function(datos){
  estimacion<- mean(datos)/(mean(datos^2)-(mean(datos))^2)
  return(estimacion)
}

Illinois60<-read.table("illinois60.txt")
Illinois61<-read.table("illinois61.txt")
Illinois62<-read.table("illinois62.txt")
Illinois63<-read.table("illinois63.txt")
Illinois64<-read.table("illinois64.txt")

datos<-rbind(Illinois60,Illinois61,Illinois62,Illinois63,Illinois64)
data<-as.numeric(datos$V1)


emo_alfa<-alfa_MO_gamma(data)
emo_lambda<-lambda_MO_gamma(data)
emv_gamma<-fitdistr(data, densfun = "gamma")

B <-1000
n<-length(data)
est_alfa_mv <- est_lambda_mv <- 0
est_alfa_mo <- est_lambda_mo <- 0

for(i in 1:B){
  lluviasboot_mv <-
    rgamma(n, emv_gamma$estimate[1],
           emv_gamma$estimate[2])
  lluviasboot_mo <-
    rgamma(n, emo_alfa,
           emo_lambda)
  emvboot <- fitdistr(lluviasboot_mv, densfun = "gamma")
  est_alfa_mo[i] <- alfa_MO_gamma(lluviasboot_mo)
  est_lambda_mo[i] <- lambda_MO_gamma(lluviasboot_mo)
  est_alfa_mv[i] <- emvboot$estimate[1]
  est_lambda_mv[i] <- emvboot$estimate[2]
}

bootalfa_mv <- sqrt(var(est_alfa_mv))
bootlambda_mv <- sqrt(var(est_lambda_mv))

bootalfa_mo <- sqrt(var(est_alfa_mo))
bootlambda_mo <- sqrt(var(est_lambda_mo))

eff_alfa<-var(est_alfa_mv)/var(est_alfa_mo)
eff_lambda<-var(est_lambda_mv)/var(est_lambda_mo)

