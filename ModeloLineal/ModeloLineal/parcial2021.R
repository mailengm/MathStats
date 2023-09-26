setwd ("C:/Users/gonza/OneDrive/Documentos/Escritorio/MASTER_UBA/Modelo_lineal")
library("MASS")
df=Boston
# 1
ajuste1=lm(data=df,formula=medv~ zn + indus,x=TRUE)
resumen1=summary(ajuste1)
# estimador de varianza del error
resumen1$sigma^2

# 2
new_val=data.frame( zn=c(1),indus=c(3))
new_val

IC=predict(ajuste1,newdata=new_val,interval="confidence",level=0.90)
IC

# chequiemos los intervalos "a  mano"

xh=matrix(c(1,1,3))
X=ajuste1$x
calc_aux=t(xh) %*% solve(t(X)%*%X) %*% xh
p=ncol(X)
alfa=0.1

lm_inf_conf= t(xh)%*%ajuste1$coefficients - qt(df=nrow(df)-p,p=1-alfa/2)*resumen1$sigma *sqrt(calc_aux)
lm_sup_conf= t(xh)%*%ajuste1$coefficients + qt(df=nrow(df)-p,p=1-alfa/2)*resumen1$sigma *sqrt(calc_aux)  
lm_inf_conf;lm_sup_conf

# 3

n=nrow(df)
n

p=ncol(ajuste1$x)

a=matrix(c(0,10,1),ncol=1)
a

c=0

X=ajuste1$x

s2 <- resumen1$sigma^2

TE <- (t(a)%*%(ajuste1$coefficients)-c)/sqrt(s2*t(a)%*%solve(t(X)%*%X)%*%a)
TE

pvalor <- 2*pt(abs(TE), df=n-p,lower.tail = FALSE)
pvalor

# 4
df$radc=as.factor(ifelse(df$rad<=3,2,
                             ifelse(df$rad<=8,1,
                                    0)))                   
ajuste2=lm(data=df, formula=medv ~ zn + indus + radc, x=TRUE )                                    
resumen2=summary(ajuste2)                                    
resumen2                     

# 6
# testeso signifciancia de radc con est F usando anova:
anova(ajuste1 , ajuste2)

# hacemos el test a mano

# test F para b5=b6=b7
A=matrix(c(0,0,0,1,0,
           0,0,0,0,1),nrow=2,ncol=5,byrow=TRUE)
A

B=matrix(ajuste2$coefficients)
B

C=matrix(c(0,0))
C

q=2

X=ajuste2$x

n=nrow(X)
p=ncol(X)

F = (t(A%*% B - C) %*% solve(A %*% solve(t(X)%*%X) %*% t(A)) %*% (A%*% B - C))/(q*resumen2$sigma^2)
F
pf(q = F,df1 = q,df2 = n-p ,lower.tail = FALSE)


# 8
ajuste3=lm(data=df, formula=medv ~ zn + indus + radc + crim, x=TRUE )                                    
resumen3=summary(ajuste3)                                    
resumen3   

ajuste3_sin_radc = lm(data=df, formula=medv ~ zn + indus  + crim, x=TRUE )                                    
resumen3_sin_radc=summary(ajuste3_sin_radc)                                    
anova(ajuste3_sin_radc,ajuste3) 


A=matrix(c(0,0,0,1,0,0,
           0,0,0,0,1,0),nrow=2,ncol=6,byrow=TRUE)
A

B=matrix(ajuste3$coefficients)
B

C=matrix(c(0,0))
C

q=2

X=ajuste3$x

n=nrow(X)
p=ncol(X)

F = (t(A%*% B - C) %*% solve(A %*% solve(t(X)%*%X) %*% t(A)) %*% (A%*% B - C))/(q*resumen3$sigma^2)
F
pf(q = F,df1 = q,df2 = n-p ,lower.tail = FALSE)

# 10
interaction.plot(df$chas,df$radc,df$medv)

interaction.plot(df$radc,df$chas,df$medv)

# 12

ajuste4=lm(data=df , formula =medv ~ zn + indus + radc*chas, x=TRUE)
resumen4=summary(ajuste4)
resumen4

ajuste4_sin_int=lm(data=df , formula =medv ~ zn + indus + radc + chas, x=TRUE)
resumen4_sin_int=summary(ajuste4_sin_int)
anova(ajuste4_sin_int,ajuste4)


A=matrix(c(0,0,0,0,0,0,1,0,
           0,0,0,0,0,0,0,1),nrow=2,ncol=8,byrow=TRUE)
A

B=matrix(ajuste4$coefficients)
B

C=matrix(c(0,0))
C

q=2

X=ajuste4$x

n=nrow(X)
p=ncol(X)

F = (t(A%*% B - C) %*% solve(A %*% solve(t(X)%*%X) %*% t(A)) %*% (A%*% B - C))/(q*resumen4$sigma^2)
F
pf(q = F,df1 = q,df2 = n-p ,lower.tail = FALSE)
