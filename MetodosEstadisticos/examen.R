
seed<-171

estimar_p_intervalo <- function(nivel, datos, p, metodo){
    alpha <- 1-nivel
    mu <- mean(datos)
    z <- qnorm(1-alpha/2)
    n <- length(datos)

    var <- mu*(1-mu)

    if (metodo == 1) {
        ic <- c(mu-z*var/n, mu+z*var/n) 
    } else {
        a <- n^2 + n*Z^2
        b <- -2*n*sum(x) - Z^2*n
        c <- (sum(x))^2
        ic <- c((-b - sqrt(b^2-4*a*c)) / (2*a), 
                (-b + sqrt(b^2-4*a*c)) / (2*a)) 
    }
    return(ic)
}

calcular_cubrimiento_empirico <- function (p, nivel, n, Nrep, metodo, seed=3487){
    cubrimiento <- numeric(length(Nrep))
    set.seed(seed)
    for (i in 1:Nrep){
        datos <- rbinom(n=n, p=p, size=1)
        intervalo <- estimar_p_intervalo(nivel, datos, p, metodo)
        cubrimiento[i] <- (p>intervalo[1]) & (p<intervalo[2])
    }
    return(mean(cubrimiento))
}

