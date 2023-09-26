TP Simulacion -Inferencia Estadística
================
2023-06-10

# Trabajo práctico - Simulacion

Una fuente de radiación emite partículas alfa qude acuerdo con un
proceso de Poisson de intensidad
![\\lambda](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda
"\\lambda") por segundo. Se tiene una fuerte sospecha de que el
parámetro desconocido supera 0.5, y con el objetivo de confirmar dicha
sospecha se medirán los tiempos entre emisiones consecutivas, obteniendo
de esa manera una muestra de variables aleatorias independientes con
distribución
![\\text{Exp}(\\lambda)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ctext%7BExp%7D%28%5Clambda%29
"\\text{Exp}(\\lambda)")

Se proponen dos alternativas, con respecto a la construcción del test de
hipótesis:

1.  Armar un test de nivel exacto
    ![\\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha
    "\\alpha") a partir del estadístico ![T\_1=\\sum\_{i=1}^n
    X\_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;T_1%3D%5Csum_%7Bi%3D1%7D%5En%20X_i
    "T_1=\\sum_{i=1}^n X_i")

2.  Armar un test de nivel
    ![\\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha
    "\\alpha") asintótico a partir del estadístico
    ![T\_2=\\sqrt{n}\\frac{\\bar{X}-2}{2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;T_2%3D%5Csqrt%7Bn%7D%5Cfrac%7B%5Cbar%7BX%7D-2%7D%7B2%7D
    "T_2=\\sqrt{n}\\frac{\\bar{X}-2}{2}")

El objetivo es comparar ambos métodos mediante una simulación en R para
luego elegir uno de ellos y realizar el experiemnto. Para cada valor de
$n {10, 30, 100, 1000} se quiere estimar el nivel empírico, y compararlo
para las dos alternativas propuestas. También se desea aproximar y
graficar la función potencia y comparar.

## Test exacto

![X\_1, \\ldots,
X\_n](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X_1%2C%20%5Cldots%2C%20X_n
"X_1, \\ldots, X_n") muestra aleatoria con densidad ![f\_\\lambda (x) =
\\lambda e^{-\\lambda
x}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;f_%5Clambda%20%28x%29%20%3D%20%5Clambda%20e%5E%7B-%5Clambda%20x%7D
"f_\\lambda (x) = \\lambda e^{-\\lambda x}")

Proponemos un test UMP con

  
![ H\_0: \\lambda \\leq 0.5
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%20H_0%3A%20%5Clambda%20%5Cleq%200.5%20
" H_0: \\lambda \\leq 0.5 ")  
  
![ H\_1: \\lambda \> 0.5
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%20H_1%3A%20%5Clambda%20%3E%200.5%20
" H_1: \\lambda \> 0.5 ")  

De la definición de
![f\_\\lambda(x)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;f_%5Clambda%28x%29
"f_\\lambda(x)") tenemos que ![c(\\lambda)= -
\\lambda](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;c%28%5Clambda%29%3D%20-%20%5Clambda
"c(\\lambda)= - \\lambda") es una función decreciente. Por lo tanto el
test queda definido como

  
![&#10; \\delta (\\mathbf{X})=&#10;\\begin{cases}&#10;1 & \\text{si
}-\\sum\_{i=1}^nX\_i \\geq k\_\\alpha \\\\&#10;0 & \\text{si
no}&#10;\\end{cases}&#10;](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%20%20%20%20%5Cdelta%20%28%5Cmathbf%7BX%7D%29%3D%0A%5Cbegin%7Bcases%7D%0A1%20%26%20%5Ctext%7Bsi%20%7D-%5Csum_%7Bi%3D1%7D%5EnX_i%20%5Cgeq%20k_%5Calpha%20%5C%5C%0A0%20%26%20%5Ctext%7Bsi%20no%7D%0A%5Cend%7Bcases%7D%0A
"
    \\delta (\\mathbf{X})=
\\begin{cases}
1 & \\text{si }-\\sum_{i=1}^nX_i \\geq k_\\alpha \\\\
0 & \\text{si no}
\\end{cases}
")  

Nos falta definir
![k\_\\lambda](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;k_%5Clambda
"k_\\lambda"). Para eso buscamos
![\\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha
"\\alpha") tal que ![P\_{\\lambda=0.5}\\left(-\\sum\_{i=1}^n X\_i \\geq
k\_\\lambda\\right)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;P_%7B%5Clambda%3D0.5%7D%5Cleft%28-%5Csum_%7Bi%3D1%7D%5En%20X_i%20%5Cgeq%20k_%5Clambda%5Cright%29
"P_{\\lambda=0.5}\\left(-\\sum_{i=1}^n X_i \\geq k_\\lambda\\right)").

Sabemos que ![\\sum\_{i=1}^n X\_i \\sim
\\Gamma(n, 1/\\lambda)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csum_%7Bi%3D1%7D%5En%20X_i%20%5Csim%20%5CGamma%28n%2C%201%2F%5Clambda%29
"\\sum_{i=1}^n X_i \\sim \\Gamma(n, 1/\\lambda)"). Por lo tanto podemos
definir

  
![ 2\\lambda \\sum\_{i=1}^n X\_i \\sim \\Gamma(n, 2) = \\chi^2\_{2n}
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%202%5Clambda%20%5Csum_%7Bi%3D1%7D%5En%20X_i%20%5Csim%20%5CGamma%28n%2C%202%29%20%3D%20%5Cchi%5E2_%7B2n%7D%20
" 2\\lambda \\sum_{i=1}^n X_i \\sim \\Gamma(n, 2) = \\chi^2_{2n} ")  

Por lo tanto,

  
![ P\_{\\lambda=0.5}\\left( 2\\lambda \\sum\_{i=1}^n X\_i
\\leq 2\\lambda k\_\\alpha\\right) = P(U \\leq k'\_\\alpha) = \\alpha
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%20P_%7B%5Clambda%3D0.5%7D%5Cleft%28%202%5Clambda%20%5Csum_%7Bi%3D1%7D%5En%20X_i%20%5Cleq%202%5Clambda%20k_%5Calpha%5Cright%29%20%3D%20P%28U%20%5Cleq%20k%27_%5Calpha%29%20%3D%20%5Calpha%20
" P_{\\lambda=0.5}\\left( 2\\lambda \\sum_{i=1}^n X_i \\leq 2\\lambda k_\\alpha\\right) = P(U \\leq k'_\\alpha) = \\alpha ")  

con ![U = 2 \\lambda \\sum\_{i=1}^n X\_i \\sim
\\chi^2\_{2n}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;U%20%3D%202%20%5Clambda%20%5Csum_%7Bi%3D1%7D%5En%20X_i%20%5Csim%20%5Cchi%5E2_%7B2n%7D
"U = 2 \\lambda \\sum_{i=1}^n X_i \\sim \\chi^2_{2n}").

Finalmente ![k'\_{\\alpha} = \\chi^2\_{2n,
\\alpha}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;k%27_%7B%5Calpha%7D%20%3D%20%5Cchi%5E2_%7B2n%2C%20%5Calpha%7D
"k'_{\\alpha} = \\chi^2_{2n, \\alpha}") y el test queda como

  
![&#10; \\delta (\\mathbf{X})=&#10;\\begin{cases}&#10;1 & \\text{si
}2\\lambda\\sum\_{i=1}^nX\_i \\leq \\chi^2\_{2n, \\alpha} \\\\&#10;0 &
\\text{si
no}&#10;\\end{cases}&#10;](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%20%20%20%20%5Cdelta%20%28%5Cmathbf%7BX%7D%29%3D%0A%5Cbegin%7Bcases%7D%0A1%20%26%20%5Ctext%7Bsi%20%7D2%5Clambda%5Csum_%7Bi%3D1%7D%5EnX_i%20%5Cleq%20%5Cchi%5E2_%7B2n%2C%20%5Calpha%7D%20%5C%5C%0A0%20%26%20%5Ctext%7Bsi%20no%7D%0A%5Cend%7Bcases%7D%0A
"
    \\delta (\\mathbf{X})=
\\begin{cases}
1 & \\text{si }2\\lambda\\sum_{i=1}^nX_i \\leq \\chi^2_{2n, \\alpha} \\\\
0 & \\text{si no}
\\end{cases}
")  

``` r
rechazo_1 <- function(n, lambda, alpha){
  muestra <- rexp(n, rate=lambda)
  T_1 <- sum(muestra)
  return(2*lambda*T_1 < qgamma(alpha, shape=n, rate=lambda))
}
```

``` r
#Datos
Nrep = 10000
n = 30
lambda = 0.5
alpha = 0.05

#Simulacion
simulaciones_rechazo <- replicate(Nrep, rechazo_1(n, lambda, alpha))

# Alpha queda
alpha_test <- mean(simulaciones_rechazo)
print(alpha_test)
```

    ## [1] 0.0534
