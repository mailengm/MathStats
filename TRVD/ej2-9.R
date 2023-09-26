library(MASS)
library(ggplot2)
mu_1 = c(1,1/2)
mu_2 = c(-1/2,1)
mu = c(5/8, 5/8)

mu_1_ <- mu_1 - mu
mu_2_ <- mu_2 - mu

S_b <- fractions(3/4 * (mu_1_%*%t(mu_1_))) + fractions(1/4 * (mu_2_%*%t(mu_2_)))
S_b
S_w = matrix(c(1,1/2,1/2,1),ncol = 2, nrow = 2)
S_b = matrix(c(-27/64,-9/64,-9/64,3/64),ncol = 2, nrow = 2)

S_aux<-solve(S_w)%*%S_b
fractions(S_aux)

#definimos como la matriz C triangular tal que t(C) %*% C recupere sigma_w:
C = sqrtm(S_w)
#compruebo:
t(C) %*% C
#me da igual que sigma_w!

#calculo los autovalores y autovectores de B C^(-1) simga_b C que me define la
#magnitud F que quiero maximizar y adopta los valores maximos en sus autovectores:
B = t(solve(C)) %*% S_b %*% solve(C)

#calculo ave y ava:
b = eigen(B)

#a continuación defino la matriz A que esta compuesta por los alpha definidos 
#como C^(-1)*beta_j con beta_j autovector de B:
A_aux = b$vectors

A = solve(C) %*% A_aux 

#A<-eigen(S_aux)$vectors
#fractions(A)

t(A)%*%S_w%*%A
t(A)%*%S_b%*%A

Sx <- S_w+S_b

t(A)%*%Sx%*%A

# Load necessary libraries
library(ggplot2)
library(mvtnorm)

# Set your parameters (make sure you've defined mu_1, mu_2, and S_w)
mu_1 <- c(1, 2)  # Example mean vector for x_1
mu_2 <- c(3, 4)  # Example mean vector for x_2
S_w <- matrix(c(1, 0.5, 0.5, 2), nrow = 2)  # Example covariance matrix

# Generate random data
set.seed(3487)  # for reproducibility
n_1 <- rbinom(1, 500, 3/4)
n_2 <- 500 - n_1

x_1 <- mvrnorm(n_1, mu = mu_1, Sigma = S_w)
x_2 <- mvrnorm(n_2, mu = mu_2, Sigma = S_w)

# Combine the data into a data frame
data <- data.frame(
  x = c(x_1[, 1], x_2[, 1]),
  y = c(x_1[, 2], x_2[, 2]),
  group = factor(rep(c("G=1", "G=2"), c(n_1, n_2)))
)

# Create the scatterplot using ggplot2
scatter_plot<-ggplot(data, aes(x = x, y = y, color = group)) +
  geom_point() +
  labs(x = "X1", y = "X2", title = "Dispersión de las variables originales") +
  scale_color_manual(values = c("G=1" = "#EE349c", "G=2" = "#07004D"))  # Customize colors if needed

# Save the plot to a PDF file
ggsave("scatter_plot.pdf", plot = scatter_plot, width = 8, height = 3)


x_1_ <- sweep(x_1, 2, mu)
x_2_ <- sweep(x_2, 2, mu)


z_1 <- t(t(A)%*%t(x_1_))
z_2 <- t(t(A)%*%t(x_2_))

zz <- rbind(z_1, z_2)

data <- data.frame(
  x = zz[,1],
  y = zz[,2],
  group = factor(rep(c("G=1", "G=2"), c(length(z_1), length(z_2))))
)

trans_plot<-ggplot(data, aes(x = x, y = y, color = group)) +
  geom_point() +
  labs(x = "x", y = "y", title = "Dispersión de las variables transformadas") +
  scale_color_manual(values = c("G=1" = "#EE349c", "G=2" = "#07004D"))  # Customize colors if needed
trans_plot
# Save the plot to a PDF file
ggsave("scatter_plot.pdf", plot = trans_plot, width = 8, height = 3)


plot(z_1, col = '#EE3496', pch=16)
points(z_2, col = '#004D40', pch=16)

