library(AER)
library(dplyr)
library(ellipse)
library(tibble)
data <- CASchools

scatter <- ggplot(data = data, aes(x = income, y = math)) +
    geom_point() +
    labs(title = "Precio vs. Tiempo",
         x = "Tiempo",
         y = "Precio")
scatter

### B ####

model <- lm(math~income, data=data)
summary(model)

### C ####

fitplot <- scatter +
    geom_abline(intercept = model$coefficients[1], 
                slope = model$coefficients[2],
                color = "firebrick",
                linewidth = 2) 
fitplot
### F ####

residuos <- model$residuals
predict <- model$fitted.values

plot(residuos, predict)


### G ####

data$income2<- data$income^2

model2 <- lm(math ~ income + income2, data=data)
summary(model2)

fitplot2 <-fitplot +
        geom_line(aes(y = model2$fitted.values),
           color = "steelblue", linewidth = 1.5)
fitplot2

### J ####

data$income3<- data$income^3
model3 <- lm(math ~ income + income2 +income3, data=data)
summary(model3)

### K ####
anova(model, model3)
# El p valor es chico entonces las variables son significativas simultaneamente

### L ####

### M ####
summary(model)$sigma
summary(model2)$sigma
summary(model3)$sigma

### N ####
data <- data %>%
    mutate(income3 = income^3)
model3 <- lm(data = data, math ~ income + income2 + income3)
model3

fitplot3 <- fitplot2 + 
    geom_line(aes(y = model3$fitted.values),
              color = "purple", linewidth = 1.5)
fitplot3


## 3 ####

growth <- read.csv("growth.txt", sep="\t")


cols = c("red", "orange", "yellow")
labs = c("Cebada", "Avena", "Trigo")
barplot(tapply(growth$gain, list(growth$diet, growth$supplement), mean), col=cols, ylim=c(0,40), beside=T)
legend(6.3,40, labs, fill=cols)
### B. ####
modelo <- lm(gain~diet*supplement, data=growth)

### C. ####
interaction.plot(growth$diet, growth$supplement, growth$gain)

interaction.plot(growth$supplement, growth$diet,growth$gain)

### D. ####

model_ad <- lm(gain~diet+supplement, data=growth)

anova(model_ad, modelo)
# no rechazo la hipotesis nula : no tengo evidencia para decir que son significativas

model_nosupp <-lm(gain~diet, data=growth)
anova(model_ad, model_nosupp)
# Supp es significativo

### G ####

summary(model_ad)

new_data_1 <- data.frame(diet="oats", supplement ="supersupp")
new_data_2 <- data.frame(diet="barley", supplement ="agrimore")
predict(model_ad, new_data_1)
predict(model_ad, new_data_2)

### Ejercicio 4 #####

data <- stackloss

## A ####

pairs (data)

## B ####
model <- lm(stack.loss~Air.Flow+Water.Temp+Acid.Conc., data=data)

## C ####
summary(model)

## D ####
model2 <- lm(stack.loss~Air.Flow+Water.Temp, data=data)
summary(model2)

# A mano
confint(model, level = 0.9)
alpha <- 1-0.9

betas <- matrix(model$coefficients)
X <- model.matrix(model)

n <- nrow(model.matrix(model))
p <- ncol(model.matrix(model))

a <- matrix(c(1, 0, 0, 0))
t(a) %*%  betas # Valor predicho

t(a) %*%  betas + qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model)  * sqrt(t(a) %*% Matrix::solve(t(X)%*%X) %*% a)
t(a) %*%  betas - qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)
## F ####

alpha <- 1-0.95

betas <- matrix(model$coefficients)
X <- model.matrix(model)

n <- nrow(model.matrix(model))
p <- ncol(model.matrix(model))

a <- matrix(c(1, 58, 20, 86))
t(a) %*%  betas # Valor predicho

t(a) %*%  betas + qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model)  * sqrt(t(a) %*% Matrix::solve(t(X)%*%X) %*% a)
t(a) %*%  betas - qt(p = alpha/2, df = n-p, lower.tail = F) *  sigma(model)  * sqrt(t(a) %*% solve(t(X)%*%X) %*% a)


## H ###


## I ####
library(car)
# Usando la librería ellipse
data_ellipse <- as_tibble(ellipse(model, which=cov(model), level=.90),)
plot_ellipse <- data_ellipse %>% 
    ggplot(aes(x = air_flow,
               y = water_temp)) +
    geom_path() +
    geom_point(x = coef(mode)[2], y = coef(model)[3], size =2,color="red") +
    theme_bw()
plot_ellipse