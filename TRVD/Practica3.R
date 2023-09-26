# Practica 3
library(dplyr)
library(plotly)
## Medidas de proximidad, clusters, escalado ####

## Ejercicio 3.1 ####

data_planets <- HSAUR::planets
data_planets <- data_planets %>% scale(.)


### 3.1.A ####
k <- 4
clusters <- kmeans(data_planets, centers = k)
clusters 

clusters$cluster

# Comparemos la salida de 

data_planets_cluster <- data_planets %>% 
    bind_cols(clusters$cluster) %>%
    rename(clus = `...4`)

centros <- data_planets_cluster %>%
    group_by(clus) %>%
    mutate(n_members=n()) %>%
    group_by(clus) %>%
    summarise_all(mean)

clusters$centers

withins <- data_planets_cluster %>% 
    group_by(clus) %>%
    summarise(SS = sum((mass-mean(mass))^2 + (period-mean(period))^2 + (eccen-mean(eccen))^2))

withins 
clusters$withinss

plot_ly(data_planets_cluster,x = ~mass, y = ~period, z = ~eccen, color = ~as.factor(clusters$cluster)) %>% 
    add_markers()

