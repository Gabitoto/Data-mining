
rm(list = ls())

library("tidyverse")
library("arules")

data <- read.csv("orders.csv", sep = ";")

view(data) # vemos que si bien esta en formato Basket no esun objeto Transactions por lo que debemos de transformarlo

# lectura en formato basquet
mis_transacciones1 <- read.transactions("orders.csv", 
                                       format = "basket", 
                                       sep = ";",      # Separador entre columnas (ID y Lista)
                                       cols = 1,       # La columna 1 es el ID (se ignora al procesar)
                                       rm.duplicates = TRUE) # Limpieza: quita productos repetidos en una misma compra

# lectura formato largo
mis_transacciones2 <- read.transactions("orders.csv", 
                                       format = "single", 
                                       sep = ";", 
                                       cols = c("order_id", "product_name"), 
                                       header = TRUE)

# 1. Encontrar itemsets frecuentes con soporte >= 1% (0.001)
itemsets_frecuentes1 <- apriori(mis_transacciones2, 
                               parameter = list(supp = 0.001, target = "frequent itemsets"))

# Ordenar por soporte y ver los primeros 5
inspect(head(sort(itemsets_frecuentes1, by = "support"), n = 5))

# Generar reglas con soporte 1% y confianza 70%
reglas1 <- apriori(mis_transacciones2, parameter = list(supp=0.001,conf=0.7, target="rules"))

summary(reglas1)

# Ordenar por Lift (fuerza de la relación)
reglas_ordenadas <- sort(reglas1, by = "lift", decreasing = TRUE)

# Mostrar las 10 mejores reglas
inspect(head(reglas_ordenadas, n = 10))
