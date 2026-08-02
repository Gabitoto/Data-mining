rm(list = ls())

library("tidyverse")
library("arules")

data <- read.csv("orders.csv", sep = ";")

# lectura formato largo
mis_transacciones1 <- read.transactions("orders.csv", 
                                        format = "single", 
                                        sep = ";", 
                                        cols = c("order_id", "product_name"), 
                                        header = TRUE)


summary(mis_transacciones1) # vemos si levanto bien todas las transacciones
mis_transacciones1 # vemos lo mismo pero mas reducido

# Ver las frecuencias relativas (soporte individual)
frec <- itemFrequency(mis_transacciones1, type = "relative")
sort(frec, decreasing = TRUE)[1:10]

items_frec_eclat <- eclat(mis_transacciones1,
                          parameter = list(support = 0.001, minlen = 3), 
                          control = list(verbose = F))
items_frec_eclat
inspect(sort(items_frec_eclat, by = "support", decreasing = T)[1:10])

################################################################################

# Es necesario pasar el objeto de transacciones para calcular la confianza
reglas_eclat <- ruleInduction(items_frec_eclat, 
                              mis_transacciones1, 
                              confidence = 0.6)

# 2. Ordenar las reglas por 'lift' de forma descendente
reglas_ordenadas <- sort(reglas_eclat, by = "lift", decreasing = TRUE)

# 3. Visualizar los resultados
inspect(reglas_ordenadas)

################################################################################

# Comparamos el ECLAT con el Apriori con la misma confianza y soporte:

#Apriori
reglasapriori <- apriori(mis_transacciones1, 
                  parameter = list(support = 0.001,
                                   confidence = 0.6,
                                   target = "rules", minlen = 3), 
                  control = list(verbose=F))
reglasapriori # 15 reglas

#ECLAT
reglas_eclat # 15 reglas

################################################################################

# 1. Calculamos las métricas adicionales para ambos conjuntos
# (Suponiendo que 'reglas_eclat' y 'reglas_apriori' ya existen)

metricas_extra_eclat <- interestMeasure(reglas_eclat, 
                                        measure = c("coverage", "fishersExactTest"), 
                                        transactions = transacciones)

metricas_extra_apriori <- interestMeasure(reglasapriori, 
                                          measure = c("coverage", "fishersExactTest"), 
                                          transactions = transacciones)

# 2. Agregamos estas métricas a los objetos de reglas para poder visualizarlas
quality(reglas_eclat) <- cbind(quality(reglas_eclat), metricas_extra_eclat)
quality(reglasapriori) <- cbind(quality(reglasapriori), metricas_extra_apriori)

# 3. Comparación visual de las 5 mejores reglas por Lift
inspect(head(sort(reglas_eclat, by = "lift"), 5))
inspect(head(sort(reglasapriori, by = "lift"), 5))

################################################################################

# Filtrar las reglas generadas (puedes usar reglas_eclat o reglas_apriori)
reglas_filtradas <- subset(reglasapriori, 
                           subset = rhs %in% "Bag of Organic Bananas" & 
                             lhs %in% "Organic Raspberries")

# Ordenar por lift para priorizar las asociaciones más fuertes
reglas_filtradas <- sort(reglas_filtradas, by = "lift", decreasing = TRUE)

# Visualizar el resultado
inspect(reglas_filtradas)
