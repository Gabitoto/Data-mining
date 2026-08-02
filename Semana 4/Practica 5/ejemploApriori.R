# Algoritmo Apriori
#Liberamos la memoria
rm(list = ls())
gc()
#Cargamos las librerias
library("arules")
# Generamos los datos en tipo basket
datos <- list(c("A","B","C","D"),
              c("A","B","D"),
              c("A","B"),
              c("B","C","D"),
              c("B","C"),
              c("C","D"),
              c("B","D"))
# Le damos nombres
names(datos) <- paste0("Tr", c(1:length(datos)))
# Generamos el tipo de dato transacciones
transacciones <- transactions(datos)
summary(transacciones)
# Vemos las transacciones
inspect(transacciones)
# Obtenemos el tamaño de las transacciones
n.trans <- size(transacciones)
# Vemos como se distribuyen las cantidades de items por transacción
hist(n.trans)
# Para pasar a data.frame en formato "single"
aux <- toLongFormat(transacciones, cols = c("TID", "item"), decode = TRUE)
View(aux)
#----
# Y si tenemos los datos en formato single creamos el objeto transacciones desde 
# data.frame, aclarando que el formato de origen es el "single" con el parametro 
# formato = "long"
transacciones2 <- transactions(aux, format = "long")
# Vemos las transacciones
inspect(transacciones2)
# Obtenemos el tamaño de las transacciones
n.trans <- size(transacciones2)
#----
# Encontramos los itemsets frecuentes
soporte <- 0.4
itemsets_frec <- apriori(transacciones, 
                         parameter = list(support = soporte,
#                                          minlen = 2,
                                          target = "frequent itemsets"), 
                         control = list(verbose=F))
# Encontramos 4 itemsets frecuentes 
summary(itemsets_frec)
inspect(itemsets_frec)
# Vemos los 4 itemsets frecuentes de mayor soporte
itemsets <- sort(itemsets_frec, by = "support", decreasing = TRUE)
inspect(itemsets)
# Encontramos las reglas de asociación
confianza <- 0.7

reglas <- apriori(transacciones,
                  parameter = list(support = soporte,
                                   confidence = confianza,
                                   minlen = 2,
                                   target = "rules"),
                  control = list(verbose = F))
reglas # Encontramos 4 reglas
# Ordenamos las reglas por confianza y las mostramos
inspect(sort(reglas, decreasing = TRUE, by = "confidence"))
# 
size(reglas)
