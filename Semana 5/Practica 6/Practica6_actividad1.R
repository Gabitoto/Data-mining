#install.packages("arules")
library("tidyverse")
library("arules")

data <- read.csv("groceries.csv", sep = ",")

# lectura en formato basquet
mis_transacciones1 <- read.transactions("groceries.csv", 
                                        format = "basket", 
                                        sep = ",",      # Separador entre columnas (ID y Lista)
                                        rm.duplicates = TRUE) # Limpieza: quita productos repetidos en una misma compra


summary(mis_transacciones1) # vemos si levanto bien todas las transacciones
mis_transacciones1 # vemos lo mismo pero mas reducido

# Ver las frecuencias relativas (soporte individual)
frec <- itemFrequency(mis_transacciones1, type = "relative")
sort(frec, decreasing = TRUE)[1:10]         # top 10 ítems más frecuentes

# Criterio estadístico para soporte mínimo
#    Se usa la MEDIA como umbral base, una práctica común:
#    "un ítem es relevante si aparece más que el ítem promedio"
soporte_min <- median(frec)
cat("Soporte mínimo (mediana):", round(soporte_min, 4))

# visualizamos la distribucion de los datos para elegir el estadistico:
boxplot(frec,
        main  = "Distribucion de las frecuencias",
        xlab  = "Frecuencias",
        ylab  = "Valores",
        col   = c("lightblue", "lightgreen", "lightyellow"),
        border = "darkgray")

# Ejecutar ECLAT con ese soporte mínimo GENERA ITEMSETS FRECUENTES
reglas_eclat <- eclat(
  mis_transacciones1,
  parameter = list(
    support = soporte_min,   # umbral calculado
    minlen  = 1,             # mínimo 1 ítem por conjunto
    maxlen  = 10             # máximo ítems por conjunto
  )
)

# Ver los 5 itemsets de MAYOR soporte
top5 <- sort(reglas_eclat, by = "support", decreasing = TRUE)
inspect(head(top5, 5))

###############################################

# Inducir reglas desde los itemsets frecuentes
reglas1 <- ruleInduction(
  top5,                          # itemsets base
  transactions = mis_transacciones1,          # datos originales (para calcular confianza)
  confidence   = 0.5             # confianza mínima
)

length(reglas1)

inspect(sort(reglas1, by = "confidence", decreasing = TRUE))

############################

# Inducir reglas desde los itemsets frecuentes
reglas2 <- ruleInduction(
  top5,                          # itemsets base
  transactions = mis_transacciones1,          # datos originales (para calcular confianza)
  confidence   = 0.25             # confianza mínima
)

length(reglas2)

inspect(sort(reglas2, by = "confidence", decreasing = TRUE))

##############################################
# Seguimos trabajando con las reglas de 0.25 #
##############################################


# Reglas que tiene un Lift mayor a 2 (lift > 2)
reglas_lift2 <- subset(reglas2, lift > 2)
inspect(sort(reglas_lift2, by = "lift", decreasing = TRUE))
length(reglas_lift2)

# las reglas que contienen el elemento “instant coffe” en el consecuente con un lift mayor a 2

reglas_rhs <- subset(reglas2, 
                     rhs %in% "root vegetables" & 
                       lift > 2)
inspect(sort(reglas_rhs, by = "lift", decreasing = TRUE))
length(reglas_rhs)

# las reglas que contienen solamente “instant coffe" y "baby food” en el antecedente y un lift mayor a 2

reglas_lhs <- subset(reglas2, 
                     lhs %in% c("root vegetables","baby food") & 
                       lift > 2)
inspect(sort(reglas_lhs, by = "lift", decreasing = TRUE))
length(reglas_lhs)

# las reglas que contienen “preservation products " o "liqueur” en el antecedente, “instant coffe” consecuente y un lift mayor a 2.

reglas_combinada <- subset(reglas2,
                           (lhs %in% "root vegetables" | 
                              lhs %in% "liqueur") &
                             rhs %in% "other vegetables" &
                             lift > 2)
inspect(sort(reglas_combinada, by = "lift", decreasing = TRUE))
length(reglas_combinada)

################################################################################


