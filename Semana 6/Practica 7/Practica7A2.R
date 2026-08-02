#Liberamos la memoria
rm(list = ls())
gc()

library(readr)
Entrenamiento_ECI_2020 <- read_csv("Entrenamiento_ECI_2020.csv")
View(Entrenamiento_ECI_2020)

# convertir ne facto la varible Stage

Entrenamiento_ECI_2020$Stage <- as.factor(Entrenamiento_ECI_2020$Stage)

# Dividimos en partición de entrenamiento y de testeo
# Inicializamos la semilla aleatoria
set.seed(42)
porc <- 0.8
# 
train.ind <- createDataPartition(Entrenamiento_ECI_2020$Stage, p=porc, list = F)
# Separamos los datos
datos.train <- Entrenamiento_ECI_2020[train.ind,]
datos.test <- Entrenamiento_ECI_2020[-train.ind,]

################################################################################

# Entrenamos el clasificador con CBA
modelo_cba <- CBA(Stage ~ .,
                  data = datos.train,
                  parameter = list(supp = 0.1, # 10% de soporte mínimo 
                                   conf = 0.5), # 50% de confianza mínima
                  verbose = TRUE)# Para ver el progreso
modelo_cba # 
# Vemos las reglas
inspect(modelo_cba$rules)
# 
prediccion <- predict(modelo_cba, datos.test)
tabla <- confusionMatrix(prediccion, datos.test$Stage)
tabla



