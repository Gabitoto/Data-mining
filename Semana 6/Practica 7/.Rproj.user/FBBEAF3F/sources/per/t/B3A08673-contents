#Liberamos la memoria
rm(list = ls())
gc()

# Instalación del paquete y sus dependencias
install.packages("arulesCBA", dependencies = TRUE)

#Cargamos las librerias
library("arules")
library("arulesCBA")
library("caret")

# Carga del dataset
load("titanic.raw.rdata")

# Verificamos estructura (debería ser un dataframe con factores)
str(titanic.raw)

# Definimos la semilla para reproductibilidad
set.seed(123)

# Creamos el índice de partición
train_idx <- createDataPartition(titanic.raw$Survived, p = 0.8, list = FALSE)

# Dividimos los datos
train_data <- titanic.raw[train_idx, ]
test_data  <- titanic.raw[-train_idx, ]

# Entrenamos el modelo Act 1
# El parámetro formula indica que queremos predecir Survived usando el resto (~ .)
modelo_cba <- CBA(Survived ~ ., data = train_data, support = 0.1, confidence = 0.3)

modelo_cba # 4 rules

# Inspeccionamos las reglas generadas
inspect(modelo_cba$rules)

# Realizamos las predicciones
predicciones <- predict(modelo_cba, test_data)

# Matriz de confusión para ver métricas (Accuracy, Recall, etc.)
confusionMatrix(predicciones, test_data$Survived)

# Act 2 ########################################################################

# Entrenamos el clasificador con CBA
modelo_cba1 <- CBA(Survived ~ .,
                  data = train_data,
                  parameter = list(supp = 0.001, # 0.001% de soporte mínimo 
                                   conf = 0.8), # 80% de confianza mínima
                  verbose = TRUE)# Para ver el progreso
modelo_cba1 # 9 reglas
# Vemos las reglas
inspect(modelo_cba1$rules)
# 
prediccion1 <- predict(modelo_cba1, test_data)
tabla1 <- confusionMatrix(prediccion1, test_data$Survived)
tabla1

# Act 3 ########################################################################

# Entrenamos un clasificador CMAR
modelo_cmar <- CMAR(Survived ~ .,
                    data = train_data,
                    support = 0.1,   # 10% de soporte mínimo
                    confidence = 0.3, # 30% de confianza mínima
                    verbose = TRUE)# Para ver el progreso
modelo_cmar # 30 reglas
# Vemos las reglas
inspect(modelo_cmar$rules)
# 
prediccion2 <- predict(modelo_cmar, test_data)
tabla2 <- confusionMatrix(prediccion2, test_data$Survived)
tabla2

# Act 4 ########################################################################

# Entrenamos un clasificador CMAR
modelo_cmar1 <- CMAR(Survived ~ .,
                    data = train_data,
                    support = 0.001,   # 0.001% de soporte mínimo
                    confidence = 0.8, # 80% de confianza mínima
                    verbose = TRUE)# Para ver el progreso
modelo_cmar1 # 16
# Vemos las reglas
inspect(modelo_cmar1$rules)
# 
prediccion3 <- predict(modelo_cmar1, test_data)
tabla3 <- confusionMatrix(prediccion3, test_data$Survived)
tabla3

# Act 5 ########################################################################

confusionMatrix(predicciones, test_data$Survived)
confusionMatrix(prediccion1, test_data$Survived)
confusionMatrix(prediccion2, test_data$Survived)
confusionMatrix(prediccion3, test_data$Survived)

