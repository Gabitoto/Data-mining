#Liberamos la memoria
rm(list = ls())
gc()
#Cargamos las librerias
library("arules")
library("arulesCBA")
library("caret")
#Cargamos los datos
data("iris")
summary(iris)
#Como son datos numericos, discretizamos las 4 variables numéricas 
#(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) por frecuancia 
#para dividir en 3 grupos de igual frecuencia (alto, medio y bajo). 
#Usamos la función discretize() de arules
iris_d <- iris
iris_d$Sepal.Length <- discretize(iris$Sepal.Length, method = "frequency", 
                                  breaks = 3, 
                                  labels = c("SL_bajo", "SL_medio", "SL_alto"))
iris_d$Sepal.Width  <- discretize(iris$Sepal.Width,  method = "frequency", 
                                  breaks = 3, 
                                  labels = c("SW_bajo", "SW_medio", "SW_alto"))
iris_d$Petal.Length <- discretize(iris$Petal.Length, method = "frequency", 
                                  breaks = 3, 
                                  labels = c("PL_bajo", "PL_medio", "PL_alto"))
iris_d$Petal.Width  <- discretize(iris$Petal.Width,  method = "frequency", 
                                  breaks = 3, 
                                  labels = c("PW_bajo", "PW_medio", "PW_alto"))

# Verificamos
head(iris_d)
str(iris_d)
# Dividimos en partición de entrenamiento y de testeo
# Inicializamos la semilla aleatoria
set.seed(1234)
porc <- 0.8
# 
train.ind <- createDataPartition(iris_d$Species, p=porc, list = F)
# Separamos los datos
datos.train <- iris_d[train.ind,]
datos.test <- iris_d[-train.ind,]
#----
# Entrenamos el clasificador con CBA
modelo_cba <- CBA(Species ~ .,
                  data = datos.train,
                  parameter = list(supp = 0.05, # 5% de soporte mínimo 
                                   conf = 0.8), # 80% de confianza mínima
                  verbose = TRUE)# Para ver el progreso
modelo_cba # 9 reglas
# Vemos las reglas
inspect(modelo_cba$rules)
# 
prediccion1 <- predict(modelo_cba, datos.test)
tabla1 <- confusionMatrix(prediccion1, datos.test$Species)
tabla1
#-----------------------------------------------------------------------
# Entrenamos un clasificador CMAR
modelo_cmar <- CMAR(Species ~ .,
                    data = datos.train,
                    support = 0.05,   # 5% de soporte mínimo
                    confidence = 0.8, # 80% de confianza mínima
                    verbose = TRUE)# Para ver el progreso
modelo_cmar # 43 reglas
# Vemos las reglas
inspect(modelo_cmar$rules)
# 
prediccion2 <- predict(modelo_cmar, datos.test)
tabla2 <- confusionMatrix(prediccion2, datos.test$Species)
tabla2
