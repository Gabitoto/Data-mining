#Liberamos la memoria
rm(list = ls())
gc()
#
library("arules")
data(Groceries)
summary(Groceries)
#----
soporte <- 0.01
# Aplicamos APRIORI
t1 <- Sys.time()
items_frec_apriori <- apriori(Groceries, 
                              parameter = list(support = soporte,
                                               target = "frequent itemsets"), 
                              control = list(verbose=F))
t2 <- Sys.time()
t_apriori <- t2-t1
# Aplica FP-Growth
t1 <- Sys.time()
items_frec_fpg <- fim4r(Groceries, 
                        method = "fpgrowth", 
                        target = "frequent",
                        support = soporte,
                        zmin = 1, # tamaño mínimo, si no, toma el conjunto vacío
                        verbose = F)
t2 <- Sys.time()
t_fpg <- t2-t1
# Aplica ECLAT
t1 <- Sys.time()
items_frec_eclat <- eclat(Groceries,
                          parameter = list(support = soporte), 
                          control = list(verbose = F))
t2 <- Sys.time()
t_eclat <- t2-t1
# Ver resultados
t_apriori
t_fpg
t_eclat
# Cantidad de itemsets frecuentes
items_frec_apriori
items_frec_eclat
items_frec_fpg  
# 
inspect(sort(items_frec_apriori, by = "support", decreasing = T)[1:10])
inspect(sort(items_frec_eclat, by = "support", decreasing = T)[1:10])
inspect(sort(items_frec_fpg, by = "support", decreasing = T)[1:10])

#----
# Graficamos los items más frecuentes
# Los items más frecuentes son Leche entera (2513), otros vegetales (1903) y 
# panecillos/bollos (1809)
frecuencias <- itemFrequency(Groceries, type = "absolute")
# Extraer los nombres de los 10 ítems más frecuentes correctamente
top_items_names <- names(head(sort(frecuencias, decreasing = TRUE), 10))
# Graficamos
barplot((frecuencias[top_items_names]))
# 
itemFrequencyPlot(Groceries, 
                  topN = 10,
                  type = "absolute")

#----
# Reglas
reglas <- apriori(Groceries, 
                  parameter = list(support = soporte,
                                   confidence = 0.3,
                                   target = "rules"), 
                  control = list(verbose=F))
reglas # 125 reglas
# Con ruleInduction
reglas2 <- ruleInduction(items_frec_eclat, 
                              Groceries, 
                              confidence = 0.3)
reglas2
# Ordenamos
reglasOrd <- sort(reglas2, by = "confidence")

inspect(reglasOrd[1:20])
# Nos quedamos con las que tienen "otros vegetales" en el consecuente
reglas_veggie <- subset(reglas2, 
                        subset = rhs %in% "other vegetables") # 44 reglas

reglas_veggieOrd <- sort(reglas_veggie, by = "confidence")

inspect(reglas_veggieOrd[1:20])
#----
# Ejemplo del Titanic
# Liberamos la memoria
rm(list = ls())
gc()
#Cargamos las librerias
library("arules")
# a)
# Cargamos los datos
load("./datos/titanic.raw.rdata")
str(titanic.raw)
head(titanic.raw)
summary(titanic.raw)

table(titanic.raw$Class)
table(titanic.raw$Sex)
table(titanic.raw$Age)
table(titanic.raw$Survived)

# Veamos algunos supuestos
# Niños y mujeres primero
child <- titanic.raw[titanic.raw$Age == "Child",]
table(child$Survived)
#
female <- titanic.raw[(titanic.raw$Age == "Adult" & titanic.raw$Sex == "Female"),]
table(female$Survived)
#
male <- titanic.raw[(titanic.raw$Age == "Adult" & titanic.raw$Sex == "Male"),]
table(male$Survived)
# Vemos la tripulación
crew <- titanic.raw[(titanic.raw$Class == "Crew"),]
table(crew$Sex)
table(crew$Survived[crew$Sex == "Male"])
table(crew$Survived[crew$Sex == "Female"])
# b)
#Encontramos las reglas de asociación con parámetros por defecto
titanic_trans <- transactions(titanic.raw)
titanic_trans2 <- as(titanic.raw, "transactions")

titanic_trans
titanic_trans2

summary(titanic_trans)
summary(titanic_trans2)

inspect(titanic_trans[1:20])

# Reglas usando como consecuente sólo si sobrevive o no
reglas.sobrevive <- apriori(titanic_trans, 
                            parameter = list(supp=0.05, conf=0.8),
                            appearance = list(rhs=c("Survived=No", "Survived=Yes")), 
                                              #para asegurarse que explique tanto "Yes" como "No"
                            control = list(verbose=F))
reglas.sobrevive
inspect(reglas.sobrevive) #encontramos 6 reglas
# Las ordenamos por confianza
reglas.sobrevive.ordenadas <- sort(reglas.sobrevive, by="confidence")
inspect(reglas.sobrevive.ordenadas)
#----
# Separamos las reglas para los sobrevivientes y lo que no sobrevivieron
reglas.sobrevive.si <- apriori(titanic_trans, 
                               parameter = list(supp=0.005, conf=0.8),
                               appearance = list(rhs=c("Survived=Yes")),
                               control = list(verbose=F))
# 8 reglas
reglas.sobrevive.si <- sort(reglas.sobrevive.si, by="confidence")
inspect(reglas.sobrevive.si)
#
reglas.sobrevive.no <- apriori(titanic_trans, 
                               parameter = list(supp=0.001, conf=0.6),
                               appearance = list(rhs=c("Survived=No")),
                               control = list(verbose=F))
# 19 reglas
reglas.sobrevive.no <- sort(reglas.sobrevive.no, by="confidence")
inspect(reglas.sobrevive.no)

itemsets.si <- generatingItemsets(reglas.sobrevive.si)
inspect(itemsets.si)
#----
# Visualización
if(!require("arulesViz"))  install.packages("arulesViz")
library("arulesViz")
# Default: confianza vs soporte
plot(reglas.sobrevive.ordenadas)
# Usando "shading", coloreamos los puntos por la medida que querramos
# Por lift
plot(reglas.sobrevive.ordenadas, shading = "lift")
# Usamos method (metodo de visualización). Opciones: "scatterplot", 
# "two-key plot", "matrix", "grouped matrix", "graph", "paracoord"
# Con "grouped matrix" o simplemente "grouped" grafica los concecuentes (rhs)
# en función de los antecedentes (lhs). Coloreados por lift y el radio del
# punto indica el soporte de la regla
plot(reglas.sobrevive.ordenadas, method="grouped matrix")
# Como grafo
plot(reglas.sobrevive.ordenadas, method="graph")
# Grafico interactivo con engine = "interactive"
plot(reglas.sobrevive.ordenadas, 
     method = "graph", 
     engine = "interactive")
# Coordenadas paralelas
plot(reglas.sobrevive.ordenadas, 
     method="paracoord", 
     control=list(reorder=TRUE))
# El motor "default" usa mayormente ggplot2. Otros motores: ‘default’, 
# ‘ggplot2’, ‘igraph’, ‘interactive’, ‘graphviz’, ‘visNetwork’, 
# ‘htmlwidget’ (que puede incrustarse en RMarkdown). 
# Con "visNetwork" permite filtrar por regla, ítem, etc. y es interactvo
plot(reglas.sobrevive.ordenadas, 
     method = "graph", 
     engine = "visNetwork")



















