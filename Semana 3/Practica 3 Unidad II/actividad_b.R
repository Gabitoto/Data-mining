library(data.table)

library(readr)
Entrenamiento_ECI_2020 <- read_csv("Entrenamiento_ECI_2020.csv")
View(Entrenamiento_ECI_2020)

# 1. Forzar la conversión a data.table
table1 <- fread("Entrenamiento_ECI_2020.csv", col.names = "indice")
setDT(table1)