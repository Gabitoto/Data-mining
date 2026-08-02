library(readr)
merval <- read_csv("merval.csv")
View(merval)

library(data.table)

# 1. Forzar la conversión a data.table (esto corrige el error)
dt <- fread("merval.csv", col.names = "indice")
setDT(dt)

# 2. Definir los nombres de las nuevas columnas
nombres_lags <- c("t_4", "t_3", "t_2", "t_1", "t")

# 3. Crear las columnas de desfase y el target
# Usamos shift con un vector de offsets ( 5, 4, 3, 2, 1)
dt[, (nombres_lags) := shift(indice, 5:1, type = "lag")]

# 4. Renombrar la columna original como target y eliminar filas con NA
setnames(dt, "indice", "target")
dt_windowed <- na.omit(dt)

# 5. Reordenar las columnas para que el target quede al final
setcolorder(dt_windowed, c(nombres_lags, "target"))

# Ver el resultado
print(head(dt_windowed))





