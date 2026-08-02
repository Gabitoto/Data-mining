# Limpiamos la memoria
rm(list = ls())
# Cargamos las librerias que necesitamos
library("pdftools") # Para pdf_text()
library("stringr")  # Para str_split_fixed()
# Cargamos los datos
datos <- pdf_text("https://iah.msal.gov.ar/doc/722.pdf")
# La tabla de interés está en la página 11
hoja <- datos[11]
# Separamos por linea
lineas <- strsplit(hoja, "\n") # \n es nueva línea
# Nos quedamos con el primer elemento
lineas <- lineas[[1]]
#
grep("Región", lineas)
ini <- grep("Región", lineas)[2] # Nos quedamos con la segunda posición que es la que
# queremos
fin <- grep("Región del Pacífico Occidental", lineas)
tabla <- lineas[ini:fin]
head(tabla)
# Eliminamos los espacios en blanco al inicio y final de las cadenas filas
tabla <- trimws(tabla)
# Verificamos
tabla
# Como los números aparece con la "." como divisor de miles, lo eliminamos
tabla <- str_remove_all(tabla,"\\.")
# Vamos a dividir las cadenas de las filas en 3 columnas y usamos como
# patrón en espacio " ", pero que aparezca 2 o más veces
tabla <- str_split_fixed(tabla, " {2,}", 3)
# Armamos un dataframe
# Desde la fila 2, porque la 1 es el encabezado de la tabla
tabla1 <- data.frame(matrix(tabla[2:nrow(tabla),],ncol=3))
# Le damos los nombres
colnames(tabla1) <- tabla[1,]
# Transformamos los datos
tabla1$Casos <- as.numeric(tabla1$Casos)
tabla1$Fallecidos <- as.numeric(tabla1$Fallecidos)
View(tabla1)
# Guardamos los datos
library("data.table") # para fwrite()
fwrite(tabla1,"./salidas/viruela_OMS_sep_2022.csv", sep=";")
