# Carga de librerías necesarias
# install.packages("tidyverse", dependencies = TRUE)
# library(tidyverse)

install.packages(c("tidyverse", "rmatio", "dplyr", "ggplot2"))
library(tidyverse)
library(rmatio)


# 1. Leer el archivo
df <- read.csv("Entrenamiento_ECI_2020.csv", stringsAsFactors = TRUE)

# 2. Verificar dimensiones (Filas y Columnas)
dim(df)

# 3. Tipos de datos y estructura
str(df)

# 4. Medidas de resumen
summary(df)

# Ver niveles actuales
levels(df$Stage)

# Discretización: Supongamos que solo nos interesan "Closed Won" y "Closed Lost"
# Filtrar registros: solo "Closed Won" y "Closed Lost"
df1 <- df[df$Stage == "Closed Won" | df$Stage == "Closed Lost", ]

# Transformar y limpiar niveles (discretización)
# Al filtrar en R Base, los niveles viejos quedan en la memoria del factor, 
# por lo que usamos factor() para refrescarlos.
df1$Stage <- factor(df1$Stage)

# Verificar niveles resultantes
levels(df1$Stage)

# Boxplot de Total_Amount para cada clase de Stage
boxplot(Total_Amount ~ Stage, data = df1, 
        main = "Monto Total por Estado",
        col = c("red", "green"),
        ylab = "Monto", xlab = "Estado")
# 

# Dispersión simple (usando colores para la clase)
plot(df1$TRF, df1$Total_Amount, 
     col = df1$Stage, 
     pch = 19,
     main = "Dispersión TRF vs Total Amount")
legend("topright", legend = levels(df1$Stage), col = 1:length(levels(df1$Stage)), pch = 19)

# Mediana:
median(df1$Total_Amount, na.rm = TRUE)

# Matriz de dispersión para las 3 variables
# Usamos colores según la variable 'Stage' (1: Won, 2: Lost)
pairs(df1[, c("TRF", "Total_Amount", "Total_Taxable_Amount")], 
      col = df1$Stage, 
      pch = 19, 
      main = "Relación entre TRF y Montos por Stage")


ggplot(df1, aes(x = Total_Amount, y = Total_Taxable_Amount, color = Stage)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~TRF) + # Opcional: separa por el valor de TRF
  theme_minimal() +
  labs(title = "Dispersión de Montos según Stage")

ggplot(df1, aes(x = TRF, y = Total_Amount, color = Stage)) +
  geom_point(alpha = 0.5) + # alpha ayuda a ver puntos solapados
  theme_minimal() +
  labs(title = "Relación entre TRF y Total Amount por Stage",
       x = "TRF",
       y = "Monto Total")

# Contar NAs por columna en tu dataset filtrado
colSums(is.na(df1[, c("TRF", "Total_Amount", "Total_Taxable_Amount")]))

# Ejemplo de imputación con la mediana para que aparezcan en el gráfico
# Justificación: En el procesamiento de datos, cuando existen errores o ruido (outliers), la mediana es un estadístico más robusto que el promedio ($mean$). Las variables financieras como Total_Amount suelen tener distribuciones sesgadas con valores muy altos que distorsionarían el promedio, mientras que la mediana representa mejor el centro de los datos en estos casos.
mediana_monto <- median(df1$Total_Amount, na.rm = TRUE) 
df1$Total_Amount[is.na(df1$Total_Amount)] <- mediana_monto 

# Frecuencia de Region
barplot(table(df1$Region), las = 2, col = "skyblue", main = "Frecuencia por Región")

# Frecuencia de Bureaucratic_Code
barplot(table(df1$Bureaucratic_Code), las = 2, col = "orange", main = "Frecuencia por Bureaucratic Code")

length(unique(df1$TRF))

#########################################

# Opción: Dispersión de montos, usando el color para Stage y el tamaño para TRF
ggplot(df1, aes(x = Total_Amount, y = Total_Taxable_Amount, color = Stage)) +
  geom_point(aes(size = TRF), alpha = 0.4) + # El tamaño del punto representa el TRF
  scale_x_log10() + # Escala logarítmica si los valores son muy grandes [cite: 13]
  scale_y_log10() + 
  theme_minimal() +
  labs(title = "Dispersión de Montos según Stage",
       subtitle = "El tamaño del punto indica el valor de TRF",
       x = "Monto Total (Escala Log)",
       y = "Monto Impuesto Total (Escala Log)")

# Frecuencias de Region
ggplot(df1, aes(x = Region, fill = Region)) +
  geom_bar() +
  coord_flip() + # Giramos el gráfico para leer mejor los nombres de las regiones 
  theme_minimal() +
  labs(title = "Frecuencia de Registros por Región", x = "Región", y = "Cantidad")

# Frecuencias de Bureaucratic_Code
ggplot(df1, aes(x = Bureaucratic_Code)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotamos etiquetas 
  labs(title = "Frecuencia por Código Burocrático", x = "Código", y = "Cantidad")




