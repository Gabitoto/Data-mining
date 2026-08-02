# Indicamos la dimenciones del dataset:

library(readr)
Entrenamiento_ECI_2020 <- read_csv("Entrenamiento_ECI_2020.csv")
View(Entrenamiento_ECI_2020)

str(Entrenamiento_ECI_2020)
summary(Entrenamiento_ECI_2020)
is.nan(Entrenamiento_ECI_2020)

# Si querés especificar el orden de los niveles (levels)
Entrenamiento_ECI_2020$Stage <- factor(Entrenamiento_ECI_2020$Stage, levels = c("Closed Lost", "Closed Won"))
str(Entrenamiento_ECI_2020)

# Eliminamos los NA que estan en Stage
library(data.table)

# Convertir a data.table (esto es lo que probablemente falta)
setDT(Entrenamiento_ECI_2020)

# Ahora sí funcionará el argumento cols
dataset_limpio2 <- na.omit(Entrenamiento_ECI_2020, cols = "Stage")

# Graficamos:

library(ggplot2)

# Crear el boxplot
ggplot(dataset_limpio2, aes(x = Stage, y = Total_Amount, fill = Stage)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(
    title = "Distribución del Monto Total por Etapa (Stage)",
    subtitle = "Análisis de variabilidad entre categorías",
    x = "Etapa del Proceso (Stage)",
    y = "Monto Total (Total Amount)"
  ) +
  scale_y_log10() +
  labs(y = "Monto Total (Escala Log10)") +
  theme_minimal() +
  theme(legend.position = "none") # Ocultamos la leyenda ya que el eje X ya indica el Stage


