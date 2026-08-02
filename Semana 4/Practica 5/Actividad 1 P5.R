library("tidyverse")
library("arules")

data("Groceries")

trans <- transactions(Groceries)
trans

rules <- apriori(trans, supp = 0.01, conf = 0.9, target = "rules")

# Actividad 1. Graficar los 10 productos más frecuentes directamente
itemFrequencyPlot(Groceries, 
                  topN = 10, 
                  type = "absolute", 
                  main = "Top 10 Productos más Frecuentes",
                  col = "steelblue",
                  ylab = "Frecuencia Absoluta")

# otra forma de hacerlo es utilizando tidyverse:

# 1. Extraer frecuencias y convertir a tibble
frecuencias <- itemFrequency(Groceries, type = "absolute") %>% 
  enframe(name = "producto", value = "frecuencia") %>% 
  arrange(desc(frecuencia)) %>% 
  slice(1:10)

# 2. Mostrar tabla en consola
print(frecuencias)

# 3. Graficar con ggplot2
ggplot(frecuencias, aes(x = reorder(producto, frecuencia), y = frecuencia)) +
  geom_col(fill = "skyblue") +
  coord_flip() +  # Horizontal para leer mejor los nombres de productos
  labs(title = "Top 10 Productos más Frecuentes",
       subtitle = "Dataset: Groceries",
       x = "Producto",
       y = "Cantidad de Transacciones") +
  theme_minimal()

# Actividad 2. Los itemsets frecuentes con un soporte mínimo del 1% y las reglas de asociación para el mismo soporte y una confianza del 30%.

# 1. Encontrar itemsets frecuentes con soporte >= 1% (0.01)
itemsets_frecuentes <- apriori(Groceries, 
                               parameter = list(supp = 0.01, target = "frequent itemsets"))

# Ordenar por soporte y ver los primeros 10
inspect(head(sort(itemsets_frecuentes, by = "support"), n = 10))

# ahora generamos reglas

# Generar reglas con soporte 1% y confianza 30%
reglas <- apriori(Groceries, 
                  parameter = list(supp = 0.01, conf = 0.3, target = "rules"))

# Ver un resumen de las reglas generadas
summary(reglas)

# Inspeccionar las 10 reglas con mayor 'lift' (métrica de interés/fuerza)
inspect(head(sort(reglas, by = "lift"), n = 10))

################################################################################
# Convertir reglas a dataframe para exportar
# df_reglas <- as(reglas, "data.frame")
# write.csv(df_reglas, "reglas_asociacion.csv")
################################################################################

"Soporte (1%): Al bajar el soporte de 0.1 a 0.01, notarás que el algoritmo encuentra muchos más resultados. Esto es porque ahora estás permitiendo productos que aparecen en al menos 98 transacciones (el 1% de las ~9,835 que tiene Groceries).

Confianza (30%): Indica que, si el antecedente (lado izquierdo) ocurre, hay un 30% de probabilidad de que el consecuente (lado derecho) también ocurra.

Interpretación del Lift: Si el lift es mayor a 1, significa que la relación entre los productos es más fuerte que si fueran independientes. Por ejemplo, si encuentras una regla {citrus fruit} => {whole milk} con un lift alto, indica que la compra de cítricos aumenta significativamente la probabilidad de comprar leche en comparación con el promedio general."


