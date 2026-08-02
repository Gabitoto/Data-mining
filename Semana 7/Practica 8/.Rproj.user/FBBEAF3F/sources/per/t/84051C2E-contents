
# Instalar y cargar el paquete
install.packages("arulesSequences", dependencies = TRUE)
library(arulesSequences)

# Cargar el dataset
data(zaki)

# ¿Qué clase de objeto es?
class(zaki)

# Resumen general
summary(zaki)

# ¿Cuántas secuencias?
length(zaki)

# ¿Cuántas transacciones totales?
sum(size(zaki))

# ¿Cuáles son los ítems únicos?
itemLabels(zaki)

# Inspeccionar todas las transacciones
inspect(zaki)

# Tamaño de cada transacción (cuántos ítems tiene cada una)
size(zaki)

# soporte 0.10
seq_010 <- cspade(zaki, parameter = list(support = 0.10))
cat("minSup=0.10:", length(seq_010), "secuencias frecuentes\n")
inspect1 <- as(seq_010, 'data.frame')
inspect1

# soporte 0.25
seq_025 <- cspade(zaki, parameter = list(support = 0.25))
cat("minSup=0.25:", length(seq_025), "secuencias frecuentes\n")
inspect2 <- as(seq_025, 'data.frame')
inspect2

# soporte 0.40
seq_040 <- cspade(zaki, parameter = list(support = 0.40))
cat("minSup=0.40:", length(seq_040), "secuencias frecuentes\n")
inspect3 <- as(seq_040, 'data.frame')
inspect3

summary(seq_040)

# soporte 0.70
seq_070 <- cspade(zaki, parameter = list(support = 0.70))
cat("minSup=0.70:", length(seq_070), "secuencias frecuentes\n")
inspect4 <- as(seq_070, 'data.frame')
inspect4

################################################################################

# ¿Cuáles son los ítems individuales más frecuentes? = 11A      10B      10F       8D      28(other)

# ¿Cuáles son los itemsets más frecuentes en los eventos? 8A       8D       4B       4F       4B,F       3other

# El conjunto de secuencias encontrado ordenado por el valor de soporte.

inspect(head(sort(seq_040, by = "support", decreasing = TRUE), 18))

################################################################################

soportes <- c(0.10, 0.25, 0.40, 0.70)

for (sup in soportes) {
  cat("\n=============================\n")
  cat("minSup =", sup, "\n")
  cat("=============================\n")
  
  seq_freq <- cspade(zaki, parameter = list(support = sup))
  reglas   <- ruleInduction(seq_freq, zaki, confidence = 0.80)
  
  cat("Total reglas encontradas:", length(reglas), "\n\n")
  
  q <- quality(reglas)
  rownames(q) <- NULL  # resetear indices discontinuos
  
  top_n <- min(10, nrow(q))
  top10_idx <- order(q$lift, decreasing = TRUE)[1:top_n]
  inspect(reglas[top10_idx])
}

