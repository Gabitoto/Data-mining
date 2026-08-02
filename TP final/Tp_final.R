#Liberamos la memoria
rm(list = ls())
gc()

# Cargamos librerias necesarías que facilitaran algunos aspectos de nuestro analisis 

if (!require("tidyverse"))  install.packages("tidyverse")
if (!require("skimr"))      install.packages("skimr")
if (!require("DataExplorer")) install.packages("DataExplorer")
if (!require("janitor"))    install.packages("janitor")
if (!require("scales"))     install.packages("scales")
if (!require("knitr"))      install.packages("knitr")

library(tidyverse)      # Manipulación y visualización de datos
library(skimr)          # Resumen estadístico completo y compacto
library(DataExplorer)   # EDA automático (missing values, distribuciones)
library(janitor)        # Limpieza de nombres de columnas
library(scales)         # Formateo de ejes en gráficos
library(knitr)          # Tablas formateadas

#Creamos un dataset sintetico con los nombres de las variables para poder graficarlo de manera correcta.

# 1. CARGAR EL CSV ORIGINAL
df <- read.csv("e-shop clothing 2008.csv", sep = ";", stringsAsFactors = FALSE)

#Breve exploración de datos
summary(df)
View(df)

#Verificamos si existen valores faltantes
colSums(is.na(df))

# Vemos la cantidad de valores unicos por cada variable
sapply(df, function(x) length(unique(x)))

# 2. RENOMBRAR COLUMNAS con nombres descriptivos

df <- df %>%
  rename(
    year               = year,
    month              = month,
    day                = day,
    order              = order,
    country            = country,
    session_id         = `session.ID`,          
    main_category      = `page.1..main.category.`,
    clothing_model     = `page.2..clothing.model.`,
    colour             = colour,
    location           = location,
    model_photography  = `model.photography`,
    price              = price,
    price_2            = `price.2`,
    page               = page
  )

# 3. REEMPLAZAR CÓDIGOS NUMÉRICOS POR ETIQUETAS
# (según el diccionario del archivo .txt)

# --- COUNTRY ---
df <- df %>%
  mutate(country = recode(as.character(country),
                          "1"  = "Australia",
                          "2"  = "Austria",
                          "3"  = "Belgium",
                          "4"  = "British Virgin Islands",
                          "5"  = "Cayman Islands",
                          "6"  = "Christmas Island",
                          "7"  = "Croatia",
                          "8"  = "Cyprus",
                          "9"  = "Czech Republic",
                          "10" = "Denmark",
                          "11" = "Estonia",
                          "12" = "Unidentified",
                          "13" = "Faroe Islands",
                          "14" = "Finland",
                          "15" = "France",
                          "16" = "Germany",
                          "17" = "Greece",
                          "18" = "Hungary",
                          "19" = "Iceland",
                          "20" = "India",
                          "21" = "Ireland",
                          "22" = "Italy",
                          "23" = "Latvia",
                          "24" = "Lithuania",
                          "25" = "Luxembourg",
                          "26" = "Mexico",
                          "27" = "Netherlands",
                          "28" = "Norway",
                          "29" = "Poland",
                          "30" = "Portugal",
                          "31" = "Romania",
                          "32" = "Russia",
                          "33" = "San Marino",
                          "34" = "Slovakia",
                          "35" = "Slovenia",
                          "36" = "Spain",
                          "37" = "Sweden",
                          "38" = "Switzerland",
                          "39" = "Ukraine",
                          "40" = "United Arab Emirates",
                          "41" = "United Kingdom",
                          "42" = "USA",
                          "43" = "biz",
                          "44" = "com",
                          "45" = "int",
                          "46" = "net",
                          "47" = "org"
  ))

# --- MAIN CATEGORY (page 1) ---
df <- df %>%
  mutate(main_category = recode(as.character(main_category),
                                "1" = "trousers",
                                "2" = "skirts",
                                "3" = "blouses",
                                "4" = "sale"
  ))

# --- COLOUR ---
df <- df %>%
  mutate(colour = recode(as.character(colour),
                         "1"  = "beige",
                         "2"  = "black",
                         "3"  = "blue",
                         "4"  = "brown",
                         "5"  = "burgundy",
                         "6"  = "gray",
                         "7"  = "green",
                         "8"  = "navy blue",
                         "9"  = "of many colors",
                         "10" = "olive",
                         "11" = "pink",
                         "12" = "red",
                         "13" = "violet",
                         "14" = "white"
  ))

# --- LOCATION ---
df <- df %>%
  mutate(location = recode(as.character(location),
                           "1" = "top left",
                           "2" = "top in the middle",
                           "3" = "top right",
                           "4" = "bottom left",
                           "5" = "bottom in the middle",
                           "6" = "bottom right"
  ))

# --- MODEL PHOTOGRAPHY ---
df <- df %>%
  mutate(model_photography = recode(as.character(model_photography),
                                    "1" = "en face",
                                    "2" = "profile"
  ))

# --- PRICE 2 ---
df <- df %>%
  mutate(price_2 = recode(as.character(price_2),
                          "1" = "yes",
                          "2" = "no"
  ))

glimpse(df)
head(df)


#Analizamos las variables numericas:

skim(df)

#Verificamos distribuciones de las variables numericas:

df %>%
  select(where(is.numeric)) %>%
  summary() %>%
  print()

#Vemos las frecuencias para variables categoricas:

vars_categoricas <- df %>% select(where(is.character)) %>% names()

for (vars in vars_categoricas) {
  cat("\n--- Variable:", vars, "---\n")
  df %>%
    count(.data[[vars]], sort = TRUE) %>%
    mutate(Porcentaje = round(n / sum(n) * 100, 2)) %>%
    print()
}

# --- 6a. Distribución de PRICE (variable numérica clave) ---

p1 <- ggplot(df, aes(x = price)) +
  geom_histogram(bins = 40, fill = "#4E79A7", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(price, na.rm = TRUE)),
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(price, na.rm = TRUE)),
             color = "darkgreen", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribución de Precios",
       subtitle = "Línea roja = Media | Línea verde = Mediana",
       x = "Precio (USD)", y = "Frecuencia") +
  theme_minimal()

print(p1)

# --- 6b. Distribución de sesiones por PAÍS (Top 15) ---
p2 <- df %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(country, n), y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#AED6F1", high = "#1A5276") +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 15 Países por Número de Registros",
       x = "País", y = "N° de Registros") +
  theme_minimal() +
  theme(legend.position = "none")

print(p2)

# --- 6c. Distribución por CATEGORÍA PRINCIPAL ---
p3 <- df %>%
  count(main_category, sort = TRUE) %>%
  ggplot(aes(x = reorder(main_category, n), y = n, fill = main_category)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Registros por Categoría Principal de Producto",
       x = "Categoría", y = "N° de Registros") +
  theme_minimal()

print(p3)

# --- 6d. Distribución por COLOR ---
p4 <- df %>%
  count(colour, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(colour, n), y = n)) +
  geom_col(fill = "#8E44AD", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 Colores Más Visitados",
       x = "Color", y = "N° de Registros") +
  theme_minimal()

print(p4)

# --- 6e. Actividad por MES ---
p5 <- df %>%
  count(month) %>%
  ggplot(aes(x = factor(month), y = n, group = 1)) +
  geom_line(color = "#E74C3C", linewidth = 1.2) +
  geom_point(color = "#E74C3C", size = 3) +
  scale_y_continuous(labels = comma) +
  labs(title = "Actividad de Navegación por Mes",
       x = "Mes", y = "N° de Registros") +
  theme_minimal()

print(p5)

# --- 6f. Price vs. Categoría (Boxplot) ---
# Analogía: comparar el "rango de precios" de distintas secciones de la tienda
p6 <- ggplot(df, aes(x = main_category, y = price, fill = main_category)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.3) +
  labs(title = "Distribución de Precios por Categoría Principal",
       x = "Categoría", y = "Precio (USD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p6)

# --- Análisis de Correlaciones y Varianza de Variables Cuantitativas ---

# 1. Seleccionar solo las variables numéricas
df_numericas <- df %>% select(where(is.numeric))

cat("\n--- Varianza de las Variables Numéricas ---\n")
varianzas <- sapply(df_numericas, var, na.rm = TRUE)
print(varianzas)

cat("\n--- Matriz de Correlación entre Variables Numéricas ---\n")
correlaciones <- cor(df_numericas, use = "pairwise.complete.obs")
print(correlaciones)

# Para una visualización más amigable de la correlación
if (!require("corrplot")) install.packages("corrplot", dependencies = TRUE)
library(corrplot)
cat("\n--- Visualización de la Matriz de Correlación ---\n")
corrplot(correlaciones, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

# --- Visualizaciones de Scatterplots entre Variables Cuantitativas ---
library(ggplot2)
library(patchwork)

# 1. Scatterplot: Precio vs. Página
# ¿Los productos más caros están en las primeras páginas?
p_scatter1 <- ggplot(df, aes(x = page, y = price)) +
  geom_jitter(alpha = 0.1, color = "#2E86AB") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación: Precio vs. Página",
       x = "Número de Página",
       y = "Precio (USD)") +
  theme_minimal()

# 2. Scatterplot: Orden (Secuencia de clicks) vs. Página
# ¿A medida que avanza la sesión, el usuario llega a páginas más profundas?
p_scatter2 <- ggplot(df, aes(x = order, y = page)) +
  geom_jitter(alpha = 0.1, color = "#2A9D8F") +
  geom_smooth(method = "lm", color = "orange") +
  labs(title = "Relación: Orden de Click vs. Página",
       x = "Orden (Secuencia de clicks)",
       y = "Número de Página") +
  theme_minimal()

# Mostramos ambos gráficos
(p_scatter1 | p_scatter2) +
  plot_annotation(title = "Análisis de Dispersión entre Variables Numéricas")

# Pasamos a visualizar:
# Clicks por sesión,
# sesiones por país,
# productos vistos por sesión
# categoría de producto por sesión

sesiones <- df %>%
  group_by(session_id) %>%
  summarise(
    pais             = first(country),
    clicks_sesion    = n(),
    productos_vistos = n_distinct(clothing_model, na.rm = TRUE),
    categoria_moda   = names(sort(table(main_category), decreasing = TRUE))[1]
  ) %>%
  ungroup()

# GRÁFICO 1: CLICKS POR SESIÓN
# Histograma — muestra cuántos clicks hace cada cliente

ggplot(sesiones, aes(x = clicks_sesion)) +
  geom_histogram(
    bins  = 35,
    fill  = "#2E86AB",
    color = "white",
    alpha = 0.9
  ) +
  geom_vline(
    aes(xintercept = median(clicks_sesion)),
    color = "#E84855", linetype = "dashed", linewidth = 1
  ) +
  geom_vline(
    aes(xintercept = mean(clicks_sesion)),
    color = "#F4A261", linetype = "dashed", linewidth = 1
  ) +
  annotate("text",
           x = median(sesiones$clicks_sesion) + 0.5, y = Inf,
           label = paste0("Mediana: ", round(median(sesiones$clicks_sesion), 1)),
           color = "#E84855", hjust = 0, vjust = 2, size = 3.5
  ) +
  annotate("text",
           x = mean(sesiones$clicks_sesion) + 0.5, y = Inf,
           label = paste0("Media: ", round(mean(sesiones$clicks_sesion), 1)),
           color = "#F4A261", hjust = 0, vjust = 4, size = 3.5
  ) +
  scale_x_continuous(limits = c(0, quantile(sesiones$clicks_sesion, 0.99))) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Distribución de clicks por sesión",
    subtitle = "Cada barra = cantidad de sesiones con ese número de clicks",
    x        = "Clicks en la sesión",
    y        = "Número de sesiones",
    caption  = "Recortado al percentil 99 para excluir outliers extremos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# GRÁFICO 2: SESIONES POR PAÍS
# Barras horizontales — Top 15 países con más sesiones

sesiones %>%
  count(pais, sort = TRUE) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(pais, n), y = n, fill = n)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = comma(n)),
    hjust = -0.15, size = 3.2, color = "gray30"
  ) +
  coord_flip() +
  scale_fill_gradient(low = "#A8DADC", high = "#1D3557") +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title    = "Sesiones por país (Top 15)",
    subtitle = "Número de sesiones únicas originadas en cada país",
    x        = NULL,
    y        = "Número de sesiones"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold")
  )

# GRÁFICO 3: PRODUCTOS VISTOS POR SESIÓN
# Violin + boxplot — muestra tanto la forma de la distribución
# como los valores de posición central

ggplot(sesiones, aes(x = "", y = productos_vistos)) +
  geom_violin(
    fill  = "#A8DADC",
    color = "#457B9D",
    alpha = 0.7,
    trim  = TRUE
  ) +
  geom_boxplot(
    width         = 0.18,
    fill          = "#457B9D",
    color         = "white",
    outlier.shape = 16,
    outlier.alpha = 0.25,
    outlier.size  = 1
  ) +
  scale_y_continuous(
    limits = c(0, quantile(sesiones$productos_vistos, 0.99, na.rm = TRUE))
  ) +
  labs(
    title    = "Productos únicos vistos por sesión",
    subtitle = "Violin: forma de la distribución | Caja: mediana e IQR",
    x        = NULL,
    y        = "Cantidad de productos distintos",
    caption  = "Recortado al percentil 99"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    plot.title  = element_text(face = "bold")
  )

# GRÁFICO 4: CATEGORÍA DE PRODUCTO POR SESIÓN
# Barras horizontales con porcentaje

sesiones %>%
  count(categoria_moda, sort = TRUE) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 1),
    etiqueta   = paste0(comma(n), "  (", porcentaje, "%)")
  ) %>%
  ggplot(aes(x = reorder(categoria_moda, n), y = n, fill = categoria_moda)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = etiqueta),
    hjust  = -0.08,
    size   = 3.5,
    color  = "gray30"
  ) +
  coord_flip() +
  scale_fill_manual(values = c(
    "trousers" = "#E63946",
    "skirts"   = "#F4A261",
    "blouses"  = "#2A9D8F",
    "sale"     = "#457B9D"
  )) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title    = "Categoría principal de producto por sesión",
    subtitle = "Categoría más visitada dentro de cada sesión de navegación",
    x        = NULL,
    y        = "Número de sesiones"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# EVOLUCIÓN DE CLICKS A LO LARGO DE LOS MESES
# ¿Cómo ha sido la evolución de los clicks de navegación a lo largo de los meses estudiados?


library(tidyverse)
library(scales)

# Agregamos clicks totales por mes

clicks_mes <- df %>%
  group_by(month) %>%
  summarise(
    total_clicks   = n(),
    sesiones_unicas = n_distinct(session_id),
    clicks_por_sesion = round(total_clicks / sesiones_unicas, 2)
  ) %>%
  ungroup() %>%
  mutate(
    month_label = factor(month,
                         levels = 4:8,
                         labels = c("Abril", "Mayo", "Junio", "Julio", "Agosto")
    )
  )

# Ver tabla resumen
print(clicks_mes)

# GRÁFICO: Clicks totales por mes

ggplot(clicks_mes, aes(x = month_label)) +
  # Barras: volumen absoluto
  geom_col(
    aes(y = total_clicks),
    fill  = "#2E86AB",
    alpha = 0.75,
    width = 0.6
  ) +
  # Línea encima: tendencia visual
  geom_line(
    aes(y = total_clicks, group = 1),
    color    = "#E84855",
    linewidth = 1.2
  ) +
  geom_point(
    aes(y = total_clicks),
    color = "#E84855",
    size  = 3.5
  ) +
  # Etiquetas sobre cada barra
  geom_text(
    aes(y = total_clicks, label = comma(total_clicks)),
    vjust = -0.8,
    size  = 3.5,
    color = "gray30"
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title    = "Evolución de clicks de navegación por mes",
    subtitle = "Abril – Agosto 2008 | Clicks totales registrados en el e-shop",
    x        = "Mes",
    y        = "Total de clicks",
    caption  = "Fuente: e-shop clothing 2008"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# GRÁFICO: Clicks PROMEDIO por sesión por mes
# Esto responde: ¿los clientes navegaban más o menos
# intensamente en cada mes? (calidad de visita, no volumen)

ggplot(clicks_mes, aes(x = month_label, y = clicks_por_sesion, group = 1)) +
  geom_line(color = "#2A9D8F", linewidth = 1.3) +
  geom_point(
    color = "#2A9D8F",
    size  = 4,
    shape = 21,
    fill  = "white",
    stroke = 2
  ) +
  geom_text(
    aes(label = clicks_por_sesion),
    vjust = -1.2,
    size  = 3.5,
    color = "gray30"
  ) +
  scale_y_continuous(
    limits = c(0, max(clicks_mes$clicks_por_sesion) * 1.2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Clicks promedio por sesión según mes",
    subtitle = "¿Qué tan intensamente navegaba cada cliente según el mes?",
    x        = "Mes",
    y        = "Clicks promedio por sesión",
    caption  = "Fuente: e-shop clothing 2008"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# GRÁFICO Sesiones únicas por mes
# Distingue si el volumen de clicks sube por más clientes
# o porque cada cliente clickea más

ggplot(clicks_mes, aes(x = month_label, y = sesiones_unicas, group = 1)) +
  geom_col(fill = "#F4A261", alpha = 0.8, width = 0.6) +
  geom_line(color = "#E76F51", linewidth = 1.2) +
  geom_point(color = "#E76F51", size = 3.5) +
  geom_text(
    aes(label = comma(sesiones_unicas)),
    vjust = -0.8, size = 3.5, color = "gray30"
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title    = "Sesiones únicas por mes",
    subtitle = "Cantidad de visitas distintas al e-shop por mes",
    x        = "Mes",
    y        = "Sesiones únicas",
    caption  = "Fuente: e-shop clothing 2008"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# PANEL COMBINADO
# Requiere: install.packages("patchwork")

# Install the patchwork package if it's not already installed
if (!require(patchwork)) install.packages("patchwork")
library(patchwork)

g1 <- ggplot(clicks_mes, aes(x = month_label)) +
  geom_col(aes(y = total_clicks), fill = "#2E86AB", alpha = 0.75, width = 0.6) +
  geom_line(aes(y = total_clicks, group = 1), color = "#E84855", linewidth = 1.2) +
  geom_point(aes(y = total_clicks), color = "#E84855", size = 3) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Clicks totales", x = NULL, y = "Total clicks") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

g2 <- ggplot(clicks_mes, aes(x = month_label, y = clicks_por_sesion, group = 1)) +
  geom_line(color = "#2A9D8F", linewidth = 1.3) +
  geom_point(color = "#2A9D8F", size = 3, shape = 21, fill = "white", stroke = 2) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Clicks promedio por sesión", x = NULL, y = "Clicks / sesión") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

g3 <- ggplot(clicks_mes, aes(x = month_label, y = sesiones_unicas, group = 1)) +
  geom_col(fill = "#F4A261", alpha = 0.8, width = 0.6) +
  geom_line(color = "#E76F51", linewidth = 1.2) +
  geom_point(color = "#E76F51", size = 3) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Sesiones únicas", x = NULL, y = "Sesiones") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

(g1 | g2 | g3) +
  plot_annotation(
    title    = "Evolución de la navegación por mes — E-shop Ropa Maternidad 2008",
    subtitle = "Abril a Agosto 2008",
    theme    = theme(
      plot.title    = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray50")
    )
  )


# CONSIGNAS D : H
#--------------APLICACION DE MODELOS----------------------

# Comenzamos convirtiendo el dataset a formato de Transacciones:

# 1. TRANSACCIONES = sesiones únicas
# Cada sesión es un "ticket de compra"
n_transacciones <- n_distinct(df$session_id)
cat("Número de transacciones (sesiones únicas):", n_transacciones, "\n")

# 2. ÍTEMS = productos únicos vistos en todo el dataset
n_items <- n_distinct(df$clothing_model, na.rm = TRUE)
cat("Número de ítems únicos (productos):", n_items, "\n")

# 3. RESUMEN POR TRANSACCIÓN
# Cuántos ítems distintos tiene cada "ticket"
transacciones <- df %>%
  group_by(session_id) %>%
  summarise(
    n_items_sesion = n_distinct(clothing_model, na.rm = TRUE)
  ) %>%
  ungroup()
# Estadísticas del tamaño de las transacciones
cat("\n--- Tamaño de las transacciones (ítems por sesión) ---\n")
summary(transacciones$n_items_sesion)

# CONSIGNA E) ITEMSETS FRECUENTES 
# SOPORTE 2% | LONGITUD MIN. 2

if (!require("arules"))  install.packages("arules")

# PREPARAR TRANSACCIONES Y EJECUTAR ECLAT

library(tidyverse)
library(arules)

# --- Formato transaccional (una bolsa por sesión) ---
transacciones_lista <- df %>%
  filter(!is.na(clothing_model)) %>%
  group_by(session_id) %>%
  summarise(items = list(unique(as.character(clothing_model)))) %>%
  pull(items)

# Convertir a objeto transactions
trans <- as(transacciones_lista, "transactions")

cat("Transacciones:", length(trans), "\n")
cat("Ítems únicos:", nitems(trans), "\n")

# --- Ejecutar ECLAT ---
itemsets_eclat <- eclat(
  trans,
  parameter = list(
    support = 0.02,   # 2% soporte mínimo
    minlen  = 2,      # longitud mínima 2 ítems
    maxlen  = 10
  )
)

cat("\nItemsets encontrados:", length(itemsets_eclat), "\n\n")
inspect(sort(itemsets_eclat, by = "support", decreasing = TRUE)[1:20])

# CONSIGNA F) REGLAS DE ASOCIACIÓN — Polonia, categoría Blouses
# Soporte 2% | Confianza 20% | Top 10 por soporte

library(tidyverse)
library(arules)

# --- Filtrar solo Polonia y blouses ---
df_filtrado <- df %>%
  filter(country == "Poland",
         main_category == "blouses")

cat("Registros filtrados:", nrow(df_filtrado), "\n")
cat("Sesiones únicas:    ", n_distinct(df_filtrado$session_id), "\n")

# --- Formato transaccional ---
trans_polonia <- df_filtrado %>%
  filter(!is.na(clothing_model)) %>%
  mutate(clothing_model = as.character(clothing_model)) %>%
  group_by(session_id) %>%
  summarise(items = list(unique(clothing_model))) %>%
  pull(items) %>%
  as(., "transactions")

cat("Transacciones:", length(trans_polonia), "\n")
cat("Ítems únicos: ", nitems(trans_polonia), "\n")

# --- ECLAT: itemsets frecuentes ---
itemsets <- eclat(
  trans_polonia,
  parameter = list(
    support = 0.02,
    minlen  = 2,
    maxlen  = 10
  )
)

cat("Itemsets encontrados:", length(itemsets), "\n")

# --- Generar reglas de asociación ---
reglas <- ruleInduction(itemsets, trans_polonia, confidence = 0.20)

cat("Reglas generadas:", length(reglas), "\n")

# --- Top 10 por soporte ---
top10 <- sort(reglas, by = "support", decreasing = TRUE)[1:min(10, length(reglas))]
inspect(top10)

# CONSIGNA G) REGLAS DE ASOCIACIÓN — República Checa, categoría Blouses
# Soporte 4% | Confianza 25% | Top 10 por soporte

# --- Filtrar solo República Checa y blouses ---
df_filtrado_cz <- df %>%
  filter(country == "Czech Republic",
         main_category == "blouses")

cat("Registros filtrados:", nrow(df_filtrado_cz), "\n")
cat("Sesiones únicas:    ", n_distinct(df_filtrado_cz$session_id), "\n")

# --- Formato transaccional ---
trans_cz <- df_filtrado_cz %>%
  filter(!is.na(clothing_model)) %>%
  mutate(clothing_model = as.character(clothing_model)) %>%
  group_by(session_id) %>%
  summarise(items = list(unique(clothing_model))) %>%
  pull(items) %>%
  as(., "transactions")

cat("Transacciones:", length(trans_cz), "\n")
cat("Ítems únicos: ", nitems(trans_cz), "\n")

# --- ECLAT: itemsets frecuentes ---
itemsets_cz <- eclat(
  trans_cz,
  parameter = list(
    support = 0.04,
    minlen  = 2,
    maxlen  = 10
  )
)

cat("Itemsets encontrados:", length(itemsets_cz), "\n")

# --- Generar reglas ---
reglas_cz <- ruleInduction(itemsets_cz, trans_cz, confidence = 0.25)

cat("Reglas generadas:", length(reglas_cz), "\n")

# --- Top 10 por soporte ---
top10_cz <- sort(reglas_cz, by = "support", decreasing = TRUE)[1:min(10, length(reglas_cz))]
inspect(top10_cz)

# CONSIGNA I — SPADE
# Secuencias frecuentes: más de 1 ítem, soporte > 2%

if (!require("arulesSequences"))  install.packages("arulesSequences")

library(tidyverse)
library(arulesSequences)

# --- Crear sequences.txt desde df ---
df %>%
  filter(!is.na(clothing_model)) %>%
  select(
    sequenceID = session_id,
    eventID    = order,
    items      = clothing_model
  ) %>%
  arrange(sequenceID, eventID) %>%
  mutate(sequenceID = as.integer(factor(sequenceID))) %>%
  group_by(sequenceID, eventID) %>%
  summarise(items = list(items), .groups = "drop") %>%
  rowwise() %>%
  mutate(linea = paste(sequenceID, eventID, length(items),
                       paste(items, collapse = " "))) %>%
  pull(linea) %>%
  writeLines("sequences.txt")

cat("sequences.txt generado\n")
readLines("sequences.txt", n = 5)

# --- Cargar ---
trans <- read_baskets("sequences.txt",
                      sep  = " ",
                      info = c("sequenceID", "eventID", "size"))

cat("Transacciones:", length(trans), "\n")
cat("Ítems únicos:", length(itemLabels(trans)), "\n")

# --- Ejecutar SPADE ---
seq_freq <- cspade(trans,
                   parameter = list(support = 0.02), # Removed minsize as it's not a valid parameter for SPparameter class
                   control   = list(verbose = TRUE))

cat("\nSecuencias frecuentes encontradas:", length(seq_freq), "\n")

# --- Top 20 por soporte ---
seq_ord <- sort(seq_freq, by = "support", decreasing = TRUE)
inspect(seq_ord[1:min(20, length(seq_ord))])

# --- Data frame para análisis ---
df_spade <- as(seq_freq, "data.frame") %>%
  arrange(desc(support)) %>%
  mutate(soporte_n = round(support * length(trans)))

print(head(df_spade, 20))

# --- Probar con soporte más bajo para encontrar secuencias de 2+ ítems ---
seq_freq2 <- cspade(trans,
                    parameter = list(support = 0.005),  # 0.5%
                    control   = list(verbose = TRUE))

cat("Secuencias totales:", length(seq_freq2), "\n")

# Filtrar longitud > 1
seq_multi <- seq_freq2[size(seq_freq2) > 1]
cat("Secuencias con más de 1 ítem:", length(seq_multi), "\n")

# Ver cuántas hay por longitud
cat("\nDistribución por longitud:\n")
print(table(size(seq_multi)))

# Top 20 por soporte
if(length(seq_multi) > 0) {
  seq_ord <- sort(seq_multi, by = "support", decreasing = TRUE)
  inspect(seq_ord[1:min(20, length(seq_ord))])
}

# --- Filtrar secuencias con soporte > 2% y longitud > 1 ---
seq_filtradas <- seq_freq2[size(seq_freq2) > 1 &
                             quality(seq_freq2)$support > 0.02]

cat("Secuencias con más de 1 ítem y soporte > 2%:", length(seq_filtradas), "\n")

# Distribución por longitud
cat("\nDistribución por longitud:\n")
print(table(size(seq_filtradas)))

# Top 20 por soporte
seq_ord <- sort(seq_filtradas, by = "support", decreasing = TRUE)
inspect(seq_ord[1:min(20, length(seq_ord))])

# Data frame final
df_spade_final <- as(seq_filtradas, "data.frame") %>%
  arrange(desc(support)) %>%
  mutate(
    longitud  = size(seq_filtradas),
    soporte_n = round(support * length(trans))
  )

print(df_spade_final)
