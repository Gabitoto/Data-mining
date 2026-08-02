# Cargamos las librerias que necesitamos
install.packages("pdftools")
install.packages("stringr")
library("pdftools") # Para pdf_text()
library("stringr")  # Para str_split_fixed()

datos <- pdf_text("./Datos/usbp_stats_fy2017_sector_profile.pdf")

hoja <- datos[1]

lineas <- strsplit(hoja, "\n") # \n es nueva línea

lineas <- lineas[[1]]

tabla <- lineas[10:35]

head(tabla)
# Eliminamos los espacios en blanco al inicio y final de las cadenas filas
tabla <- trimws(tabla)
# Verificamos
tabla

# Vamos a dividir las cadenas de las filas en 3 columnas y usamos como
# patrón en espacio " ", pero que aparezca 2 o más veces
tabla <- str_split_fixed(tabla, " {2,}", 10)
# Armamos un dataframe
# Desde la fila 2, porque la 1 es el encabezado de la tabla
tabla1 <- data.frame(matrix(tabla[1:nrow(tabla),],ncol=10))
# Le damos los nombres
mis_nombres <- c("SECTOR","Agent Staffing","Apprehensions", "Other Than Mexican Apprehensions","Marijuana","Cocaine","Accepted Prosecutions","Assaults", "Rescues","Deaths")

# Asignación directa
colnames(tabla1) <- mis_nombres

# Transformamos los datos
View(tabla1)

############################################################################################################################################
library("dplyr")

# Limpiamos la memoria
rm(list = ls())

datos <- pdf_text("./Datos/rep_covid-19_er_01_06_2022.pdf")

hoja <- datos[4]

lineas <- strsplit(hoja, "\n") # \n es nueva línea

lineas <- lineas[[1]]

tabla <- lineas[12:48]

head(tabla)
# Eliminamos los espacios en blanco al inicio y final de las cadenas filas
tabla <- trimws(tabla)
# Verificamos
tabla

# Vamos a dividir las cadenas de las filas en 3 columnas y usamos como
# patrón en espacio " ", pero que aparezca 2 o más veces
tabla <- str_split_fixed(tabla, " {2,}", 5)
# Armamos un dataframe
# Desde la fila 2, porque la 1 es el encabezado de la tabla
tabla1 <- data.frame(matrix(tabla[1:nrow(tabla),],ncol=5))
# Le damos los nombres
mis_nombres <- c("Departamentos","Conf_total_acumulados","Conf_ultimo_14_dias", "Fall_total_acumulados","Fall_ultimo_14_dias")

# Asignación directa
colnames(tabla1) <- mis_nombres

# Transformamos los datos
View(tabla1)

tabla_limpia <- tabla1 %>%
  filter(Departamentos != "")

# ahora lo guardamos en un CSV
write.csv(tabla_limpia, "./Datos/tabla_departamentos.csv", row.names = FALSE)

# ahora pasamos al dos ###################################################################

library(tidyr) # Fundamental para la función fill()
hojas_localidades <- datos[5:8] # Tomamos un rango para asegurarnos



# Unimos el texto de esas hojas y dividimos por líneas
lineas_loc <- unlist(strsplit(hojas_localidades, "\n"))



# Buscamos líneas que parezcan datos (que tengan al menos un número al final)
lineas_loc <- trimws(lineas_loc)



# Supongamos que en las líneas extraídas, las localidades están entre la 10 y la 200

# Nota: Ajustar estos índices según el contenido de 'lineas_loc'
tabla_loc_raw <- lineas_loc[5:282]



# Usamos el patrón de 2 o más espacios que ya aplicaste
matriz_loc <- str_split_fixed(tabla_loc_raw, " {2,}", 10)



# Conversión a Data Frame y limpieza
tabla_localidades <- as.data.frame(matriz_loc)

# 1. Partimos de la matriz cruda para evitar nombres duplicados
df_raw <- as.data.frame(matriz_loc, stringsAsFactors = FALSE)

# 2. Re-alineación lógica
# Analizamos la columna V2: si contiene números, la fila está desplazada a la izquierda.
df_alineado <- df_raw %>%
  mutate(
    # ¿V2 es un número? (Detecta si hay dígitos)
    es_desplazada = str_detect(V2, "\\d"),
    
    # Si está desplazada: el Depto es NA y la Localidad es V1
    # Si NO está desplazada: el Depto es V1 y la Localidad es V2
    Depto_Limpio = ifelse(es_desplazada, NA, V1),
    Loc_Limpia   = ifelse(es_desplazada, V1, V2),
    
    # Ajustamos los datos numéricos según el desplazamiento
    C2020 = ifelse(es_desplazada, V2, V3),
    F2020 = ifelse(es_desplazada, V3, V4),
    C2021 = ifelse(es_desplazada, V4, V5),
    F2021 = ifelse(es_desplazada, V5, V6),
    C2022 = ifelse(es_desplazada, V6, V7),
    F2022 = ifelse(es_desplazada, V7, V8)
  )

# 3. Aplicamos el relleno (Fill) y limpieza final
df_final <- df_alineado %>%
  # Seleccionamos solo las columnas nuevas y útiles
  select(Departamento = Depto_Limpio, Localidad = Loc_Limpia, 
         C2020, F2020, C2021, F2021, C2022, F2022) %>%
  
  # Rellenamos el nombre del departamento hacia abajo 
  fill(Departamento, .direction = "down") %>%
  
  # Filtramos filas que no son datos (encabezados de años o filas vacías)
  filter(!is.na(C2020), 
         !str_detect(Localidad, "2020|2021|Localidad|Total")) %>%
  
  # Convertimos a numérico (quitando puntos de miles si existen)
  mutate(across(C2020:F2022, ~as.numeric(str_replace_all(., "\\.", ""))))

# Verificamos el resultado
View(df_final)
