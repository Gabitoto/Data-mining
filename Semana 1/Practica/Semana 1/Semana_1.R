# Comenzamos con la descarga de datos y webscrapping.

## limpio la memoria
rm( list=ls() )
gc()
# Cargamos los paquetes que vamos a usar
library("data.table") # para usar fread()
#install.packages("rvest")
library("rvest")
library("dplyr")
#install.packages("robotstxt")
library("robotstxt")

# descargamos desde la URL
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00616/Tetuan%20City%20power%20consumption.csv"
# Verificamos el acceso a la página
paths_allowed(url)
archsalida <- "./Practica"
download.file(
  url = url, 
  destfile = archsalida
)
# lo abrimos 
datos <- fread(archsalida)
View(datos)
################################################## Segunda forma ###############

rm( list=ls() )

url <- "http://www.espn.com/nfl/superbowl/history/winners"
pagina <- read_html(url, encoding = "UTF-8")
selector <- "#my-players-table > div.mod-container.mod-table.mod-no-header-footer > div > table"
# Obtenemos en una lista el elemento tabla con el 
# selector css
# Por función
elemento <- html_elements(pagina, css = selector)
tabla1 <- html_table(elemento, dec = ".")
# Vemos la tabla con el primer elemento de la lista
View(tabla1[[1]])
# Con dplyr
tabla2 <- pagina %>%
  html_elements(css = selector) %>% 
  html_table(dec = ",")
View(tabla2[[1]])
df <- tabla2
# limpiamos la primera fila
nuevo_df <- df[-1, ]
View(nuevo_df)
columnas <- nuevo_df[1,]
nuevo_df1 <- nuevo_df[-1, ]
header <- columnas
colnames(nuevo_df1) <- header

###################################### tercera forma ###########################
#install.packages("httr")
#install.packages("purrr")
library(purrr)
library(httr)


rm( list=ls() )

url <- "https://statistics.stanford.edu/people/alumni"
# Verificamos el acceso a la página
paths_allowed(url)

# Leemos la página
pagina <- read_html(url, encoding = "UTF-8")
paginas <- 0:11

header <- ".hb-table-pattern__row"

header <- pagina %>% 
  html_elements(header) %>% 
  html_elements("div") %>% 
  html_text(trim = T)

cssTabla <- ".hb-table-pattern__body"
cssFila <-  ".hb-table-row"# div.hb-table-row:nth-child(1)
cssCamposAlumno <- ".hb-table-row__column" #div.hb-table-row:nth-child(1) > div:nth-child(1)

# Obtenemos los elementos de cada celda, de cada fila de la tabla
filas <- pagina %>% 
  html_elements(css = cssTabla) %>% 
  html_elements(css = cssFila) %>% 
  html_elements(css = cssCamposAlumno) %>% 
  html_text(trim = T)
# Armamos la tabla

tabla <- NULL
tabla <- data.frame(matrix(filas, ncol = length(header), 
                           byrow = T))
# Le agregamos los nombres a las columnas
colnames(tabla) <- header
head(tabla)

#1. Configuración inicial
url_base <- "https://statistics.stanford.edu/people/alumni"
paginas <- 0:11
lista_tablas <- list() # Creamos una lista vacía para guardar los resultados

# 2. El Bucle (Iteración)
for (i in paginas) {
  
  # Construimos la URL dinámica
  url_completa <- paste0(url_base, "?page=", i)
  print(paste("Procesando página:", i))
  
  # Leemos la página (R Base usa el motor interno de xml2)
  # Nota: Si te da error de conexión, Stanford te está pidiendo el User-Agent
  # que vimos antes. Sin httr es más difícil "engañarlo", pero probemos así:
  pagina <- try(read_html(url_completa, encoding = "UTF-8"))
  
  if (inherits(pagina, "try-error")) {
    message(paste("No se pudo acceder a la página", i))
    next # Salta a la siguiente iteración si hay error
  }
  
  # --- Tu lógica de extracción ---
  header_css <- ".hb-table-pattern__row"
  header <- pagina %>% 
    html_elements(header_css) %>% 
    html_elements("div") %>% 
    html_text(trim = TRUE)
  
  cssTabla <- ".hb-table-pattern__body"
  cssFila <- ".hb-table-row"
  cssCamposAlumno <- ".hb-table-row__column"
  
  filas <- pagina %>% 
    html_elements(css = cssTabla) %>% 
    html_elements(css = cssFila) %>% 
    html_elements(css = cssCamposAlumno) %>% 
    html_text(trim = TRUE)
  
  # Verificamos si hay datos antes de crear la matriz
  if (length(filas) > 0) {
    # Armamos la tabla de esta página
    tabla_pag <- data.frame(matrix(filas, ncol = length(header), byrow = TRUE))
    colnames(tabla_pag) <- header
    
    # Guardamos el data frame en nuestra lista
    lista_tablas[[i + 1]] <- tabla_pag
  }
  
  # Pausa de 2 segundos para no ser bloqueados
  Sys.sleep(2)
}

# 3. Consolidación Final (El "Merge")
# do.call aplica la función rbind a todos los elementos de la lista a la vez
tabla_final <- do.call(rbind, lista_tablas)

# Ver resultado
head(tabla_final)

# Arreglamos el año 2000 --> 



# Filtrado de años y acomodado:1980 - 2007

alumni_filtrado <- tabla_final %>%
  # Convertimos a numérico por si hay algún carácter extraño
  mutate(`Graduation Year` = as.numeric(`Graduation Year`)) %>%
  # Aplicamos el filtro solicitado (1980 - 2007)
  filter(`Graduation Year` >= 1980 & `Graduation Year` <= 2007) %>%
  # Ordenamos cronológicamente
  arrange(`Graduation Year`)

show(alumni_filtrado)



# guardamos los datos en un csv.
library("data.table")
fwrite(tabla_final, file = "./paginas-de-alumnos.csv", sep = ";")

###########################################################################################

rm( list=ls() )


