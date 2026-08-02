# Ejemplo 1
## limpio la memoria
rm( list=ls() )
gc()
# Cargamos los paquetes que vamos a usar
library("data.table") # para usar fread()
# Descargamos los datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
archsalida <- "./datos/iris2.data"

download.file(
  url = url, 
  destfile = archsalida
)
# Abrimos el archivo
datos <- fread(archsalida)
#----
# Ejemplo 2
## limpio la memoria
rm( list=ls() )
# Cargamos los paquetes que vamos a usar
#install.packages("rvest")
library("rvest")
library("dplyr")
library("robotstxt")

# a) Obtenemos una tabla
url <- "https://ourworldindata.org/the-our-world-in-data-dataset-of-famines"
# La URL vieja (ahora tiene nuevas gráficas): "https://ourworldindata.org/famines#the-our-world-in-data-dataset-of-famines"
# Verificamos el acceso a la página
paths_allowed(url)
# Leemos la página
pagina <- read_html(url, encoding = "UTF-8")
# Identificador CSS de la tabla
# .article-block__table--wide > table:nth-child(1)
css <- ".article-block__table--wide > table:nth-child(1)"
 # .article-block__table--wide > table:nth-child(1)
# Inspeccionar
# Firefox: selector CSS
# Brave, Edge: Copy selector
# "#owid-document-root > article > div.article-block__table--wide.col-start-2.span-cols-12.article-block__table--header-row > table"

# Obtenemos en una lista el elemento tabla con el 
# selector css
# Por función
elemento <- html_elements(pagina, css = css)
tabla1 <- html_table(elemento, dec = ".")
# Vemos la tabla con el primer elemento de la lista
# <table>xxxxxxxxx</table>
View(tabla1[[1]])
# Con dplyr
tabla2 <- pagina %>%
  html_elements(css = css) %>% 
  html_table(dec = ".")
# Vemos la tabla con el primer elemento de la lista
# <table>xxxxxxxxx</table>
View(tabla2[[1]])
#Devuelve un formato de lista que en el primer elemento tiene la tabla

datos <- tabla1[[1]]
# Guardamos los datos
fwrite(datos, "./datos/famines.csv", sep = ";")
# Vemos el tipo de los datos
str(datos)
#----
# Ejemplo 3
## limpio la memoria
rm( list=ls() )
gc()
# Cargamos los paquetes que vamos a usar
#install.packages("rvest")
library("rvest")
library("dplyr")
#install.packages("robotstxt")
library("robotstxt")
# a) Obtenemos una tabla
# Verificamos el acceso a la página
paths_allowed("https://statistics.stanford.edu/people/alumni")
# Cargamos la primera página y extraemos la info. Le agregamos la 
# primera pagina de datos
pagina <- read_html("https://statistics.stanford.edu/people/alumni?page=0")
# Definimos el elemento CSS del encabezado de la tabla
cssHeader <- ".hb-table-pattern__row"
# .hb-table-pattern__header
#.hb-table-pattern__row
header <- pagina %>% 
  html_elements(cssHeader) %>% 
  html_elements("div") %>% 
  html_text(trim = T)
# Definimos los CSS de los campos de interés 
cssTabla <- ".hb-table-pattern__body" # .hb-table-pattern__body
cssFila <-  ".hb-table-row"# div.hb-table-row:nth-child(1)
cssCamposAlumno <- ".hb-table-row__column" #div.hb-table-row:nth-child(1) > div:nth-child(1)
#
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
# Guardamos nuestra tabla
library("data.table")
fwrite(tabla, file = "./salidas/pagina01Alumnos.csv", sep = ";")
# b) Obtenemos las primeras 3 páginas
# Extraemos los datos de las primeras 3 páginas de alumnos
path <- "https://statistics.stanford.edu/people/alumni?page="
ini <- 0
fin <- 2
filas <- NULL
for(i in 0:(fin-ini)){
  pagina <- read_html(paste0(path,i))
  aux <- pagina %>% 
    html_elements(css = cssTabla) %>% 
    html_elements(css = cssFila) %>% 
    html_elements(css = cssCamposAlumno) %>% 
    html_text(trim = T)
  filas <- c(filas, aux)
}
tabla <- data.frame(matrix(filas, ncol = length(header), 
                           byrow = T))
colnames(tabla) <- header
head(tabla)
tail(tabla)
# Guardamos nuestra tabla completa
library("data.table")
fwrite(tabla, file = "./salidas/pagina1-3Alumnos.csv", sep = ";")
#-------------------------------------------------------------------------------

