# Actividad 2 de Practica N4

# Instalar y cargar el paquete necesario
if(!require(rmatio)) install.packages("rmatio")
library(rmatio)

# 1. Cargar el dataset original de intervalos RR (ajusta el nombre del archivo)
df_rr <- read.csv("datasetRR.csv")

# Para cada una de las 200 señales, necesitamos obtener:Tiempo total ($T$): Dado que la frecuencia de muestreo ($f_s$) es de $300\text{ Hz}$, el tiempo se calcula como $T = \frac{\text{cantidad de muestras}}{f_s}$.Estadísticas de Amplitud (mV): Mínimo, máximo, media, mediana y desvío estándar.

# Obtengo la lista de archivos
archivos <- list.files(path = "ECGs", pattern = "\\.mat$", full.names = TRUE)

# Uso una LISTA para almacenar las señales crudas
lista_señales <- list()

# Creo un DATAFRAME para las estadísticas
df_estadisticas <- data.frame()

for (i in 1:length(archivos)) {
  # Leer el archivo .mat
  mat_data <- read.mat(archivos[i])
  
  # Extraer el vector de amplitud (ajustar el nombre del campo según el archivo)
  señal_cruda <- mat_data[[1]] 
  
  # Guardar la señal cruda en la lista (acepta diferentes longitudes)
  lista_señales[[i]] <- señal_cruda
  
  # Calcular las variables requeridas 
  # Frecuencia de muestreo = 300 Hz
  tiempo_seg <- length(señal_cruda) / 300 
  
  # Crear una fila con las métricas
  resumen_señal <- data.frame(
    Archivo = basename(archivos[i]),
    Tiempo_Total = tiempo_seg,
    Minimo = min(señal_cruda),
    Maximo = max(señal_cruda),
    Media = mean(señal_cruda),
    Mediana = median(señal_cruda),
    Desvio_Estandar = sd(señal_cruda)
  )
  
  # Unir al dataframe de estadísticas
  df_estadisticas <- rbind(df_estadisticas, resumen_señal)
}

last_df <- cbind(df_rr,df_estadisticas)

# Elimino la columna archivo de este ultimo df:

df_finalissima <- last_df %>% 
  select(-Archivo)

