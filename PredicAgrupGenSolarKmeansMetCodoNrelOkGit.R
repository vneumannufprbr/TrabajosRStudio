# Instalar y cargar bibliotecas (si es necesario)
if(!require(solaR)) install.packages("solaR")
if(!require(lubridate)) install.packages("lubridate")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(cluster)) install.packages("cluster")
if(!require(factoextra)) install.packages("factoextra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(httr)) install.packages("httr")
if(!require(jsonlite)) install.packages("jsonlite")

library(solaR)
library(lubridate)
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyr)
library(httr)
library(jsonlite)

# Parámetros de la API (¡REEMPLAZA con tus credenciales!)
api_key <- "Yhtat7En22F0EBsE5ZPcirha6IrSvwIObWcg9ght"  # Tu clave de API de NREL
lat <- 37.0  # Latitud: California
lon <- -119.0
#lat <- 37.7749 # Latitud: San Francisco, California
#lon <- -122.4194 
#lat <- 40.7128  # Latitud (por ejemplo, Nueva York)
#lon <- -74.0060
year <- "2020" # Como cadena
email <- "vneumann@ufpr.br"

# Construir la URL (corregida)
url <- paste0("https://developer.nrel.gov/api/solar/nsrdb_psm3_download.csv",
              "?wkt=POINT(", lon, "%20", lat, ")",
              "&names=", year,
              "&leap_day=false",
              "&interval=60",
              "&utc=true",
              "&api_key=", api_key,
              "&email=", email)

# Imprimir la URL para verificar (opcional pero recomendado)
print(url)

# Descargar los datos (con skip = 2 y check.names = FALSE)
data <- tryCatch({
  read.csv(url, skip = 2, check.names = FALSE) # Saltar primera fila, permitir nombres duplicados
},
error = function(e) {
  stop("Error al descargar datos: ", e$message, "\nVerifica la URL y tu clave API.")
}
)

# Imprimir nombres de columnas para verificar
print(names(data))

# Crear la columna Date
data$Date <- with(data, ymd_hm(paste(Year, Month, Day, Hour, Minute, sep = "-")))

# Asegurarse de que Date esté en formato POSIXct
data$Date <- as.POSIXct(data$Date, tz = "UTC")

# Seleccionar y renombrar columnas (GHI y Temperatura del Aire)
data <- data %>%
  select(Date, GHI, Temperature)

# Preprocesar los datos (escalar)
scaled_data <- scale(data %>% select(GHI, Temperature))

# Clustering con K-means
set.seed(123)

# Método del codo
set.seed(123) # Para reproducibilidad
wcss <- vector() # Vector para almacenar los valores de WCSS
for (i in 1:10) { # Probar k de 1 a 10 (puedes ajustar el rango)
  kmeans_result <- kmeans(scaled_data, centers = i, nstart = 25)
  wcss[i] <- kmeans_result$tot.withinss
}

# Graficar el codo
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Número de clusters (k)",
     ylab = "WCSS (Suma total de cuadrados dentro de clusters)")

# Calcular la segunda derivada
d2wcss <- diff(diff(wcss))

# Encontrar el k con la mayor segunda derivada
k <- which.max(d2wcss) + 1  # +1 porque diff() reduce la longitud del vector

kmeans_result <- kmeans(scaled_data, centers = k, nstart = 25)

data$Cluster <- as.factor(kmeans_result$cluster)

# Visualización
ggplot(data, aes(x = GHI, y = Temperature, color = Cluster)) +
  geom_point() +
  labs(title = "Clusters de Condiciones Meteorológicas (NREL)",
       x = "Irradiancia Global Horizontal (GHI)",
       y = "Temperatura del Aire (°C)",
       color = "Cluster") +
  theme_minimal()

# Predicción
new_data <- data.frame(GHI = 500, Temperature = 20)
scaled_new_data <- scale(new_data, 
                         center = attr(scaled_data, "scaled:center"), 
                         scale = attr(scaled_data, "scaled:scale"))

distances <- as.matrix(dist(rbind(scaled_new_data, kmeans_result$centers)))[1, -(1:nrow(scaled_new_data))]
predicted_cluster <- which.min(distances)
cat("El nuevo punto pertenece al cluster:", predicted_cluster, "\n")

