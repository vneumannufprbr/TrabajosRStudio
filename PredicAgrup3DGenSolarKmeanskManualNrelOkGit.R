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
if(!require(plotly)) install.packages("plotly")
library(plotly)

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
lat <- 25.7617  # Miami
lon <- -80.1918
year <- "2020"
#lat <- 37.7749 # Latitud: San Francisco, California
#lon <- -122.4194 
#lat <- 40.7128  # Latitud (por ejemplo, Nueva York)#
#lon <- -74.0060
#year <- "2020" # Como cadena
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


# Procesamiento
data$Date <- with(data, ymd_hm(paste(Year, Month, Day, Hour, Minute, sep = "-")))
data$Date <- as.POSIXct(data$Date, tz = "UTC")

# Selección y escalado
data <- data %>% select(Date, GHI, Temperature, `Relative Humidity`)
scaled_data <- scale(data %>% select(GHI, Temperature, `Relative Humidity`))

# K-means clustering con k = 4
set.seed(123)
k <- 4
kmeans_result <- kmeans(scaled_data, centers = k, nstart = 25)
data$Cluster <- as.factor(kmeans_result$cluster)

# Gráfica 3D con plotly
plot_ly(data = data, 
        x = ~GHI, 
        y = ~Temperature, 
        z = ~`Relative Humidity`, 
        color = ~Cluster, 
        colors = "Set1", 
        type = "scatter3d", 
        mode = "markers") %>%
  layout(title = "Clusters 3D de Condiciones Meteorológicas (Miami, 2020)",
         scene = list(xaxis = list(title = "GHI"),
                      yaxis = list(title = "Temperatura (°C)"),
                      zaxis = list(title = "Humedad Relativa (%)")))

# 🔮 Nuevo punto a predecir
new_data <- data.frame(GHI = 600, Temperature = 28, Relative.Humidity = 65)

# Asegurar que los nombres de columna coincidan con los usados en el escalado original
colnames(new_data) <- c("GHI", "Temperature", "Relative Humidity")

# Escalar el nuevo punto con los mismos parámetros usados anteriormente
scaled_new_data <- scale(new_data, 
                         center = attr(scaled_data, "scaled:center"), 
                         scale = attr(scaled_data, "scaled:scale"))

# Calcular distancias a los centroides
distances <- as.matrix(dist(rbind(scaled_new_data, kmeans_result$centers)))[1, -(1:nrow(scaled_new_data))]

# Determinar el cluster más cercano
predicted_cluster <- which.min(distances)

# Mostrar el resultado
cat("✅ El nuevo punto pertenece al cluster:", predicted_cluster, "\n")

