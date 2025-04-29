### Instalación de los paquetes necesarios
install.packages("klaR")
library(klaR)

### Lectura de los datos
url <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/BancoDatos.csv"
datos <- read.csv(url)
View(datos)

### Aplicación de algoritmo Kmodes (versión de K-means para datos categóricos) 
set.seed(1912)
cluster.results <- kmodes(datos, 5, iter.max = 10, weighted = FALSE ) 
cluster.results

### Crea un archivo con todos los registros más los clusters de cada uno
resultado <- cbind(datos, cluster.results$cluster)
resultado

