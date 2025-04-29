# INSTALACIÓN Y CARGA DE PAQUETES NECESARIOS
# Instalar clustMixType si no está presente
if (!require("clustMixType")) install.packages("clustMixType")
# Instalar tidyverse si no está presente (para manejo de datos opcional)
if (!require("tidyverse")) install.packages("tidyverse")

# Cargar los paquetes
library(clustMixType)
library(tidyverse) # Opcional, para pipelines %>%

# LECTURA DE LOS DATOS
url <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/BancoDatos.csv"
datos <- read.csv(url)

# (Opcional) Inspeccionar los datos y sus tipos
View(datos)

str(datos) # Verificar tipos de columnas (numeric, character/factor)

cat("Paquetes cargados y datos leídos.\n")

# APLICACIÓN DEL ALGORITMO K-PROTOTYPES
# Fijar semilla para reproducibilidad
set.seed(1912)

# Convertir todas las columnas 'character' a 'factor'
datos <- datos %>%
  mutate(across(where(is.character), as.factor))

# Ejecutar K-Prototypes
# k = 5: Número de clusters deseado (igual que en el ejemplo k-modes)
# iter.max = 20: Aumentamos un poco las iteraciones máximas
# nstart = 10: Ejecutar 10 veces con inicios aleatorios y elegir el mejor resultado
# lambda: Parámetro que pondera la contribución categórica.
#         Si no se especifica, kproto lo estima automáticamente (recomendado para empezar).

# Ahora ejecuta kproto
set.seed(1912)
cat("Ejecutando K-Prototypes...\n")
kp.res <- kproto(x = datos, 
                 k = 5, 
                 iter.max = 20, 
                 nstart = 10, 
                 verbose = FALSE)
cat("Ejecución completada.\n")
print(kp.res)

# Mostrar un resumen de los resultados
print(kp.res)

# Mostrar los prototipos (centroides) de cada cluster
# Para variables numéricas mostrará la media, para categóricas la moda.
cat("\nPrototipos (Centroides) de los Clusters:\n")
print(kp.res$centers)

