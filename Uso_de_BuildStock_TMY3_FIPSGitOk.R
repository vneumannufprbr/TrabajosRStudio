# Código para Descompresión del archivo ZIP BuildStock_TMY3_FIPS.zip, Listado 
# de archivos .epw, Lectura del archivo .epw, Extracción de datos y Guardado 
# en example_weather.csv

# Instalar y cargar el paquete eplusr (solo si no está instalado)
if (!requireNamespace("eplusr", quietly = TRUE)) {
  install.packages("eplusr")
}
library(eplusr)

# -------------------------------------------------------------------------
# 1️⃣ Descomprimir el archivo ZIP---OJO: PERSONALIZAR zip_path y unzip_dir
# -------------------------------------------------------------------------
zip_path <- "C:/Users/UFPR/Downloads/BuildStock_TMY3_FIPS.zip"
unzip_dir <- "C:/Users/UFPR/Downloads/weather_data"

# Verificar si el archivo ZIP existe
if (!file.exists(zip_path)) {
  stop("El archivo ZIP no existe en la ruta especificada: ", zip_path)
}

# Crear directorio si no existe
if (!dir.exists(unzip_dir)) {
  dir.create(unzip_dir, recursive = TRUE)
  message("Directorio creado: ", unzip_dir)
} else {
  message("El directorio ya existe: ", unzip_dir)
}

# Descomprimir el archivo ZIP
tryCatch(
  {
    unzip(zip_path, exdir = unzip_dir)
    message("¡Descompresión exitosa!")
  },
  error = function(e) {
    stop("Error al descomprimir: ", e$message)
  }
)

# -------------------------------------------------------------------------
# 2️⃣ Listar archivos .epw
# -------------------------------------------------------------------------
weather_files <- list.files(unzip_dir, pattern = "\\.epw$", full.names = TRUE)

# Verificar si se encontraron archivos .epw
if (length(weather_files) == 0) {
  stop("No se encontraron archivos .epw en la carpeta: ", unzip_dir)
} else {
  message("Archivos .epw encontrados:")
  print(weather_files)
}

# -------------------------------------------------------------------------
# 3️⃣ Leer el primer archivo .epw
# -------------------------------------------------------------------------
tryCatch(
  {
    # Leer el segundo archivo .epw (índice 2)
    epw_file <- read_epw(weather_files[2])
    message("Archivo .epw leído correctamente: ", weather_files[2])
  },
  error = function(e) {
    stop("Error al leer el archivo .epw: ", e$message)
  }
)

# -------------------------------------------------------------------------
# 4️⃣ Extraer los datos
# -------------------------------------------------------------------------
epw_data <- epw_file$data()

# Verificar si los datos se extrajeron correctamente
if (nrow(epw_data) == 0) {
  stop("No se pudieron extraer datos del archivo .epw.")
} else {
  message("Datos extraídos correctamente. Primeras filas:")
  print(head(epw_data))
}

# -------------------------------------------------------------------------
# 5️⃣ Guardar los datos en un archivo CSV
# -------------------------------------------------------------------------
output_path <- file.path(unzip_dir, "example_weather.csv")
tryCatch(
  {
    write.csv(epw_data, output_path, row.names = FALSE)
    message("Datos guardados correctamente en: ", output_path)
  },
  error = function(e) {
    stop("Error al guardar el archivo CSV: ", e$message)
  }
)