# Cargar librerías necesarias
library(tidyverse)
library(caret)
library(class)
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(class)) install.packages("class")

# 1️⃣ Obtención y lectura de datos
# 1ra opción: Primera linea de codigo abajo como opción para cargar los datos desde el computador
#weather_data <- read.csv("C:/Users/UFPR/Downloads/weather_data/example_weather.csv", stringsAsFactors = FALSE)

### 2da opción: lectura de los datos del Github
#### Inicio codigo accesso a Github del Prof. Victor Neumann
url <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/example_weather.csv"

#Ahora sí, leemos el archivo:
weather_data <- read.csv(url, header = TRUE)

# Imprimir las primeras filas para verificar
head(weather_data)

#### Fin codigo accesso a Github

# 2️⃣ Reemplazar valores incorrectos con NA y convertir columnas numéricas
weather_clean <- weather_data %>%
  mutate(across(everything(), ~ ifelse(grepl("[^0-9\\.\\-]", .x), NA, .x))) %>% 
  mutate(across(where(is.character), as.numeric))

# 3️⃣ Verificar datos faltantes
summary(weather_clean)

# 4️⃣ Eliminar filas con valores NA en variables clave
weather_clean <- weather_clean %>%
  filter(!is.na(global_horizontal_radiation) & !is.na(global_horizontal_illuminance))

# 5️⃣ Clasificar la producción fotovoltaica en 3 niveles (Alta, Media, Baja)
weather_clean <- weather_clean %>%
  mutate(production_level = case_when(
    global_horizontal_radiation >= 600 ~ "Alta",
    global_horizontal_radiation >= 200 ~ "Media",
    TRUE ~ "Baja"
  ))

# 6️⃣ Convertir la variable objetivo a factor
weather_clean$production_level <- as.factor(weather_clean$production_level)

# 7️⃣ Revisar distribución de clases
table(weather_clean$production_level)

# 8️⃣ Seleccionar variables predictoras y la variable objetivo
features <- weather_clean %>%
  select(global_horizontal_radiation, direct_normal_radiation, diffuse_horizontal_radiation,
         global_horizontal_illuminance, direct_normal_illuminance, diffuse_horizontal_illuminance)

target <- weather_clean$production_level

# 9️⃣ Dividir los datos en entrenamiento (80%) y prueba (20%)
set.seed(123)  # Para reproducibilidad
train_index <- createDataPartition(target, p = 0.8, list = FALSE)

X_train <- features[train_index, ]
X_test <- features[-train_index, ]
Y_train <- target[train_index]
Y_test <- target[-train_index]

# 🔟 Normalizar las variables predictoras DESPUÉS de dividir los datos
X_train_scaled <- as.data.frame(scale(X_train))
X_test_scaled <- as.data.frame(scale(X_test))

# 1️⃣1️⃣ Probar con un valor de k más pequeño
k <- 2

# 1️⃣2️⃣ Aplicar K-NN
Y_pred <- knn(train = X_train_scaled, test = X_test_scaled, cl = Y_train, k = k)

# 1️⃣3️⃣ Evaluar el modelo
conf_matrix <- confusionMatrix(Y_pred, Y_test)
print(conf_matrix)

# Convertir la matriz en un formato gráfico
conf_df <- as.data.frame(conf_matrix$table)

# Graficar la matriz de confusión
 
library(ggplot2)
ggplot(data = conf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Matriz de Confusión", x = "Valores Reales", y = "Predicciones")

