# Cargar librerías necesarias
library(tidyverse)  # Manejo de datos\library(caret)      # Modelado y evaluación
library(e1071)      # Implementación de SVM
library(class)
library(caret)
if(!require(ggplot2)) install.packages("ggplot2")


# 1️⃣ Obtención y lectura de datos
# 1ra opción: Primera linea de codigo abajo como opción para cargar los datos desde el computador
weather_data <- read.csv("C:/Users/UFPR/Downloads/weather_data/example_weather.csv", stringsAsFactors = FALSE)

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

# 5️⃣ Clasificar la producción fotovoltaica en 3 niveles
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

# 🔟 Entrenar el modelo SVM
svm_model <- svm(Y_train ~ ., data = data.frame(X_train, Y_train), kernel = "radial", cost = 1, gamma = 0.1)

# 1️⃣1️⃣ Realizar predicciones
Y_pred <- predict(svm_model, X_test)

# 1️⃣2️⃣ Evaluar el modelo
conf_matrix <- confusionMatrix(Y_pred, Y_test)
print(conf_matrix)

# 1️⃣3️⃣ Graficar la matriz de confusión
conf_df <- as.data.frame(conf_matrix$table)

ggplot(data = conf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Matriz de Confusión - SVM", x = "Valores Reales", y = "Predicciones")


