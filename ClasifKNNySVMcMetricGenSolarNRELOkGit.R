# 📌 1️⃣ Cargar librerías necesarias
library(tidyverse)   # Manipulación de datos
library(caret)       # Para dividir datos y evaluar modelos
library(class)       # Para K-NN
library(e1071)       # Para SVM

# 📌 2️⃣ Cargar los datos desde un archivo CSV o URL
url <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/example_weather.csv"
weather_data <- read.csv(url, stringsAsFactors = FALSE)

# 📌 3️⃣ Preprocesamiento de datos
# Convertir caracteres a numéricos y manejar valores erróneos
weather_clean <- weather_data %>%
  mutate(across(everything(), ~ ifelse(grepl("[^0-9\\.\\-]", .x), NA, .x))) %>%
  mutate(across(where(is.character), as.numeric))

# 📌 4️⃣ Eliminar filas con valores NA en variables clave
weather_clean <- weather_clean %>%
  filter(!is.na(global_horizontal_radiation) & !is.na(global_horizontal_illuminance))

# 📌 5️⃣ Crear la variable objetivo "production_level" (Alta, Media, Baja)
weather_clean <- weather_clean %>%
  mutate(production_level = case_when(
    global_horizontal_radiation >= 600 ~ "Alta",
    global_horizontal_radiation >= 200 ~ "Media",
    TRUE ~ "Baja"
  ))

# 📌 6️⃣ Convertir a factor la variable objetivo
weather_clean$production_level <- as.factor(weather_clean$production_level)

# 📌 7️⃣ Seleccionar variables predictoras y la variable objetivo
features <- weather_clean %>%
  select(global_horizontal_radiation, direct_normal_radiation, diffuse_horizontal_radiation,
         global_horizontal_illuminance, direct_normal_illuminance, diffuse_horizontal_illuminance)

target <- weather_clean$production_level

# 📌 8️⃣ Dividir los datos en entrenamiento (80%) y prueba (20%)
set.seed(123)  # Reproducibilidad
train_index <- createDataPartition(target, p = 0.8, list = FALSE)

X_train <- features[train_index, ]
X_test <- features[-train_index, ]
Y_train <- target[train_index]
Y_test <- target[-train_index]

# 📌 9️⃣ Normalizar las variables predictoras (Importante para K-NN)
X_train_scaled <- as.data.frame(scale(X_train))
X_test_scaled <- as.data.frame(scale(X_test))

# 📌 🔟 Implementación del Modelo K-NN
k <- 3  # Valor óptimo de k
Y_pred_knn <- knn(train = X_train_scaled, test = X_test_scaled, cl = Y_train, k = k)

# 📌 1️⃣1️⃣ Implementación del Modelo SVM
svm_model <- svm(production_level ~ ., data = data.frame(X_train_scaled, production_level = Y_train), kernel = "radial")
Y_pred_svm <- predict(svm_model, newdata = X_test_scaled)

# 📌 1️⃣2️⃣ Evaluación con Matriz de Confusión y Métricas
conf_matrix_knn <- confusionMatrix(Y_pred_knn, Y_test)
conf_matrix_svm <- confusionMatrix(Y_pred_svm, Y_test)

print("Resultados K-NN:")
print(conf_matrix_knn)

print("Resultados SVM:")
print(conf_matrix_svm)

# 📌 1️⃣3️⃣ Extraer métricas clave
# 📌 Función para calcular métricas
calcular_metricas <- function(conf_matrix) {
  precision <- conf_matrix$byClass[, "Pos Pred Value"]  # Precisión por clase
  recall <- conf_matrix$byClass[, "Sensitivity"]  # Sensibilidad (Recall) por clase
  f1_score <- 2 * ((precision * recall) / (precision + recall))  # F1-score por clase
  
  # Manejar posibles NA en F1-score
  f1_score[is.na(f1_score)] <- 0
  
  return(list(
    Precision = precision,
    Recall = recall,
    F1_Score = f1_score
  ))
}

# 📌 Aplicar la función a K-NN
metrics_knn <- calcular_metricas(conf_matrix_knn)

# 📌 Aplicar la función a SVM
metrics_svm <- calcular_metricas(conf_matrix_svm)

# 📌 Mostrar resultados
cat("\n📊 Métricas K-NN:\n")
names(metrics_knn) <- c("Precisión", "Sensibilidad (Recall)", "Puntaje F1")
print(metrics_knn)

cat("\n📊 Métricas SVM:\n")
names(metrics_svm) <- c("Precisión", "Sensibilidad (Recall)", "Puntaje F1")
print(metrics_svm)

