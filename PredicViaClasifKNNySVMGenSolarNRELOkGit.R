# 📌 1️⃣ Cargar librerías
library(shiny)
library(caret)
library(e1071)
library(class)

# 📌 2️⃣ Cargar los datos y entrenar el modelo SVM (podría cargarse desde un archivo en lugar de reentrenarlo aquí)
url <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/example_weather.csv"
weather_data <- read.csv(url, stringsAsFactors = FALSE)

# Preprocesamiento
weather_clean <- weather_data %>%
  mutate(across(everything(), ~ ifelse(grepl("[^0-9\\.\\-]", .x), NA, .x))) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  filter(!is.na(global_horizontal_radiation) & !is.na(global_horizontal_illuminance)) %>%
  mutate(production_level = case_when(
    global_horizontal_radiation >= 600 ~ "Alta",
    global_horizontal_radiation >= 200 ~ "Media",
    TRUE ~ "Baja"
  ))

weather_clean$production_level <- as.factor(weather_clean$production_level)

# Selección de variables
features <- weather_clean %>%
  select(global_horizontal_radiation, direct_normal_radiation, diffuse_horizontal_radiation,
         global_horizontal_illuminance, direct_normal_illuminance, diffuse_horizontal_illuminance)

target <- weather_clean$production_level

# División en entrenamiento (80%) y prueba (20%)
set.seed(123)
train_index <- createDataPartition(target, p = 0.8, list = FALSE)
X_train <- features[train_index, ]
X_test <- features[-train_index, ]
Y_train <- target[train_index]
Y_test <- target[-train_index]

# Normalizar las variables
X_train_scaled <- as.data.frame(scale(X_train))
X_test_scaled <- as.data.frame(scale(X_test))

# Entrenar SVM
svm_model <- svm(production_level ~ ., data = data.frame(X_train_scaled, production_level = Y_train), kernel = "radial")

# Entrenar KNN
k <- 3
knn_model <- function(new_data) {
  knn(train = X_train_scaled, test = new_data, cl = Y_train, k = k)
}

# 📌 3️⃣ Crear la Aplicación Shiny
ui <- fluidPage(
  titlePanel("Predicción de Producción Fotovoltaica"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("ghr", "Radiación Global Horizontal:", min = 0, max = 1000, value = 500),
      sliderInput("dnr", "Radiación Directa Normal:", min = 0, max = 1000, value = 400),
      sliderInput("dhr", "Radiación Difusa Horizontal:", min = 0, max = 1000, value = 200),
      sliderInput("ghi", "Iluminancia Global Horizontal:", min = 0, max = 150000, value = 75000),
      sliderInput("dni", "Iluminancia Directa Normal:", min = 0, max = 150000, value = 60000),
      sliderInput("dhi", "Iluminancia Difusa Horizontal:", min = 0, max = 150000, value = 20000),
      selectInput("model_type", "Selecciona el Modelo:", choices = c("SVM", "K-NN")),
      actionButton("predict", "Predecir")
    ),
    mainPanel(
      h3("Predicción:"),
      textOutput("prediction")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$predict, {
    # Crear nuevo punto de datos
    new_data <- data.frame(
      global_horizontal_radiation = input$ghr,
      direct_normal_radiation = input$dnr,
      diffuse_horizontal_radiation = input$dhr,
      global_horizontal_illuminance = input$ghi,
      direct_normal_illuminance = input$dni,
      diffuse_horizontal_illuminance = input$dhi
    )
    
    # Normalizar nuevo punto
    new_data_scaled <- as.data.frame(scale(new_data, center = colMeans(X_train), scale = apply(X_train, 2, sd)))
    
    # Seleccionar el modelo
    if (input$model_type == "SVM") {
      prediction <- predict(svm_model, new_data_scaled)
    } else {
      prediction <- knn_model(new_data_scaled)
    }
    
    # Mostrar la predicción
    output$prediction <- renderText({ as.character(prediction) })
  })
}

shinyApp(ui = ui, server = server)
