### Paquetes requeridos:
install.packages("e1071") 
install.packages("kernlab")
install.packages("caret")
install.packages("Metrics")
library(caret)

### Lectura del archivo de datos Volumen desde Github del Prof.  Neumann
url1 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/EstimativadeVolumenDatos.csv"
dados<- read.csv(url1, header=T)
View(dados)

### Crear bases de entrenamiento y pruebas
set.seed(1912)
indices <- createDataPartition(dados$Volume, p=0.80,list=FALSE)
treino <- dados[indices,] 
teste <- dados[-indices,]

### Entrene SVM con la base de entrenamiento
set.seed(1912)
svm <- train(Volume~., data=treino, method="svmRadial") 
svm

### Aplicación de modelos entrenados en la base de pruebas
predicoes.svm <- predict(svm, teste)

### Calcular las métricas
library(Metrics)
library("caret")
rmse(teste$Volume, predicoes.svm)

### Via formulación de R2
r2 <- function(predito, observado) {
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
}
r2(predicoes.svm,teste$Volume)

# Para calcular r2 tambien se puede usar la función R2 de la librería Caret 
R2(predicoes.svm, teste$Volume)

#### SVM de validación cruzada
ctrl <- trainControl(method = "cv", number = 10)

set.seed(1912)
svm <- train(Volume~., data=treino, method="svmRadial", trControl=ctrl)
svm

### Aplicación de modelos entrenados en la base de pruebas
predicoes.svm <- predict(svm, teste)

### Calcular las métricas
library(Metrics)
library("caret")
rmse(teste$Volume, predicoes.svm)

### Via formulación de R2
r2 <- function(predito, observado) {
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
}
r2(predicoes.svm,teste$Volume)

# Para calcular r2 tambien se puede usar la función R2 de la librería Caret 
R2(predicoes.svm, teste$Volume)

#### Varias C's y sigmas
tuneGrid = expand.grid(C=c(1, 2, 10, 50, 100), sigma=c(.01, .015, 0.2))

set.seed(1912)
svm <- train(Volume~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
svm

### Aplicación de modelos entrenados en la base de pruebas
predicoes.svm <- predict(svm, teste)

### Calcular las métricas
rmse(teste$Volume, predicoes.svm)

r2 <- function(predito, observado) {
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
}
r2(predicoes.svm,teste$Volume)

# Para calcular r2 tambien se puede usar la función R2 de la librería Caret 
R2(predicoes.svm, teste$Volume)

### PREDICCIONES DE NUEVOS CASOS
url2 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/EstimativadeVolumenDatosNuevosCasos.csv"
dados_novos_casos <- read.csv(url2)
View(dados_novos_casos)

dados_novos_casos$Volume <- NULL
predict.svm <- predict(svm, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predict.svm)
View(resultado)

