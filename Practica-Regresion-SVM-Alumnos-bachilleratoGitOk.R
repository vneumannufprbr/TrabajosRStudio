### Paquetes requeridos:
install.packages("e1071") 
install.packages("kernlab")
install.packages("caret")
install.packages("Metrics")
library(caret)
library(Metrics)

### Lectura del archivo de datos Volumen desde Github del Prof.  Neumann
url1 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/EstudiantesBachilleratoDatos.csv"
dados<- read.csv(url1, header=T)
View(dados)

### Crear bases de entrenamiento y pruebas
set.seed(1912)
indices <- createDataPartition(dados$G3, p=0.80,list=FALSE)
treino <- dados[indices,] 
teste <- dados[-indices,]

### Entrene SVM con la base de entrenamiento
set.seed(1912)
svm <- train(G3~., data=treino, method="svmRadial") 
svm

### Aplicación de modelos entrenados en la base de pruebas
predicoes.svm <- predict(svm, teste)

### Calcular las métricas
rmse(teste$G3, predicoes.svm)

r2 <- function(predito, observado) {
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
}
r2(predicoes.svm,teste$G3)

# Para calcular r2 tambien se puede usar la función R2 de la librería Caret 
R2(predicoes.svm, teste$G3)

#### SVM de validación cruzada
ctrl <- trainControl(method = "cv", number = 10)

set.seed(1912)
svm <- train(G3~., data=treino, method="svmRadial", trControl=ctrl)
svm

### Aplicación de modelos entrenados en la base de pruebas
predicoes.svm <- predict(svm, teste)

### Calcular as métricas
rmse(teste$G3, predicoes.svm)

r2 <- function(predito, observado) {
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
}
r2(predicoes.svm ,teste$G3)

# Para calcular r2 tambien se puede usar la función R2 de la librería Caret 
R2(predicoes.svm, teste$G3)

#### Varias C's y sigmas
tuneGrid = expand.grid(C=c(1, 2, 10, 50, 100), sigma=c(.01, .015, 0.2))

set.seed(1912)
svm <- train(G3~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
svm

### Aplicación de modelos entrenados en la base de pruebas
predicoes.svm <- predict(svm, teste)

### Calcular las métricas
rmse(teste$G3, predicoes.svm)

r2 <- function(predito, observado) {
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
}
r2(predicoes.svm,teste$G3)

# Para calcular r2 tambien se puede usar la función R2 de la librería Caret 
R2(predicoes.svm, teste$G3)

### PREDICCIONES DE NUEVOS CASOS
url2 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/EstudiantesBachilleratoDatosNuevosCasos.csv"
dados_novos_casos <- read.csv(url2)
View(dados_novos_casos)

dados_novos_casos$G3 <- NULL
predict.svm <- predict(svm, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predict.svm)
View(resultado)



