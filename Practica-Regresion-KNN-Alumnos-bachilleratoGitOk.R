### Instalación de los paquetes
install.packages("e1071") 
install.packages("caret")
install.packages("Metrics")
library(caret)


### Lectura del archivo de datos Volumen desde Github del Prof.  Neumann
url1 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/EstudiantesBachilleratoDatos.csv"
dados<- read.csv(url1, header=T)
View(dados)

### Crea archivos de entrenamiento y prueba
set.seed(1912)
ind <- createDataPartition(dados$G3, p=0.80, list = FALSE)
treino <- dados[ind,]
teste <- dados[-ind,]

### Prepara una cuadrícula con los valores de k que se utilizará
tuneGrid <- expand.grid(k = c(1,3,5,7,9))

### Ejecuta Knn con esta cuadrícula
set.seed(1912)
knn <- train(G3 ~ ., data = treino, method = "knn",
             tuneGrid=tuneGrid)
knn

### Aplicar el modelo al archivo de prueba
predict.knn <- predict(knn, teste)

### Muestra métricas
rmse(teste$G3, predict.knn)

r2 <- function(predito, observado) {
  return(1 - (sum((predito-observado)^2) / sum((predito-mean(observado))^2)))
}
r2(predict.knn,teste$G3)

# Para calcular r2 tambien se puede usar la función R2 de la librería Caret 
R2(predict.knn, teste$G3)

### PREDICCIONES DE NUEVOS CASOS
url2 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/EstudiantesBachilleratoDatosNuevosCasos.csv"
dados_novos_casos <- read.csv(url2)
View(dados_novos_casos)

predict.knn <- predict(knn, dados_novos_casos)
dados_novos_casos$G3 <- NULL
resultado <- cbind(dados_novos_casos, predict.knn)
View(resultado)
