### Paquetes requeridos:
install.packages("e1071") 
install.packages("caret")
library("caret")

### Lectura del archivo de datos BancoDatos.csv de Github del Prof. Neumann
url1 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/BancoDatos.csv"
dados<- read.csv(url1)
View(dados)

### Crea un fichero con entrenamiento con el 80% y prueba con el 20% de las filas de forma aleatoria
set.seed(1912)
ran <- sample(1:nrow(dados), 0.8 * nrow(dados))
treino <- dados[ran,] 
teste <- dados[-ran,] 

### Haga un ajuste con los valores de K y ejecute el KNN
tuneGrid <- expand.grid(k = c(1,3,5,7,9))

set.seed(1912)
knn <- train(y ~ ., data = treino, method = "knn",tuneGrid=tuneGrid)
knn

### Hace la predicción y muestra la matriz de confusión
predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$y))

### PREDICCIONES DE NUEVOS CASOS
url2 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/BancoDatosNuevosCasos.csv"
dados_novos_casos <- read.csv(url2)
View(dados_novos_casos)

predict.knn <- predict(knn, dados_novos_casos)
dados_novos_casos$y <- NULL
resultado <- cbind(dados_novos_casos, predict.knn)
View(resultado)
