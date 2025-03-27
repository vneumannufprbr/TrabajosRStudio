### Paquetes requeridos:
install.packages("e1071") 
install.packages("kernlab")
install.packages("caret")
library("caret")

### Lectura de los datos
### Los datos de la flor Iris, que son muy utilizados para hacer clasificación en R, vienen incluidos en R de forma predeterminada.
data(iris)
dados <- iris
View(dados)

### Crea un archivo con el 80 % de las filas para el entrenamiento y el 20 % para las pruebas
set.seed(1912)
ran <- sample(1:nrow(dados), 0.8 * nrow(dados))
treino <- dados[ran,] 
teste <- dados[-ran,] 

### Haga un auto-ajuste con los valores para K y realice el entrenamiento
tuneGrid <- expand.grid(k = c(1,3,5,7,9))

set.seed(1912)
knn <- train(Species ~ ., data = treino, method = "knn",tuneGrid=tuneGrid)
knn

### Hace la predicción y muestra la matriz de confusión
predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$Species))
