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

### Divida las bases en entrenamiento (80 %) y pruebas (20 %)
set.seed(1912)
indices <- createDataPartition(dados$Species, p=0.80, list=FALSE) 
treino <- dados[indices,]
teste <- dados[-indices,]

### Genere un nuevo modelo utilizando SVM, predicciones y matriz de confusión
set.seed(1912)
svm <- train(Species~., data=treino, method="svmRadial") 
svm

### Predicciones con el archivo de prueba
predicoes.svm <- predict(svm, teste)
confusionMatrix(predicoes.svm, teste$Species)
