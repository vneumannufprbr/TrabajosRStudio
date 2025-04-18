### Paquetes requeridos:
install.packages("e1071") 
install.packages("kernlab")
install.packages("caret")
install.packages("randomForest") 
library("caret")

### Lectura de los datos
data(iris)
dados <- iris
View(dados)

### Divida las bases en entrenamiento (80 %) y pruebas (20 %))
set.seed(1912)
indices <- createDataPartition(dados$Species, p=0.80, list=FALSE) 
treino <- dados[indices,]
teste <- dados[-indices,]

### Genere un nuevo modelo utilizando Randon Forest, predicciones y matriz de confusión
set.seed(1912)
rf <- train(Species~., data=treino, method="rf")
rf

### Predicciones con el archivo de prueba
predicoes.rf <- predict(rf, teste)
confusionMatrix(predicoes.rf, teste$Species)

