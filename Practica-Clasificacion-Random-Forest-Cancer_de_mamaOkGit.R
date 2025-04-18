### Paquetes requeridos:
install.packages("e1071") 
install.packages("randomForest") 
install.packages("kernlab")
install.packages("caret")
library("caret")

install.packages("mice")
library(mice)


### Lectura del archivo de datos CancerdeMamaDatos.csv de Github del Prof.  Neumann
url1 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/CancerdeMamaDatos.csv"
dados<- read.csv(url1)

### Elimina el ID y rellena los valores que faltan
dados$Id <- NULL
imp <- mice(dados) 
dados <- complete(imp, 1)

### Crear bases de entrenamiento y pruebas
set.seed(1912)
indices <- createDataPartition(dados$Class, p=0.80,list=FALSE)
treino <- dados[indices,] 
teste <- dados[-indices,]

### Entrena Random Forest con la base de entrenamiento
set.seed(1912) 
rf <- train(Class~., data=treino, method="rf")
rf

### Aplicación de modelos entrenados en la base de pruebas
predict.rf <- predict(rf, teste)
confusionMatrix(predict.rf, as.factor(teste$Class)) 

#### Validación cruzada (CV- Cross-validation)
ctrl <- trainControl(method = "cv", number = 10)

set.seed(1912)
rf <- train(Class~., data=treino, method="rf", trControl=ctrl)
rf

### Matriz de confusión con todos los datos
predict.rf <- predict(rf, dados)
confusionMatrix(predict.rf, as.factor(dados$Class))

#### Varios mtry
tuneGrid = expand.grid(mtry=c(2, 5, 7, 9))

set.seed(1912)
rf <- train(Class~., data=treino, method="rf", trControl=ctrl, tuneGrid=tuneGrid)
rf

### Matriz de confusión con todos los datos
predict.rf <- predict(rf, dados)
confusionMatrix(predict.rf, as.factor(dados$Class))

### PREDICCIONES DE NUEVOS CASOS
url2 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/CancerdeMamaDatosNuevosCasos.csv"
dados_novos_casos <- read.csv(url2)
dados_novos_casos$Id <- NULL
View(dados_novos_casos)

predict.rf <- predict(rf, dados_novos_casos)
dados_novos_casos$Class <- NULL
resultado <- cbind(dados_novos_casos, predict.rf)
View(resultado)

