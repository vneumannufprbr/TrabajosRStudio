### Paquetes requeridos:
install.packages("e1071") 
install.packages("kernlab")
install.packages("caret")
install.packages("mice")
library("caret")
library(mice)

### Lectura del archivo de datos CancerdeMamaDatos.csv de Github del Prof.  Neumann
url1 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/CancerdeMamaDatos.csv"
dados<- read.csv(url1)

### Elimina el ID y rellena los valores que faltan
dados$Id <- NULL
View(dados)

### Crear bases de entrenamiento y pruebas
set.seed(1912)
indices <- createDataPartition(dados$Class, p=0.80,list=FALSE)
treino <- dados[indices,] 
teste <- dados[-indices,]

### Entrene SVM con la base de entrenamiento
set.seed(1912)
svm <- train(Class~., data=treino, method="svmRadial") 
svm

### Aplicación de modelos entrenados en la base de pruebas
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$Class))

#### SVM de validación cruzada
ctrl <- trainControl(method = "cv", number = 10)

set.seed(1912)
svm <- train(Class~., data=treino, method="svmRadial", trControl=ctrl)
svm

### Matriz de confusión con todos los datos
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$Class))

#### Varias C's y sigmas
tuneGrid = expand.grid(C=c(1, 2, 10, 50, 100), sigma=c(.01, .015, 0.2))

set.seed(1912)
svm <- train(Class~., data=treino, method="svmRadial", trControl=ctrl, tuneGrid=tuneGrid)
svm
melhor_modelo_svm <- svm

### Matriz de confusión con todos los datos nuevamente
predict.svm <- predict(svm, teste)
confusionMatrix(predict.svm, as.factor(teste$Class))

### PREDICCIONES DE NUEVOS CASOS
url2 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/CancerdeMamaDatosNuevosCasos.csv"
dados_novos_casos <- read.csv(url2)
dados_novos_casos$Id <- NULL
View(dados_novos_casos)

predict.svm <- predict(svm, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predict.svm)
resultado$Class <- NULL
View(resultado)

### GUARDE EL MEJOR MODELO PARA SU USO PRÁCTICO

### GUARDAR EL MEJOR MODELO
getwd()
melhor_modelo_svm <- svm
saveRDS(melhor_modelo_svm,"CancerdeMamaMejorModeloSVM.rds")

### LEA Y APLIQUE EL MEJOR MODELO
modelo_lido <- readRDS("./CancerdeMamaMejorModeloSVM.rds ")
novas_predicoes <- predict(modelo_lido, teste)
confusionMatrix(novas_predicoes, as.factor(teste$Class))

