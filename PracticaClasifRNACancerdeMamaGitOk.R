### Instalación de los paquetes necesarios
install.packages("caret") 
install.packages("e1071") 
install.packages("mlbench") 
install.packages("mice")

library(caret) 
library(mlbench) 
library(mice)

### Lectura del archivo de datos CancerdeMamaDatos.csv de Github del Prof.  Neumann
url1 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/CancerdeMamaDatos.csv"
dados<- read.csv(url1)

### Tratar el id y los Missing Values (datos faltantes)
dados$Id <- NULL
imp <- mice(dados) 
dados <- complete(imp, 1)

### Crear bases de entrenamiento y pruebas
set.seed(1912)
indices <- createDataPartition(dados$Class, p=0.80,list=FALSE)
treino <- dados[indices,] 
teste <- dados[-indices,]

### Entrene el modelo com Hold-out
set.seed(1912)
rna <- train(Class~., data=treino, method="nnet",trace=FALSE)
rna

### Predicciones de valores del conjunto de pruebas
predict.rna <- predict(rna, teste)

### Matriz de confusión
confusionMatrix(predict.rna, as.factor(teste$Class))

## Usando Cross-validation
### Indica el método CV y el número de folders 10
ctrl <- trainControl(method = "cv", number = 10)

### Ejecute la RNA con ese ctrl
set.seed(1912)
rna <- train(Class~., data=treino, method="nnet",trace=FALSE, trControl=ctrl)
predict.rna <- predict(rna, teste) 
confusionMatrix(predict.rna, as.factor(teste$Class))

## Parametrización de la RNA
### size, decay
grid <- expand.grid(size = seq(from = 1, to = 45, by = 10), decay = seq(from = 0.1, to = 0.9, by = 0.3))

set.seed(1912)
rna <- train( form = Class~. ,  data = treino ,  method = "nnet" ,  tuneGrid = grid ,  trControl = ctrl ,  maxit = 1000,trace=FALSE) 

### Analizar el resultado del Entrenamiento
rna

### Hace predicciones y muestra la matriz de confusión
predict.rna <- predict(rna, teste)
confusionMatrix(predict.rna, as.factor(teste$Class))

### PREDICCIONES DE NUEVOS CASOS
url2 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/CancerdeMamaDatosNuevosCasos.csv"
dados_novos_casos <- read.csv(url2)
dados_novos_casos$Id <- NULL
View(dados_novos_casos)

predict.rna <- predict(rna, dados_novos_casos)
dados_novos_casos$Class <- NULL
resultado <- cbind(dados_novos_casos, predict.rna)
View(resultado)

### GUARDE EL MEJOR MODELO PARA SU USO PRÁCTICO

### GUARDE EL MODELO
getwd()
saveRDS(rna, "CancerdeMamaMejorModeloRNA.rds")

### LEA Y APLIQUE EL MEJOR MODELO
modelo_lido <- readRDS("./CancerdeMamaMejorModeloRNA.rds ")
novas_predicoes <- predict(modelo_lido, teste)
confusionMatrix(novas_predicoes, as.factor(teste$Class))


