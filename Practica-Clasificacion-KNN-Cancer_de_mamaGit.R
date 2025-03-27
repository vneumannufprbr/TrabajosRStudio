### Paquetes requeridos:
install.packages("e1071") 
install.packages("caret")
library("caret")

### Lectura del archivo de datos CancerdeMamaDatos.csv de Github del Prof. Victor Neumann
url1 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/CancerdeMamaDatos.csv"
dados<- read.csv(url1)

### Guarda el archivo en el directorio de trabajo de tu RStudio - Es opcional, o sea no es necesario guardarlo para continuar con la práctica
getwd()    # Para verificar el directorio actual de tu Rstudio
destfile <- "CancerdeMamaDatos.csv"  # Nombre con el que se guardará el archivo
download.file(url1, destfile)  # Descarga el archivo desde la URL y lo guarda en el directorio de trabajo de tu RStudio.
# Usar setwd ("C:\ruta del archivo“) después de getwd()  si lo guardarás en una carpeta diferente de la actual de tu RStudio

### Quita el atributo ID
dados$Id <- NULL

### Visualización de los datos
View(dados)

### Crea un fichero con entrenamiento con el 80% y prueba con el 20% de las filas de forma aleatoria
set.seed(1912)
ran <- sample(1:nrow(dados), 0.8 * nrow(dados))
treino <- dados[ran,] 
teste <- dados[-ran,] 

### Haga una cuadricula con los valores de K y ejecute el KNN
tuneGrid <- expand.grid(k = c(1,3,5,7,9))

set.seed(1912)
knn <- train(Class ~ ., data = treino, method = "knn",tuneGrid=tuneGrid)
knn

### Hace la predicción y muestra la matriz de confusión
predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$Class))

### Hace la predicción y muestra la matriz de confusión
predict.knn <- predict(knn, teste)
confusionMatrix(predict.knn, as.factor(teste$Class))

### PREDICCIONES DE NUEVOS CASOS
url2 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/CancerdeMamaDatosNuevosCasos.csv"
dados_novos_casos <- read.csv(url2)
dados_novos_casos$Id <- NULL
View(dados_novos_casos)

predict.knn <- predict(knn, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predict.knn)
resultado$Class <- NULL
View(resultado)

### GUARDE EL MEJOR MODELO PARA SU USO PRÁCTICO

### EJECUCIÓN DE UN MODELO CON LOS MEJORES HIPERPARÁMETROS
tuneGrid <- expand.grid(k = c(9))

set.seed(1912)
melhor_modelo_knn <- train(Class ~ ., data = dados, method = "knn",tuneGrid=tuneGrid)
melhor_modelo_knn

### GUARDE EL MEJOR MODELO PARA SU USO PRÁCTICO
### GUARDAR EL MODELO 
getwd()
saveRDS(melhor_modelo_knn,"CancerdeMamaMejorModeloKNN.rds")

### LEA Y APLIQUE EL MODELO
modelo_lido <- readRDS("./CancerdeMamaMejorModeloKNN.rds")
novas_predicoes <- predict(modelo_lido, teste)
confusionMatrix(novas_predicoes, as.factor(teste$Class))



