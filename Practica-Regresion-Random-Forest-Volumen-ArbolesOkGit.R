### Paquetes requeridos:
install.packages("e1071") 
install.packages("kernlab")
install.packages("randomForest") 
install.packages("caret")
install.packages("Metrics")
library(Metrics)
library("caret")

### Lectura de los datos
url1 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/EstimativadeVolumenDatos.csv"
dados<- read.csv(url1)
View(dados)

### Crear bases de entrenamiento y pruebas
set.seed(1912)
indices <- createDataPartition(dados$Volume, p=0.80,list=FALSE)
treino <- dados[indices,] 
teste <- dados[-indices,]

### Entrena el Bosque Randon con la Base de Entrenamiento
set.seed(1912)
rf <- train(Volume~., data=treino, method="rf")
rf
