### Paquetes requeridos:
install.packages("e1071") 
install.packages("kernlab")
install.packages("randomForest") 
install.packages("caret")
install.packages("Metrics")
library(Metrics)
library("caret")

### Lectura de los datos
url1 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/EstudiantesBachilleratoDatos.csv"
dados<- read.csv(url1)
View(dados)

### Crear bases de entrenamiento y pruebas
set.seed(1912)
indices <- createDataPartition(dados$G3, p=0.80,list=FALSE)
treino <- dados[indices,] 
teste <- dados[-indices,]

### Entrenar Randon Forest con la base de entrenamiento
set.seed(1912)
rf <- train(G3~., data=treino, method="rf")
rf

### Aplicación de modelos entrenados en la base de pruebas
predicoes.rf <- predict(rf, teste)

### Calcular las métricas RMSE y R2
rmse <- rmse(teste$G3, predicoes.rf)
rmse

r2 <- R2(teste$G3, predicoes.rf)
r2

#### Validación cruzada - RF
ctrl <- trainControl(method = "cv", number = 10)

set.seed(1912)
rf <- train(G3~., data=treino, method="rf", trControl=ctrl)
predicoes.rf <- predict(rf, teste)

### Calcular las métricas RMSE y R2
rmse <- rmse(teste$G3, predicoes.rf)
rmse

r2 <- R2(teste$G3, predicoes.rf)
r2

### PREDICCIONES DE NUEVOS CASOS
### Lectura de los datos
url2 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/EstudiantesBachilleratoDatosNuevosCasos.csv"
dados_novos_casos<- read.csv(url2)
View(dados_novos_casos)

dados_novos_casos$G3 <- NULL
predict.rf <- predict(rf, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predict.rf)
View(resultado)

