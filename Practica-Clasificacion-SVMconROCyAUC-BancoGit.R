### Paquetes requeridos:
install.packages("e1071")
install.packages("kernlab")
install.packages("caret")
install.packages("mice")
install.packages("pROC")
install.packages("MLmetrics")

library(caret)
library(mice)
library(pROC)
library(MLmetrics)

### Lectura del archivo de datos
url1 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/BancoDatos.csv"
dados <- read.csv(url1)
View(dados)

### Crear bases de entrenamiento y pruebas
set.seed(1912)
indices <- createDataPartition(dados$y, p = 0.80, list = FALSE)
treino <- dados[indices, ]
teste  <- dados[-indices, ]

### Entrenamiento del modelo SVM con validación cruzada
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, savePredictions = TRUE)
set.seed(1912)
svm <- train(y ~ ., data = treino, method = "svmRadial", trControl = ctrl)

### Predicción de probabilidades y clases
predict.svm.prob <- predict(svm, teste, type = "prob")
predict.svm.class <- predict(svm, teste)

### Matriz de confusión (umbral por defecto 0.5)
confusionMatrix(predict.svm.class, as.factor(teste$y))

### Calcular F1-Score
f1 <- F1_Score(y_pred = predict.svm.class, y_true = teste$y, positive = "yes")
cat("F1-score:", round(f1, 3), "\n")

### Curva ROC y AUC (para clase positiva: "yes")
roc_obj <- roc(response = teste$y, predictor = predict.svm.prob$yes, levels = rev(levels(as.factor(teste$y))))
auc_value <- auc(roc_obj)
cat("AUC:", round(auc_value, 3), "\n")

### Índice de Youden = (sensibilidad + especificidad - 1)
youden_index <- roc_obj$sensitivities + roc_obj$specificities - 1
best_threshold <- roc_obj$thresholds[which.max(youden_index)]
cat("Índice de Youden máximo:", round(max(youden_index), 3), "\n")
cat("Umbral óptimo según Youden:", round(best_threshold, 3), "\n")

### Gráfico ROC
plot(roc_obj, col = "blue", lwd = 2, main = "Curva ROC - SVM")
abline(a = 0, b = 1, lty = 2, col = "gray")

### Aplicación en nuevos casos
url2 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/BancoDatosNuevosCasos.csv"
dados_novos_casos <- read.csv(url2)
predict.svm <- predict(svm, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predict.svm)
resultado$y <- NULL
View(resultado)
