### Instalar paquetes necesarios (solo la primera vez)
install.packages("e1071")
install.packages("kernlab")
install.packages("caret")
install.packages("mice")
install.packages("pROC")
install.packages("MLmetrics")

### Cargar librerías
library(caret)
library(mice)
library(pROC)
library(MLmetrics)

### Lectura de datos
url1 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/BancoDatos.csv"
dados <- read.csv(url1)

### Partición de entrenamiento y prueba
set.seed(1912)
indices <- createDataPartition(dados$y, p = 0.80, list = FALSE)
treino <- dados[indices, ]
teste <- dados[-indices, ]

### Entrenamiento del modelo SVM
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, savePredictions = TRUE)
set.seed(1912)
svm <- train(y ~ ., data = treino, method = "svmRadial", trControl = ctrl)

### Probabilidades de predicción
predict.svm.prob <- predict(svm, teste, type = "prob")

### Curva ROC y cálculo del umbral óptimo (índice de Youden)
roc_obj <- roc(response = teste$y, predictor = predict.svm.prob$yes, levels = rev(levels(as.factor(teste$y))))
youden_index <- roc_obj$sensitivities + roc_obj$specificities - 1
best_threshold <- roc_obj$thresholds[which.max(youden_index)]
cat("Umbral óptimo (Youden):", round(best_threshold, 3), "\n")

### Clasificación usando el umbral de Youden
pred.opt <- ifelse(predict.svm.prob$yes >= best_threshold, "yes", "no")

# Asegurar factores con niveles idénticos
niveles <- levels(as.factor(teste$y))
pred.opt <- factor(pred.opt, levels = niveles)
y.real <- factor(teste$y, levels = niveles)

### Matriz de confusión usando umbral óptimo
confusion <- confusionMatrix(pred.opt, y.real)
print(confusion)

### F1-Score con umbral óptimo
f1 <- F1_Score(y_pred = pred.opt, y_true = y.real, positive = "yes")
cat("F1-score (umbral Youden):", round(f1, 3), "\n")

### AUC
auc_value <- auc(roc_obj)
cat("AUC:", round(auc_value, 3), "\n")

### Gráfico ROC
plot(roc_obj, col = "blue", lwd = 2, main = "Curva ROC - SVM")
abline(a = 0, b = 1, lty = 2, col = "gray")
