### Paquetes requeridos:
install.packages("e1071")
install.packages("kernlab")
install.packages("caret")
install.packages("mice")
install.packages("pROC")

library(caret)
library(mice)
library(pROC)

### Lectura del archivo de datos CancerdeMamaDatos.csv de Github del Prof.Neumann
url1 <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/CancerdeMamaDatos.csv"
dados <- read.csv(url1)

### Elimina el ID y rellena los valores que faltan
dados$Id <- NULL
imp <- mice(dados)
dados <- complete(imp, 1)

### Crear bases de entrenamiento y pruebas
set.seed(1912)
indices <- createDataPartition(dados$Class, p=0.80, list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

### Control para entrenamiento con validación cruzada y probabilidades habilitadas
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

### Entrenar SVM con base de entrenamiento usando validación cruzada y optimización por AUC
set.seed(1912)
svm <- train(Class ~ ., data=treino,
             method="svmRadial",
             trControl=ctrl,
             metric="ROC",
             preProcess = c("center", "scale"))

print(svm)

### Predicción con clases
predict.svm <- predict(svm, teste)
print(confusionMatrix(predict.svm, as.factor(teste$Class)))

# Calcular F1-score (macro)
f1 <- F_meas(data = predict.svm, reference = as.factor(teste$Class))
print(paste("F1-score:", round(f1, 4)))

### Predicción con probabilidades (para ROC)
predict.svm.prob <- predict(svm, teste, type = "prob")

### Crear objeto ROC para la clase positiva "benign" (ajusta si es otra)
roc_obj <- roc(response = teste$Class, predictor = predict.svm.prob$benign)

### Graficar curva ROC
plot(roc_obj, col = "blue", lwd = 2, main = "Curva ROC para SVM")

### Calcular y mostrar AUC
auc_value <- auc(roc_obj)
print(paste("AUC:", round(auc_value, 4)))
