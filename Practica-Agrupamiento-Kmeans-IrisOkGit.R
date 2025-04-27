### Lectura de los datos del propio R
data(iris)
datos <- iris
View(datos)

### Gráfico con los datos
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

### Ejecuta el K-means para agrupar por largo y ancho de las pétalas (iris[, 3:4]) de Íris en 3 grupos o clusters
set.seed(1912)
irisCluster <- kmeans(iris[, 3:4], 3)
irisCluster

### Crea una tabla con los clusters e instancias
table(irisCluster$cluster, iris$Species)

### Crea un archivo con todos los registros, más los clusters de cada uno
resultado <- cbind(datos, irisCluster$cluster)
resultado

