### Lectura de datos
data("mtcars")
datos=scale(mtcars)
View(datos)

### Ejecuta el método K-means
set.seed(1912)
km.res=kmeans(datos, 4, nstart=25)
print(km.res)

### Crea un archivo con todos los registros más los clusters de cada uno
resultado <- cbind(datos, km.res$cluster)
resultado


