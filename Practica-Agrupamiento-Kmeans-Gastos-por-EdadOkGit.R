### La base se establecerá directamente en el R:
datos <- data.frame(edad = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 40, 41, 42, 44, 46, 47, 48, 49, 54),
                    gastos = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 27, 29, 20, 28, 21, 30, 31, 23, 24))
plot(datos)


###  Vamos a hacer el agrupamiento o clustering usando el algoritmo K-means con 3 clusters:
set.seed(1912)
km.res=kmeans(datos, 3)
print(km.res)

### Crea un archivo con todos los registros más los clusters de cada uno
resultado <- cbind(datos, km.res$cluster)
resultado

