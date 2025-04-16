# Cargar paquetes necesarios (añade tibble)
required_packages <- c("tidyverse", "lubridate", "cluster", "factoextra", "ggdendro")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(dplyr)
library(tidyr)
library(tibble)  # Paquete que contiene column_to_rownames()
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(cluster)


## -------------------------------------------------------------------
## 1. Cargar datos normalizados (previamente guardados)
## -------------------------------------------------------------------
# Obtener archivo desde Notebook
# datos_normalizados <- read.csv("C:/Users/UFPR/Transmissão no Google Drive/Outros computadores/GoogleDrive/Google Drive/Fpuna/Proyecto Planificación/Datos ANDE/datos_borrador_demanda_electrica_abril_2021/datos_normalizados.csv",  # CAMBIAR POR LA RUTA REAL
# header = TRUE
# )
# Obtener archivo desde Desktop
datos_normalizados <- read.csv(
  "G:/Outros computadores/GoogleDrive/Google Drive/Fpuna/Proyecto Planificación/Datos ANDE/datos_borrador_demanda_electrica_abril_2021/datos_normalizados.csv",
  header = TRUE
)

## ---------------------------------------------------------------
## 1. Preparación de datos (wide format para PCA)
## ---------------------------------------------------------------
datos_wide <- datos_normalizados %>%
  select(Alimentador, Hora, Corriente_Norm) %>%
  pivot_wider(
    names_from = Hora,
    values_from = Corriente_Norm,
    names_prefix = "Hora_"
  ) %>% 
  as.data.frame() %>%  # Convertir a dataframe estándar
  remove_rownames() %>% 
  column_to_rownames("Alimentador")  # Ahora funcionará

## ---------------------------------------------------------------
## 2. PCA - Reducción de dimensionalidad
## ---------------------------------------------------------------
pca_result <- PCA(datos_wide, graph = FALSE, ncp = 5)  # Conservamos 5 componentes

# Visualizar varianza explicada
fviz_eig(pca_result, addlabels = TRUE) +
  labs(title = "Varianza explicada por componentes PCA")

## ---------------------------------------------------------------
## 3. K-means en componentes principales (7 clusters)
## ---------------------------------------------------------------
set.seed(123)  # Para reproducibilidad
k <- 7  # Número de clusters

# Usamos los primeros 3 componentes (ajustar según varianza explicada)
cluster_result <- kmeans(pca_result$ind$coord[, 1:3], centers = k, nstart = 25)

# Añadir asignación de clusters a los datos
datos_clusterizados <- datos_normalizados %>%
  mutate(Cluster = as.factor(cluster_result$cluster[as.character(Alimentador)]))

## ---------------------------------------------------------------
## 4. Visualización de resultados
## ---------------------------------------------------------------
# Gráfico de clusters en espacio PCA
fviz_cluster(
  list(data = pca_result$ind$coord[, 1:2], cluster = cluster_result$cluster),
  ellipse.type = "norm",
  repel = TRUE,
  labelsize = 8
) +
  labs(title = "Clusters en espacio PCA (2D)")

# Perfiles promedio por cluster
ggplot(datos_clusterizados, aes(x = Hora, y = Corriente_Norm, group = Alimentador)) +
  geom_line(alpha = 0.1, aes(color = Cluster)) +
  stat_summary(
    aes(group = Cluster, color = Cluster),
    fun = mean, geom = "line", size = 1.5
  ) +
  scale_color_viridis_d() +
  labs(title = "Patrones promedio por cluster", y = "Corriente normalizada") +
  theme_minimal()

## ---------------------------------------------------------------
## 5. Análisis de clusters
## ---------------------------------------------------------------
# Estadísticos por cluster
estadisticos_clusters <- datos_clusterizados %>%
  group_by(Cluster, Hora) %>%
  summarise(
    Media = mean(Corriente_Norm),
    SD = sd(Corriente_Norm),
    .groups = "drop"
  )

# Mostrar perfiles horarios
ggplot(estadisticos_clusters, aes(x = Hora, y = Media, color = Cluster)) +
  geom_line(size = 1) +
  geom_ribbon(
    aes(ymin = Media - SD, ymax = Media + SD, fill = Cluster),
    alpha = 0.2
  ) +
  labs(title = "Perfiles promedio con desviación estándar") +
  theme_minimal()

## ---------------------------------------------------------------
## 6. Exportar resultados
## ---------------------------------------------------------------
write.csv(
  datos_clusterizados,
  "G:/Outros computadores/GoogleDrive/Google Drive/Fpuna/Proyecto Planificación/Datos ANDE/datos_borrador_demanda_electrica_abril_2021/datos_clusterizados.csv",
  row.names = FALSE
)

# 1. Interpretar visualmente los componentes principales (PCA)
# Una forma muy útil es analizar los loadings (cargas), que muestran cuánto 
# contribuye cada hora del día a cada componente.

# Obtener los loadings (contribuciones de cada variable original a los componentes)
loadings <- pca_result$var$coord

# Visualizar el primer componente
plot(loadings[1:24, 1], type = "l", col = "blue", lwd = 2,
     xlab = "Hora del día", ylab = "Carga",
     main = "Contribución de cada hora al 1er Componente Principal")
abline(h = 0, lty = 2)

# Este gráfico te dice:
# Qué horas tienen mayor peso en el componente.
# Si hay picos positivos o negativos, pueden estar diferenciando, por ejemplo, 
# consumo diurno vs. nocturno

# Puedes repetirlo para otros componentes: 2º
plot(loadings[1:24, 2], type = "l", col = "red", lwd = 2,
     xlab = "Hora", ylab = "Carga",
     main = "2do Componente Principal")

# Puedes repetirlo para otros componentes: 3º
plot(loadings[1:24, 3], type = "l", col = "red", lwd = 2,
     xlab = "Hora", ylab = "Carga",
     main = "3do Componente Principal")

# Puedes repetirlo para otros componentes: 4º
plot(loadings[1:24, 4], type = "l", col = "red", lwd = 2,
     xlab = "Hora", ylab = "Carga",
     main = "4to Componente Principal")

# Puedes repetirlo para otros componentes: 5º
plot(loadings[1:24, 5], type = "l", col = "red", lwd = 2,
     xlab = "Hora", ylab = "Carga",
     main = "5to Componente Principal")

# 2. Reconstruir un perfil horario desde componentes principales
# ¿Qué significa reconstruir?
#  Si tomás los valores de los componentes principales (por ejemplo, 
# los 5 primeros) de un alimentador y los multiplicás por las cargas originales,
# podés recrear una versión aproximada del perfil horario original.

# Escoger un alimentador (por ejemplo el primero)
componentes <- pca_result$ind$coord[1, 1:5]  # Sus 5 primeros componentes
cargas <- pca_result$var$coord[, 1:5]        # Las cargas (horas x 5 componentes)

# Reconstrucción aproximada
perfil_reconstruido <- as.numeric(componentes %*% t(cargas))

# Visualizar el perfil original vs. reconstruido
original <- datos_wide[1, ]  # El original con 24 horas

# Comparar
plot(unlist(original), type = "l", col = "blue", lwd = 2, ylim = range(c(original, perfil_reconstruido)),
     ylab = "Corriente Normalizada", xlab = "Hora", main = "Perfil Original vs. Reconstruido")
lines(perfil_reconstruido, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Original", "Reconstruido (PCA)"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# ¿Qué vas a ver?
# Si la reconstrucción con 5 componentes es buena, las dos curvas (original y 
# reconstruida) serán muy similares.

# Si ves muchas diferencias, podrías necesitar más componentes (por ejemplo, 6 o 7).

# Con esto podés:
#  Interpretar cada componente como una firma o patrón (ej: carga matutina, 
# picos vespertinos).

# Visualizar cómo se combinan para formar perfiles horarios reales.

# Y eso explica por qué los K-means trabajan sobre componentes: 
# capturan lo esencial del comportamiento con menos ruido.


library(FactoMineR)
library(factoextra)
library(ggplot2)
library(gridExtra)

# ---------------------------
# 1. Verificar columnas horarias
# ---------------------------

# Detectamos columnas que empiecen con "Hora_" o que sean numéricas del 0 al 23
columnas_hora <- grep("^Hora_|^\\d+$", colnames(datos_wide), value = TRUE)
num_horas <- length(columnas_hora)

# Extraer solo las columnas horarias como matriz
datos_horas <- datos_wide[, columnas_hora]

# ---------------------------
# 2. Ejecutar PCA (si aún no se hizo)
# ---------------------------
pca_result <- PCA(datos_horas, graph = FALSE, ncp = 5)

# ---------------------------
# 3. Reconstrucción usando componentes
# ---------------------------
num_componentes <- 5
componentes_pca <- pca_result$ind$coord[, 1:num_componentes]
cargas_pca <- pca_result$var$coord[, 1:num_componentes]

# Reconstrucción
reconstrucciones <- as.matrix(componentes_pca) %*% t(as.matrix(cargas_pca))
colnames(reconstrucciones) <- columnas_hora
rownames(reconstrucciones) <- rownames(datos_wide)
reconstrucciones_df <- as.data.frame(reconstrucciones)

# ---------------------------
# 4. Graficar perfiles: original vs reconstruido
# ---------------------------
num_alimentadores_mostrar <- 6
alimentadores_mostrar <- rownames(datos_wide)[1:num_alimentadores_mostrar]

plots <- lapply(alimentadores_mostrar, function(nombre) {
  original <- as.numeric(datos_horas[nombre, ])
  reconstruido <- as.numeric(reconstrucciones_df[nombre, ])
  
  df_plot <- data.frame(
    Hora = 0:(num_horas - 1),
    Original = original,
    Reconstruido = reconstruido
  )
  
  ggplot(df_plot, aes(x = Hora)) +
    geom_line(aes(y = Original), color = "blue", size = 1.2) +
    geom_line(aes(y = Reconstruido), color = "red", linetype = "dashed", size = 1.2) +
    labs(title = paste("Alimentador:", nombre),
         y = "Corriente Normalizada") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12))
})

do.call(grid.arrange, c(plots, ncol = 2))

# Bloque actualizado de análisis PCA con selección automática de componentes
library(FactoMineR)
library(factoextra)

# Ejecutar PCA (si no lo hiciste antes)
pca_result <- PCA(datos_wide, graph = FALSE)

# Extraer varianza explicada
var_acumulada <- pca_result$eig[, 3]  # Columna 3 = varianza acumulada
num_componentes_90 <- which(var_acumulada >= 90)[1]

# Mostrar en consola
cat("✅ Número mínimo de componentes que explican al menos el 90% de la varianza:", num_componentes_90, "\n")

# Visualizar varianza explicada y resaltar el corte automático
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100)) +
  geom_vline(xintercept = num_componentes_90, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Varianza explicada por componentes principales",
       subtitle = paste("Se necesitan", num_componentes_90, "componentes para explicar al menos el 90%"))

