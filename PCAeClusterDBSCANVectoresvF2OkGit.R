##############################################
## CARGAR LIBRERÍAS
##############################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)       # Para suavizado de series
library(dbscan)    # Para clusterización
library(factoextra)# Para visualización
library(cluster)   # Para análisis de clusters
library(RColorBrewer) # Para paletas de colores

##############################################
## 1. CARGAR Y PREPARAR DATOS
##############################################
# Leer datos normalizados
# Desde Git
url <- "https://raw.githubusercontent.com/vneumannufprbr/TrabajosRStudio/main/datos_normalizados.csv"
View(url)
datos_normalizados <- read.csv(url,  header = TRUE,
                               stringsAsFactors = FALSE
) %>%
  mutate(
    Alimentador = as.character(Alimentador),
    Hora = as.numeric(Hora)
  )

# Desde computador
#datos_normalizados <- read.csv(
#  "G:/Outros computadores/GoogleDrive/Google Drive/Fpuna/Proyecto Planificación/Datos ANDE/datos_borrador_demanda_electrica_abril_2021/datos_normalizados.csv",
#  header = TRUE,
#  stringsAsFactors = FALSE
#) %>%
#  mutate(
#    Alimentador = as.character(Alimentador),
#    Hora = as.numeric(Hora)
#  )

# Verificar estructura
str(datos_normalizados)
summary(datos_normalizados$Corriente_Norm)  # Debe estar entre 0-1

##############################################
## 2. SUAVIZADO DE CURVAS (VERSIÓN ROBUSTA)
##############################################

# Convertir a formato wide
datos_wide <- datos_normalizados %>%
  pivot_wider(
    names_from = Hora,
    values_from = Corriente_Norm,
    id_cols = Alimentador
  ) %>%
  as.data.frame()

# Asignar nombres de fila y convertir a matriz
rownames(datos_wide) <- paste0("ID_", 1:nrow(datos_wide))  # IDs únicos
datos_mat <- as.matrix(datos_wide[, -1])  # Excluir columna Alimentador

# Función mejorada de suavizado
suavizar_curva <- function(x) {
  if(sum(!is.na(x)) >= 3) {
    zoo::rollmean(x, k = 3, fill = NA, align = "center")
  } else if(sum(!is.na(x)) > 0) {
    # Interpolación lineal cuando hay al menos 2 puntos
    approx(1:length(x), x, n = length(x))$y
  } else {
    rep(median(x, na.rm = TRUE), length(x))  # Mediana global si todos son NA
  }
}

# Aplicar suavizado con manejo de errores
datos_suavizados <- tryCatch({
  t(apply(datos_mat, 1, suavizar_curva))
}, error = function(e) {
  message("Error en suavizado: ", e$message)
  datos_mat  # Retorna datos originales si falla
})

# Reemplazar NAs residuales
datos_suavizados[is.na(datos_suavizados)] <- 0

##############################################
## 3. CLUSTERIZACIÓN DBSCAN (VERSIÓN ROBUSTA)
##############################################

# Filtrar filas completamente NA (si existen)
datos_para_dbscan <- datos_suavizados[rowSums(is.na(datos_suavizados)) != ncol(datos_suavizados), ]

# Determinar parámetros automáticamente
determinar_parametros <- function(datos, k = 5) {
  tryCatch({
    distancias <- dbscan::kNNdist(datos, k = k)
    eps <- quantile(distancias, 0.8)  # Percentil 80 más conservador
    
    # Gráfico diagnóstico
    plot(sort(distancias), type = "l", 
         main = paste("Distancias al", k, "vecino más cercano"),
         ylab = "Distancia")
    abline(h = eps, col = "red", lty = 2)
    
    return(list(eps = eps, minPts = k))
  }, error = function(e) {
    message("Advertencia: Usando valores por defecto (", e$message, ")")
    return(list(eps = 0.3, minPts = 5))
  })
}

params <- determinar_parametros(datos_para_dbscan)
print(paste("Parámetros usados: eps =", params$eps, "| minPts =", params$minPts))

# Ejecutar DBSCAN
dbscan_result <- dbscan(datos_para_dbscan, 
                        eps = params$eps, 
                        minPts = params$minPts)

# Mapear resultados
resultados_dbscan <- data.frame(
  ID = rownames(datos_wide),
  Alimentador_Original = datos_wide$Alimentador,
  Cluster = NA_integer_,
  stringsAsFactors = FALSE
)

# Asignar clusters (solo a filas procesadas)
ids_procesados <- rownames(datos_para_dbscan)
resultados_dbscan[resultados_dbscan$ID %in% ids_procesados, "Cluster"] <- dbscan_result$cluster

##############################################
## 4. VISUALIZACIÓN MEJORADA
##############################################

# Reducción de dimensionalidad
pca <- prcomp(datos_para_dbscan, scale. = FALSE)

# Preparar datos para ggplot
plot_data <- data.frame(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  Cluster = as.factor(dbscan_result$cluster)
)

# 1. Gráfico PCA con clusters (versión mejorada)
ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  stat_ellipse(aes(group = Cluster), type = "norm", level = 0.9, linetype = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Clusterización DBSCAN en Espacio PCA",
       subtitle = paste(length(unique(dbscan_result$cluster))-1, "clusters identificados"),
       caption = paste("Outliers:", sum(dbscan_result$cluster == 0))) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 2. Preparación de datos para gráfico temporal
datos_plot <- datos_normalizados %>%
  left_join(
    data.frame(
      Alimentador = rownames(datos_wide),
      Cluster = as.factor(ifelse(rownames(datos_wide) %in% rownames(datos_para_dbscan),
                                 dbscan_result$cluster,
                                 NA))
    ),
    by = "Alimentador"
  ) %>%
  mutate(
    Tipo = case_when(
      is.na(Cluster) ~ "No procesado",
      Cluster == 0 ~ "Outlier",
      TRUE ~ paste("Cluster", Cluster)
    )
  )

# 3. Gráfico de series temporales con paleta dinámica
n_clusters <- length(unique(na.omit(dbscan_result$cluster[dbscan_result$cluster != 0])))  # Excluye outliers y NA
colores <- c(
  brewer.pal(min(n_clusters, 9), "Set1"),  # Máximo 9 colores cualitativos
  "#696969",  # Color para outliers
  "#C0C0C0"   # Color para no procesados
)

ggplot(datos_plot, aes(x = Hora, y = Corriente_Norm)) +
  geom_line(aes(group = Alimentador, color = Tipo), alpha = 0.15) +
  stat_summary(
    aes(group = Tipo, color = Tipo),
    fun = mean, geom = "line", size = 1.3
  ) +
  scale_color_manual(values = colores) +
  labs(
    title = "Patrones de Consumo por Cluster",
    subtitle = paste(n_clusters, "clusters identificados"),
    y = "Corriente Normalizada",
    color = "Clasificación"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  ) +
  guides(color = guide_legend(nrow = 3))

##############################################
## 5. EXPORTACIÓN DE RESULTADOS
##############################################

# Guardar datos clusterizados
datos_clusterizados <- datos_normalizados %>%
  left_join(resultados_dbscan, by = c("Alimentador" = "Alimentador_Original"))

write.csv(
  datos_clusterizados,
  "G:/Outros computadores/GoogleDrive/Google Drive/Fpuna/Proyecto Planificación/Datos ANDE/datos_borrador_demanda_electrica_abril_2021/datos_clusterizados_dbscan.csv",
  row.names = FALSE
)

# Generar reporte
sink("G:/Outros computadores/GoogleDrive/Google Drive/Fpuna/Proyecto Planificación/Datos ANDE/datos_borrador_demanda_electrica_abril_2021/resumen_analisis.txt")
cat("=== RESUMEN DE ANÁLISIS ===\n\n")
cat("Fecha análisis:", format(Sys.Date(), "%Y-%m-%d"), "\n\n")
cat("PARÁMETROS DBSCAN:\n")
cat("- eps:", params$eps, "\n")
cat("- minPts:", params$minPts, "\n\n")
cat("RESULTADOS:\n")
cat("- Alimentadores totales:", nrow(datos_wide), "\n")
cat("- Alimentadores procesados:", sum(!is.na(resultados_dbscan$Cluster)), "\n")
cat("- Clusters identificados:", max(dbscan_result$cluster), "\n")
cat("- Alimentadores atípicos:", sum(dbscan_result$cluster == 0), "\n")
cat("- Alimentadores no procesados:", sum(is.na(resultados_dbscan$Cluster)), "\n")
sink()

message("Análisis completado exitosamente. Archivos guardados en:")
message(paste0("G:/Outros computadores/GoogleDrive/Google Drive/Fpuna/Proyecto Planificación/Datos ANDE/datos_borrador_demanda_electrica_abril_2021", "/resumen_analisis.txt"))
        
        