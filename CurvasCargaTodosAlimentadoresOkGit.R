# ==============================================
# ANÁLISIS COMPLETO DE DATOS DE ALIMENTADORES
# ==============================================
# Versión para procesar el archivo original vectors.csv
# Incluye: curvas de carga, clusterización y análisis temporal
# ==============================================

## ----------------------------
## 1. CARGAR PAQUETES
## ----------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("cluster")) install.packages("cluster")
if (!require("factoextra")) install.packages("factoextra")
if (!require("lubridate")) install.packages("lubridate")
if (!require("ggsci")) install.packages("ggsci")

library(tidyverse)
library(cluster)
library(factoextra)
library(lubridate)
library(ggsci)

## ----------------------------
## 2. CONFIGURACIÓN DE RUTA
## ----------------------------
# INGRESA AQUÍ TU RUTA COMPLETA AL ARCHIVO vectors.csv
ruta_archivo <- "G:/Outros computadores/GoogleDrive/Google Drive/Fpuna/Proyecto Planificación/Datos ANDE/datos_borrador_demanda_electrica_abril_2021/vectors.csv"

## ----------------------------
## 3. FUNCIÓN PARA LEER DATOS ORIGINALES
## ----------------------------
leer_datos_originales <- function(ruta) {
  message("\nLeyendo datos originales...")
  
  # Validar existencia del archivo
  if (!file.exists(ruta)) {
    stop("El archivo no existe en:\n", normalizePath(ruta))
  }
  
  # Leer datos (13387 filas × 27 columnas)
  datos <- read.csv(ruta, header = FALSE) %>%
    as_tibble() %>%
    # Asignar estructura conocida: 446 alimentadores × 30 días × 27 horas
    mutate(
      Alimentador_ID = rep(1:446, each = 30, length.out = n()),
      Dia_relativo = rep(1:30, times = 446, length.out = n()),
      .before = 1
    ) %>%
    # Convertir a formato largo (1 fila por medición horaria)
    pivot_longer(
      cols = V1:V27,
      names_to = "Hora",
      names_prefix = "V",
      values_to = "Corriente"
    ) %>%
    mutate(
      Hora = as.numeric(Hora),
      Corriente = ifelse(Corriente <= 0, NA, Corriente)  # Eliminar ceros/negativos
    ) %>%
    filter(!is.na(Corriente))  # Remover NA
  
  # Verificación de estructura
  if (n_distinct(datos$Alimentador_ID) != 446) {
    warning("Número inesperado de alimentadores: ", n_distinct(datos$Alimentador_ID))
  }
  
  message("Datos cargados correctamente:",
          "\n• Alimentadores: ", n_distinct(datos$Alimentador_ID),
          "\n• Días por alimentador: ", n_distinct(datos$Dia_relativo),
          "\n• Mediciones horarias: ", nrow(datos))
  
  return(datos)
}

## ----------------------------
## 4. GENERACIÓN DE CURVAS DE CARGA
## ----------------------------
generar_curvas_carga <- function(datos, id_alimentador = NULL) {
  if (!is.null(id_alimentador)) {
    datos <- filter(datos, Alimentador_ID == id_alimentador)
    titulo <- paste("Curva de Carga - Alimentador", id_alimentador)
  } else {
    titulo <- "Curvas de Carga - Todos los Alimentadores"
  }
  
  ggplot(datos, aes(x = Hora, y = Corriente)) +
    geom_line(
      aes(group = interaction(Alimentador_ID, Dia_relativo)),
      alpha = ifelse(is.null(id_alimentador), 0.1, 0.3),
      color = "steelblue"
    ) +
    stat_summary(
      fun = mean,
      geom = "line",
      size = 1.5,
      color = "firebrick"
    ) +
    labs(
      title = titulo,
      subtitle = ifelse(is.null(id_alimentador),
                        "Líneas azules: días individuales | Línea roja: promedio",
                        "Línea roja: promedio horario"),
      x = "Hora del día (0-24h)",
      y = "Corriente (A)"
    ) +
    theme_minimal() +
    scale_x_continuous(
      breaks = seq(1, 27, by = 3),
      labels = seq(0, 24, by = 3)
    )
}

## ----------------------------
## 5. CLUSTERIZACIÓN ROBUSTA
## ----------------------------
clusterizar_alimentadores <- function(datos) {
  message("\nPreparando datos para clusterización...")
  
  # Crear matriz de características (promedio por alimentador y hora)
  matriz <- datos %>%
    group_by(Alimentador_ID, Hora) %>%
    summarise(Corriente_promedio = mean(Corriente, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = Hora,
      values_from = Corriente_promedio,
      names_prefix = "Hora_"
    ) %>%
    column_to_rownames("Alimentador_ID") %>%
    as.matrix()
  
  # Manejo de valores no finitos
  matriz[!is.finite(matriz)] <- 0
  
  # Determinar número óptimo de clusters (método del codo)
  set.seed(123)
  fviz_nbclust(matriz, kmeans, method = "wss", k.max = 8) +
    labs(title = "Número Óptimo de Clusters (Método del Codo)") +
    theme_minimal()
  ggsave("numero_optimo_clusters.png", width = 8, height = 5)
  
  # Ejecutar k-means con k=4 (ajustable según gráfico anterior)
  k <- 4
  km_res <- kmeans(matriz, centers = k, nstart = 25)
  
  # Visualización 2D de clusters
  fviz_cluster(km_res, data = matriz,
               ellipse.type = "norm",
               palette = "jco",
               ggtheme = theme_minimal(),
               main = paste("Clusterización de Alimentadores (k =", k, ")"))
  ggsave("resultado_clusterizacion.png", width = 10, height = 8)
  
  # Asignar clusters a los datos originales
  datos <- datos %>%
    left_join(
      tibble(
        Alimentador_ID = as.numeric(rownames(matriz)),
        Cluster = as.factor(km_res$cluster)
      ),
      by = "Alimentador_ID"
    )
  
  message("Clusterización completada:",
          "\n• Alimentadores por cluster:")
  print(table(datos$Cluster))
  
  return(datos)
}

## ----------------------------
## 6. ANÁLISIS DE PATRONES TEMPORALES
## ----------------------------
analizar_patrones <- function(datos) {
  message("\nAnalizando patrones temporales...")
  
  # Asignar fechas reales (abril 2021)
  datos <- datos %>%
    mutate(
      Fecha = ymd("2021-04-01") + days(Dia_relativo - 1),
      Dia_semana = weekdays(Fecha, abbreviate = TRUE),
      Tipo_dia = ifelse(Dia_semana %in% c("sáb", "dom"), "Fin de semana", "Laborable")
    )
  
  # Gráfico de patrones por cluster
  p <- datos %>%
    group_by(Cluster, Hora) %>%
    summarise(
      Media = mean(Corriente),
      SD = sd(Corriente),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = Hora, y = Media)) +
    geom_line(aes(color = Cluster), size = 1.2) +
    geom_ribbon(
      aes(ymin = Media - SD, ymax = Media + SD, fill = Cluster),
      alpha = 0.2
    ) +
    scale_color_jco() +
    scale_fill_jco() +
    labs(
      title = "Patrones de Consumo por Cluster",
      subtitle = "Línea: promedio | Área: desviación estándar",
      x = "Hora del día",
      y = "Corriente (A)"
    ) +
    theme_minimal() +
    facet_wrap(~Cluster, ncol = 2) +
    scale_x_continuous(
      breaks = seq(1, 27, by = 3),
      labels = seq(0, 24, by = 3)
    )
  
  print(p)
  ggsave("patrones_por_cluster.png", width = 10, height = 8)
  
  return(datos)
}

## ----------------------------
## 7. PIPELINE PRINCIPAL
## ----------------------------
tryCatch({
  # Paso 1: Leer datos originales
  datos <- leer_datos_originales(ruta_archivo)
  
  # Paso 2: Generar curvas de carga (ejemplo para 3 alimentadores)
  generar_curvas_carga(datos, id_alimentador = 1)  # Cambiar ID según necesidad
  ggsave("curva_alimentador_ejemplo.png", width = 10, height = 6)
  
  # Paso 3: Clusterización
  datos <- clusterizar_alimentadores(datos)
  
  # Paso 4: Análisis temporal
  datos <- analizar_patrones(datos)
  
  # Paso 5: Guardar resultados
  write_csv(datos, "datos_procesados_completos.csv")
  
  message("\n¡Proceso completado con éxito!",
          "\nResultados guardados en:",
          "\n• datos_procesados_completos.csv",
          "\nGráficos guardados como PNG:",
          "\n• numero_optimo_clusters.png",
          "\n• resultado_clusterizacion.png",
          "\n• patrones_por_cluster.png",
          "\n• curva_alimentador_ejemplo.png")
  
}, error = function(e) {
  message("\nERROR: ", e$message)
  message("\nPosibles soluciones:")
  message("1. Verifica que el archivo tenga exactamente 27 columnas")
  message("2. Comprueba que los valores sean numéricos")
  message("3. Asegúrate de tener permisos de lectura/escritura")
})


# ==============================================
# ANÁLISIS DEL CLUSTER 4 (GRANJAS DE MINERÍA)
# ==============================================

# 1. Filtrar datos del Cluster 4
cluster4_data <- datos %>%
  filter(Cluster == 4) %>%
  mutate(Alimentador_ID = as.integer(Alimentador_ID))  # Asegurar tipo numérico

# 2. Obtener rango de IDs y estadísticas
ids_cluster4 <- range(cluster4_data$Alimentador_ID)
estadisticas_cluster4 <- cluster4_data %>%
  group_by(Alimentador_ID) %>%
  summarise(
    Consumo_Promedio = mean(Corriente),
    Variabilidad = sd(Corriente),
    .groups = 'drop'
  )

# 3. Calcular el centroide (perfil promedio horario)
centroide_cluster4 <- cluster4_data %>%
  group_by(Hora) %>%
  summarise(
    Corriente_media = mean(Corriente),
    Corriente_sd = sd(Corriente),
    .groups = 'drop'
  ) %>%
  mutate(
    Hora_24 = seq(0, 24, length.out = 27)[1:27]  # Convertir a escala 0-24 horas
  )

# 4. Gráfico del centroide con banda de desviación estándar
ggplot(centroide_cluster4, aes(x = Hora_24, y = Corriente_media)) +
  geom_ribbon(
    aes(ymin = Corriente_media - Corriente_sd,
        ymax = Corriente_media + Corriente_sd),
    fill = "#4DBBD5", alpha = 0.2
  ) +
  geom_line(color = "#3C5488", size = 1.5) +
  geom_point(color = "#E64B35", size = 3) +
  labs(
    title = "Perfil de Carga del Cluster 4 (Granjas de Minería)",
    subtitle = paste(
      "IDs de alimentadores:", ids_cluster4[1], "a", ids_cluster4[2],
      "| Total:", n_distinct(cluster4_data$Alimentador_ID), "alimentadores"
    ),
    x = "Hora del día",
    y = "Corriente (A)",
    caption = paste(
      "Consumo promedio:", round(mean(estadisticas_cluster4$Consumo_Promedio), 1), "A",
      "| Variabilidad media:", round(mean(estadisticas_cluster4$Variabilidad), 1), "A"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 24, by = 3),
    limits = c(0, 24)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#666666")
  )

# Guardar gráfico
ggsave("cluster4_granjas_mineria.png", width = 10, height = 6, dpi = 300)

# 5. Tabla resumen de alimentadores
alimentadores_cluster4 <- estadisticas_cluster4 %>%
  arrange(desc(Consumo_Promedio)) %>%
  mutate(
    Tipo = case_when(
      Variabilidad < 20 & Consumo_Promedio > 150 ~ "Posible granja de minería",
      TRUE ~ "Otro perfil atípico"
    )
  )

# Mostrar resumen en consola
message("\nRESUMEN CLUSTER 4 (Granjas de Minería)")
message("-------------------------------------")
message("Rango de IDs: ", ids_cluster4[1], " a ", ids_cluster4[2])
message("Total alimentadores: ", n_distinct(cluster4_data$Alimentador_ID))
message("\nTop 5 alimentadores con mayor consumo:")
print(head(alimentadores_cluster4, 5))

# Guardar tabla completa
write_csv(alimentadores_cluster4, "alimentadores_cluster4.csv")

# 6. Gráfico adicional: distribución de consumos
ggplot(alimentadores_cluster4, aes(x = Consumo_Promedio, fill = Tipo)) +
  geom_histogram(bins = 15, alpha = 0.8) +
  scale_fill_manual(values = c("#E64B35", "#4DBBD5")) +
  labs(
    title = "Distribución de Consumos en Cluster 4",
    x = "Corriente Promedio (A)",
    y = "Número de Alimentadores"
  ) +
  theme_minimal()
ggsave("distribucion_consumos_cluster4.png", width = 8, height = 5)

