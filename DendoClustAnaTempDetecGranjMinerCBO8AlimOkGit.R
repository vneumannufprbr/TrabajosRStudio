# ANÁLISIS AVANZADO DE ALIMENTADORES - CLUSTERIZACIÓN Y DETECCIÓN DE MINERÍA
# ========================================================================

# 1. INSTALACIÓN Y CARGA DE PAQUETES --------------------------------------
required_packages <- c("tidyverse", "lubridate", "cluster", "factoextra", "dbscan", "ggdendro")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)
library(lubridate)
library(cluster)
library(factoextra)
library(dbscan)
library(ggdendro)

# 2. FUNCIÓN DE LECTURA DE DATOS -----------------------------------------
leer_datos_ande <- function(ruta_archivo) {
  message("Procesando archivo ANDE...")
  
  # Leer archivo con encoding Latin1
  lineas <- readLines(ruta_archivo, encoding = "Latin1")
  
  # Procesar cada línea (omitir encabezado)
  datos <- map_df(lineas[-1], function(linea) {
    # Extraer componentes con regex
    partes <- str_match(linea, '"(\\d+/\\d+/\\d+),(\\d+:\\d+),""([\\d,]+)"",""([\\d,]+)"",""([\\d,]+)"",""([\\d,]+)"",""([\\d,]+)"",""([\\d,]+)"",""([\\d,]+)"",""([\\d,]+)""')
    
    if(is.na(partes[1,1])) return(NULL)
    
    # Convertir valores
    tibble(
      fecha = dmy(partes[1,2]),
      hora = as.numeric(str_extract(partes[1,3], "\\d+")),
      AL01 = as.numeric(str_replace(partes[1,4], ",", ".")),
      AL02 = as.numeric(str_replace(partes[1,5], ",", ".")),
      AL03 = as.numeric(str_replace(partes[1,6], ",", ".")),
      AL04 = as.numeric(str_replace(partes[1,7], ",", ".")),
      AL05 = as.numeric(str_replace(partes[1,8], ",", ".")),
      AL06 = as.numeric(str_replace(partes[1,9], ",", ".")),
      AL07 = as.numeric(str_replace(partes[1,10], ",", ".")),
      AL08 = as.numeric(str_replace(partes[1,11], ",", "."))
    )
  })
  
  # Convertir a formato largo
  datos_largo <- datos %>%
    pivot_longer(
      cols = starts_with("AL"),
      names_to = "Alimentador",
      values_to = "MW",
      values_drop_na = TRUE
    ) %>%
    filter(!is.na(MW), !is.na(fecha), !is.na(hora))
  
  message("Datos procesados: ",
          nrow(datos_largo), " registros de ",
          n_distinct(datos_largo$Alimentador), " alimentadores")
  
  return(datos_largo)
}

# 3. PREPARACIÓN PARA CLUSTERIZACIÓN --------------------------------------
preparar_para_cluster <- function(datos) {
  datos %>%
    group_by(Alimentador, hora) %>%
    summarise(MW_promedio = mean(MW), .groups = "drop") %>%
    pivot_wider(
      names_from = hora,
      values_from = MW_promedio,
      names_prefix = "hora_"
    ) %>%
    column_to_rownames("Alimentador") %>%
    as.matrix() %>%
    scale() %>%
    replace(!is.finite(.), 0)
}

# 4. CLUSTERIZACIÓN JERÁRQUICA -------------------------------------------
ejecutar_clusterizacion <- function(matriz) {
  # Calcular distancias
  dist_mat <- dist(matriz, method = "euclidean")
  
  # Clusterización
  hc <- hclust(dist_mat, method = "ward.D2")
  
  # Visualizar dendrograma
  dendro_plot <- ggdendrogram(hc, rotate = TRUE) +
    labs(title = "Dendrograma de Alimentadores",
         subtitle = "Método: Ward.D2 | Distancia Euclidiana") +
    theme_minimal()
  
  print(dendro_plot)
  ggsave("dendrograma_alimentadores.png", width = 10, height = 6)
  
  return(hc)
}

# 5. ASIGNACIÓN DE CLUSTERS ----------------------------------------------
asignar_clusters <- function(datos, hc, k = 4) {
  clusters <- cutree(hc, k = k)
  
  datos_cluster <- datos %>%
    mutate(cluster = as.factor(clusters[Alimentador]))
  
  # Visualización
  cluster_plot <- fviz_cluster(
    list(data = matriz, cluster = clusters),
    ellipse.type = "norm",
    palette = "jco",
    ggtheme = theme_minimal()
  )
  
  print(cluster_plot)
  ggsave("resultado_clusterizacion.png", width = 10, height = 8)
  
  return(datos_cluster)
}

# 6. DETECCIÓN DE GRANJAS DE MINERÍA -------------------------------------
detectar_mineria <- function(datos_cluster) {
  # Identificar cluster con perfil de minería
  perfil_clusters <- datos_cluster %>%
    group_by(cluster, hora) %>%
    summarise(MW_promedio = mean(MW), .groups = "drop") %>%
    group_by(cluster) %>%
    summarise(
      variabilidad = sd(MW_promedio),
      consumo_medio = mean(MW_promedio),
      .groups = "drop"
    )
  
  cluster_mineria <- perfil_clusters %>%
    filter(variabilidad < 5, consumo_medio > quantile(consumo_medio, 0.75)) %>%
    pull(cluster) %>%
    as.character()
  
  if(length(cluster_mineria) == 0) {
    message("No se detectaron clusters con perfil de minería")
    return(datos_cluster %>% mutate(es_mineria = "Normal"))
  }
  
  message("Cluster de minería detectado: ", cluster_mineria)
  
  # Graficar perfil
  perfil_plot <- datos_cluster %>%
    filter(cluster == cluster_mineria) %>%
    group_by(hora) %>%
    summarise(
      MW_promedio = mean(MW),
      MW_sd = sd(MW),
      .groups = "drop"
    ) %>%
    ggplot(aes(hora, MW_promedio)) +
    geom_ribbon(aes(ymin = MW_promedio - MW_sd, ymax = MW_promedio + MW_sd), 
                alpha = 0.2, fill = "red") +
    geom_line(color = "red", size = 1) +
    labs(title = paste("Perfil Cluster", cluster_mineria),
         subtitle = "Posible granja de minería",
         x = "Hora", y = "MW") +
    theme_minimal()
  
  print(perfil_plot)
  ggsave("cluster_mineria.png", width = 10, height = 6)
  
  datos_cluster %>%
    mutate(es_mineria = ifelse(cluster == cluster_mineria, "Sospechoso", "Normal"))
}

# 7. ANÁLISIS TEMPORAL ---------------------------------------------------
analizar_patrones_temporales <- function(datos) {
  datos_temp <- datos %>%
    mutate(
      dia_semana = wday(fecha, label = TRUE, abbr = TRUE),
      tipo_dia = ifelse(dia_semana %in% c("sáb", "dom"), "Fin semana", "Laborable")
    )
  
  patrones_plot <- datos_temp %>%
    group_by(cluster, hora, tipo_dia) %>%
    summarise(MW_promedio = mean(MW), .groups = "drop") %>%
    ggplot(aes(hora, MW_promedio, color = tipo_dia)) +
    geom_line(size = 1) +
    facet_wrap(~cluster, scales = "free_y", ncol = 2) +
    labs(title = "Patrones por Cluster", x = "Hora", y = "MW") +
    theme_minimal()
  
  print(patrones_plot)
  ggsave("patrones_temporales.png", width = 12, height = 8)
  
  return(datos_temp)
}

# 8. EJECUCIÓN PRINCIPAL -------------------------------------------------
# Obtener archivo desde Notebook
# ruta_archivo <- "C:/Users/UFPR/Transmissão no Google Drive/Outros computadores/GoogleDrive/Google Drive/Fpuna/Proyecto Planificación/Datos ANDE/Curvas de carga 2024/Coronel Bogado marzo 2025csv.csv"  # CAMBIAR POR LA RUTA REAL
# Obtener archivo desde Desktop
ruta_archivo <- "G:/Outros computadores/GoogleDrive/Google Drive/Fpuna/Proyecto Planificación/Datos ANDE/Curvas de carga 2024/Coronel Bogado marzo 2025csv.csv"

#ruta_carpeta_results <- "G:/Outros computadores/GoogleDrive/Google Drive/Fpuna/1. Curso IA FPUNA/Módulo 2. Aprendizaje de Máquina. 12h/Agrupación/Predicciones con R/Avanzado_Clusteriazación_Jerarquica/"

tryCatch({
  # Paso 1: Cargar datos
  datos <- leer_datos_ande(ruta_archivo)
  
  # Paso 2: Preparar para clusterización
  matriz <- preparar_para_cluster(datos)
  
  # Paso 3: Clusterización
  hc <- ejecutar_clusterizacion(matriz)
  
  # Paso 4: Asignar clusters
  datos_cluster <- asignar_clusters(datos, hc)
  
  # Paso 5: Detectar minería
  datos_mineria <- detectar_mineria(datos_cluster)
  
  # Paso 6: Análisis temporal
  datos_final <- analizar_patrones_temporales(datos_mineria)
  
  # Guardar resultados
  write_csv(datos_final, "resultados_clusterizacion.csv")
  
  message("\n¡Análisis completado!")
  message("Resultados guardados en:")
  message("- resultados_clusterizacion.csv")
  message("Gráficos guardados:")
  message("- dendrograma_alimentadores.png")
  message("- resultado_clusterizacion.png")
  message("- cluster_mineria.png")
  message("- patrones_temporales.png")
  
}, error = function(e) {
  message("Error: ", e$message)
  message("Sugerencias:")
  message("1. Verifique la ruta del archivo")
  message("2. Revise el formato de los datos")
  message("3. Ejecute warnings() para ver advertencias")
})

# 9. ANÁLISIS ADICIONAL PARA CLUSTER DE MINERÍA --------------------------
if(exists("datos_final")) {
  cluster_mineria <- datos_final %>%
    filter(es_mineria == "Sospechoso") %>%
    pull(cluster) %>%
    unique()
  
  if(length(cluster_mineria) > 0) {
    message("\nANÁLISIS DETALLADO CLUSTER ", cluster_mineria, " (MINERÍA)")
    
    # Estadísticas
    stats <- datos_final %>%
      filter(cluster == cluster_mineria) %>%
      group_by(Alimentador) %>%
      summarise(
        MW_promedio = mean(MW),
        MW_max = max(MW),
        .groups = "drop"
      ) %>%
      arrange(desc(MW_promedio))
    
    message("\nTop alimentadores:")
    print(head(stats, 5))
    
    # Gráfico detallado
    detalle_plot <- datos_final %>%
      filter(cluster == cluster_mineria) %>%
      ggplot(aes(hora, MW, group = interaction(fecha, Alimentador))) +
      geom_line(alpha = 0.2, color = "red") +
      stat_summary(fun = mean, geom = "line", size = 1.5, color = "black") +
      labs(title = paste("Detalle Cluster", cluster_mineria),
           x = "Hora", y = "MW") +
      theme_minimal()
    
    print(detalle_plot)
    ggsave("detalle_cluster_mineria.png", width = 12, height = 6)
    
    # Guardar datos del cluster
    datos_final %>%
      filter(cluster == cluster_mineria) %>%
      write_csv("alimentadores_sospechosos.csv")
  }
}

# Despues de identificar al Alimentador sospechoso
# Generar reporte específico para AL08
datos %>% 
  filter(Alimentador == "AL08") %>%
  group_by(hora) %>%
  summarise(
    MW_promedio = mean(MW),
    MW_pico = max(MW),
    Desviación = sd(MW)
  ) %>%
  write_csv("reporte_al08_mineria.csv")

# Script para monitoreo diario
monitoreo_mineria <- function(datos) {
  datos %>%
    filter(Alimentador == "AL08") %>%
    group_by(fecha) %>%
    summarise(
      Consumo_diario = sum(MW),
      Variabilidad = sd(MW)
    ) %>%
    ggplot(aes(fecha, Consumo_diario)) +
    geom_line(color = "red") +
    labs(title = "Consumo Diario AL08 - Posible Granja de Minería")
}

# Calcular costo energético estimado
datos %>%
  filter(Alimentador == "AL08") %>%
  summarise(
    Consumo_mensual_MWh = sum(MW) * 24 * 30 / 1000,
    Costo_estimado = Consumo_mensual_MWh * 40  # Asumiendo $40/MWh
  )

