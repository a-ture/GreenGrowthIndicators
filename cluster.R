library(dplyr)
library(tidyr)
library(cluster)
library(ggplot2)
library(ggdendro)
library(scatterplot3d)

# Imposta la directory di destinazione per il heatmap
directory_destinazione <- "Cluster"
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')

# Crea la directory se non esiste
if (!file.exists(directory_destinazione)) {
  dir.create(directory_destinazione)
}

calculate_silhouette <- function(data, cluster_labels, save_path = NULL) {
  if (!is.numeric(cluster_labels)) {
    cluster_labels <- as.numeric(as.factor(cluster_labels))
  }
  
  numeric_data <- data %>% select_if(is.numeric)
  dist_matrix <- dist(numeric_data)
  
  silhouette_score <- silhouette(cluster_labels, dist_matrix)
  avg_silhouette <- mean(silhouette_score[, 3])
  
  print(paste("Average silhouette score:", round(avg_silhouette, 2)))
  
  if (!is.null(save_path)) {
    png(save_path, width = 800, height = 600)
    plot(silhouette_score, border = NA, col = as.numeric(cluster_labels) + 1,
         main = paste("Silhouette plot - Avg Silhouette Score:", round(avg_silhouette, 2)))
    dev.off()
  } else {
    plot(silhouette_score, border = NA, col = as.numeric(cluster_labels) + 1,
         main = paste("Silhouette plot - Avg Silhouette Score:", round(avg_silhouette, 2)))
  }
  
  return(silhouette_score)
}

perform_kmeans_clustering <- function(data, variables_of_interest, variables_of_interest_t, num_clusters, iter_max = 100, nstart = 8, directory_destinazione) {
  processed_data <- data %>%
    filter(Variable %in% variables_of_interest, !is.na(Value)) %>%
    group_by(Country, Variable) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      Trend = ifelse(sum(!is.na(Value)) > 1, lm(Value ~ YEA, data = cur_data())$coefficients[2], NA),
      .groups = 'drop'
    )
  
  transposed_data <- processed_data %>%
    pivot_wider(names_from = Variable, values_from = c(Mean, SD, Trend), id_cols = Country)
  
  print("transposed_data")
  print(transposed_data)
  
  set.seed(123)
  kmeans_result <- kmeans(
    transposed_data[, -1],
    centers = num_clusters,
    iter.max = iter_max,
    nstart = nstart
  )
  
  transposed_data$Cluster <- as.factor(kmeans_result$cluster)
  
  new_column_names <- unlist(lapply(variables_of_interest, function(var) {
    c(paste0("Mean_", var), paste0("SD_", var), paste0("Trend_", var))
  }))
  
  result_columns <- c(new_column_names, "Cluster")
  results <- transposed_data[, c(result_columns)]
  
  pdf(file.path(directory_destinazione, paste0("cluster_", num_clusters, ".pdf")), width = 12, height = 8.3)
  
  unique_countries <- unique(transposed_data$Country)
  country_color_map <- setNames(rainbow(length(unique_countries)), unique_countries)
  
  unique_clusters <- sort(unique(transposed_data$Cluster))
  cluster_pch_map <- setNames(1:length(unique_clusters), unique_clusters)
  
  s3d <- scatterplot3d(
    results[[paste0("Mean_", variables_of_interest[1])]],
    results[[paste0("Mean_", variables_of_interest[2])]],
    results[[paste0("Mean_", variables_of_interest[3])]],
    color = "grey",
    pch = 19,
    main = "K-Means Clustering in 3D",
    grid = TRUE,
    col.grid = "grey",
    lty.grid = par("lty"),
    box = FALSE,
    xlab = deparse(variables_of_interest_t[1]),
    ylab = deparse(variables_of_interest_t[2]),
    zlab = deparse(variables_of_interest_t[3])
  )
  
  for (i in seq_len(nrow(results))) {
    s3d.coords <- s3d$xyz.convert(
      results[[paste0("Mean_", variables_of_interest[1])]][i],
      results[[paste0("Mean_", variables_of_interest[2])]][i],
      results[[paste0("Mean_", variables_of_interest[3])]][i]
    )
    points(s3d.coords$x, s3d.coords$y, s3d.coords$z,
           col = country_color_map[transposed_data$Country[i]],
           pch = cluster_pch_map[transposed_data$Cluster[i]])
  }
  
  addgrids3d(results[[paste0("Mean_", variables_of_interest[1])]],
             results[[paste0("Mean_", variables_of_interest[2])]],
             results[[paste0("Mean_", variables_of_interest[3])]], grid = c("xy", "xz", "yz"))
  
  legend("topright", inset = .05, title = "Paesi", legend = names(country_color_map), 
         col = country_color_map, pch = 19, cex = 0.6)
  
  legend("bottomright", inset = .05, title = "Cluster", legend = unique_clusters, 
         pch = cluster_pch_map, cex = 0.6)
  
  dev.off()
  
  print(kmeans_result)
  
  silhouette_path <- file.path(directory_destinazione, paste0("silhouette_kmeans_", num_clusters, ".png"))
  silhouette_kmeans <- calculate_silhouette(transposed_data[, -1], transposed_data$Cluster, save_path = silhouette_path)
  
  return(list(kmeans_result = kmeans_result, silhouette = silhouette_kmeans, data = transposed_data))
}

# Imposta la directory di destinazione per gli scatter plots
sotto_cartella <- "Ob1_K-means"
percorso_sotto_cartella <- file.path(directory_destinazione, sotto_cartella)

if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

variables_of_interest_ob1 <- c(
  "Renewable energy supply, % total energy supply",
  "Environmentally related taxes, % GDP",
  "Terrestrial protected area, % land area"
)

variables_of_interest_ob1_t <- c(
  "Fornitura di energia rinnovabile",
  "Tasse legate all'ambiente",
  "Area terrestre protetta"
)

# Esegui K-Means con diversi numeri di cluster
num_clusters <- c(3, 4, 2)
kmeans_results <- lapply(num_clusters, function(k) {
  perform_kmeans_clustering(dataset, variables_of_interest_ob1, variables_of_interest_ob1_t, k, iter_max = 100, nstart = 8, directory_destinazione = percorso_sotto_cartella)
})

perform_hierarchical_clustering <- function(data, variables_of_interest, variables_of_interest_t, methods, directory_destinazione, k) {
  filtered_data <- data %>%
    filter(Variable %in% variables_of_interest) %>%
    group_by(Country, Variable) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      Trend = ifelse(sum(!is.na(Value)) > 1, lm(Value ~ YEA, data = cur_data())$coefficients[2], NA),
      .groups = 'drop'
    )
  
  pivoted_data <- filtered_data %>%
    pivot_wider(names_from = Variable, values_from = c(Mean, SD, Trend), id_cols = Country)
  
  colnames(pivoted_data) <- gsub(" ", "_", gsub("^(\\d+)", "X\\1_", colnames(pivoted_data)))
  pivoted_data_matrix <- pivoted_data[, -1]
  rownames(pivoted_data_matrix) <- pivoted_data$Country
  
  results <- list()
  
  for (method in methods) {
    dist_matrix <- dist(pivoted_data_matrix, method = "euclidean")
    
    if (method %in% c("centroid", "median")) {
      dist_matrix <- dist_matrix ^ 2
    }
    
    hc <- hclust(dist_matrix, method = method)
    
    # Creazione dello Scree Plot
    heights <- hc$height
    scree_plot_df <- data.frame(NumeroCluster = seq_along(heights), DistanzaAggregazione = rev(heights))
    
    scree_plot_filename <- paste0("scree_plot_", method, ".pdf")
    g <- ggplot(scree_plot_df, aes(x = DistanzaAggregazione , y = NumeroCluster)) +
      geom_line() +
      geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 3) +
      theme_linedraw(base_size = 14) +
      scale_y_continuous(breaks = seq(from = 1, to = max(scree_plot_df$NumeroCluster), by = 4)) +
      labs(y = "Numero di cluster", x = "Distanza di aggregazione", title = paste0("Screeplot per il metodo ", method)) +
      theme(
        legend.position = "right",
        legend.key.size = unit(0.4, "cm"),
        axis.title.x = element_text(margin = margin(t = 30)),
        axis.title.y = element_text(margin = margin(r = 30)),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
      )
    
    # Salva il plot
    ggsave(file.path(directory_destinazione, scree_plot_filename), plot = g, width = 10, height = 7)
    
    # Continua con il clustering e altre analisi
    results[[method]] <- list(dendrogram = scree_plot_filename)
    
    cluster_assignments <- cutree(hc, k)
    results[[method]]$clusters <- cluster_assignments
    
    pivoted_data_clustered <- cbind(pivoted_data_matrix, Cluster = as.factor(cluster_assignments))
    aggregate_results <- aggregate(. ~ Cluster, data = pivoted_data_clustered, FUN = function(x) c(mean = mean(x, na.rm = TRUE), var = var(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
    results[[method]]$aggregate <- aggregate_results
    
    trT <- (nrow(pivoted_data_matrix) - 1) * sum(apply(pivoted_data_matrix, 2, var, na.rm = TRUE))
    agvar <- aggregate(pivoted_data_matrix, list(Cluster = cluster_assignments), var)
    trS <- sum((agvar$Cluster - 1) * rowSums(agvar[, -1], na.rm = TRUE))
    trB <- trT - trS
    
    results[[method]]$non_homogeneity_within <- trS
    results[[method]]$non_homogeneity_between <- trB
    
    print(paste("Non-omogeneity within clusters for method", method, ":", trS))
    print(paste("Non-omogeneity between clusters for method", method, ":", trB))
    
    silhouette_path <- file.path(directory_destinazione, paste0("silhouette_", method, "_hierarchical.png"))
    silhouette_hierarchical <- calculate_silhouette(pivoted_data_clustered[, -ncol(pivoted_data_clustered)], pivoted_data_clustered$Cluster, save_path = silhouette_path)
    results[[method]]$silhouette <- silhouette_hierarchical
  }
  
  return(results)
}


sotto_cartella_hierarchical <- "Ob1_Hierarchical"
percorso_sotto_cartella_hierarchical <- file.path(directory_destinazione, sotto_cartella_hierarchical)

if (!file.exists(percorso_sotto_cartella_hierarchical)) {
  dir.create(percorso_sotto_cartella_hierarchical)
}

methods <- c("single", "complete", "average", "median", "centroid")
hierarchical_results <- perform_hierarchical_clustering(dataset, variables_of_interest_ob1, variables_of_interest_ob1_t, methods, percorso_sotto_cartella_hierarchical, 2)

perform_sensitivity_analysis <- function(data, variables_of_interest, cluster_range, distance_metrics, linkage_methods, directory_destinazione) {
  results <- list()
  
  filtered_data <- data %>%
    filter(Variable %in% variables_of_interest) %>%
    group_by(Country, Variable) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      Trend = ifelse(sum(!is.na(Value)) > 1, lm(Value ~ YEA, data = cur_data())$coefficients[2], NA),
      .groups = 'drop'
    )
  
  pivoted_data <- filtered_data %>%
    pivot_wider(names_from = Variable, values_from = c(Mean, SD, Trend), id_cols = Country)
  
  pivoted_data_matrix <- pivoted_data[, -1]
  rownames(pivoted_data_matrix) <- pivoted_data$Country
  
  for (metric in distance_metrics) {
    for (method in linkage_methods) {
      for (k in cluster_range) {
        dist_matrix <- dist(pivoted_data_matrix, method = metric)
        
        if (method %in% c("centroid", "median")) {
          dist_matrix <- dist_matrix ^ 2
        }
        
        hc <- hclust(dist_matrix, method = method)
        cluster_assignments <- cutree(hc, k)
        
        silhouette_score <- silhouette(cluster_assignments, dist_matrix)
        avg_silhouette <- mean(silhouette_score[, 3])
        
        results[[paste(metric, method, k, sep = "_")]] <- list(
          clusters = cluster_assignments,
          silhouette = avg_silhouette,
          dendrogram = hc
        )
        
        pdf(file.path(directory_destinazione, paste0("dendrogram_", metric, "_", method, "_", k, ".pdf")))
        plot(hc, hang = -1, xlab = paste("Metrica:", metric, "Linkage:", method, "Cluster:", k))
        rect.hclust(hc, k = k, border = "red")
        dev.off()
        
        png(file.path(directory_destinazione, paste0("silhouette_", metric, "_", method, "_", k, ".png")))
        plot(silhouette_score, border = NA, col = as.numeric(cluster_assignments) + 1,
             main = paste("Silhouette plot - Metrica:", metric, "Linkage:", method, "Cluster:", k, "Avg Silhouette:", round(avg_silhouette, 2)))
        dev.off()
      }
    }
  }
  
  return(results)
}

variables_of_interest <- c("Renewable energy supply, % total energy supply", "Environmentally related taxes, % GDP", "Terrestrial protected area, % land area")
cluster_range <- 2:5
distance_metrics <- c("euclidean")
linkage_methods <- c("single", "complete", "average", "centroid", "median")
directory_destinazione <- "Cluster_Sensitivity_Analysis"

if (!file.exists(directory_destinazione)) {
  dir.create(directory_destinazione)
}


# Esegui l'analisi di sensibilità
sensitivity_results <- perform_sensitivity_analysis(dataset, variables_of_interest, cluster_range, distance_metrics, linkage_methods, directory_destinazione)

# Esplora i risultati dell'analisi di sensibilità
silhouette_scores <- sapply(sensitivity_results, function(x) x$silhouette)

# Specifica il percorso e il nome del file di output nella directory di destinazione
output_file <- file.path(directory_destinazione, "confronto_silhouette_scores.pdf")

# Trasforma i silhouette scores in un data frame
silhouette_df <- data.frame(
  Combination = names(silhouette_scores),
  SilhouetteScore = silhouette_scores
)
p <- ggplot(silhouette_df, aes(x = reorder(Combination, -SilhouetteScore), y = SilhouetteScore)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +  # Aggiunge un bordo nero alle barre
  labs(title = "Confronto dei Silhouette Score", x = "Combinazione", y = "Silhouette Score") +
  theme_minimal(base_size = 14) +  # Cambia il tema per uno più pulito
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),  # Ottimizza le etichette dell'asse X
        axis.title.x = element_text(margin = margin(t = 20)),  # Aggiunge margine superiore al titolo dell'asse X\
        plot.title = element_text(hjust = 0.5))  # Centra il titolo


# Specifica il percorso e il nome del file di output nella directory di destinazione
output_file <- file.path(directory_destinazione, "confronto_silhouette_scores_ggplot.pdf")

# Salva il grafico in formato A4 orizzontale
ggsave(output_file, plot = p, width = 29.7, height = 21, units = "cm")

