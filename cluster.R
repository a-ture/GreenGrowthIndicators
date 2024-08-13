library(dplyr)
library(tidyr)
library(cluster)
library(ggplot2)
library(ggdendro)
library(scatterplot3d)  # Carica il pacchetto scatterplot3d

# Imposta la directory di destinazione per il heatmap
directory_destinazione <- "Cluster"
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')

# Assicurati che la sottodirectory esista, altrimenti creala
if (!file.exists(directory_destinazione)) {
  dir.create(directory_destinazione)
}

perform_kmeans_clustering <- function(data, variables_of_interest, variables_of_interest_t, num_clusters, iter_max = 100, nstart = 8, directory_destinazione) {
  
  # Pre-elaborazione dei dati con statistiche temporali per paese
  processed_data <- data %>%
    filter(Variable %in% variables_of_interest, !is.na(Value)) %>%
    group_by(Country, Variable) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      Trend = ifelse(sum(!is.na(Value)) > 1, lm(Value ~ YEA, data = cur_data())$coefficients[2], NA),
      .groups = 'drop'
    )
  
  # Trasposizione del dataset
  transposed_data <- processed_data %>%
    pivot_wider(names_from = Variable,
                values_from = c(Mean, SD, Trend),
                id_cols = Country)
  print("transposed_data")
  print(transposed_data)
  
  # Esecuzione del K-Means
  set.seed(123)  # Imposta un seed per la riproducibilità
  kmeans_result <-
    kmeans(
      transposed_data[, -1],
      centers = num_clusters,
      iter.max = iter_max,  # Numero massimo di iterazioni
      nstart = nstart  # Numero di inizializzazioni casuali
    )
  
  # Aggiungi il risultato del clustering ai dati
  transposed_data$Cluster <- as.factor(kmeans_result$cluster)
  
  # Genera i nuovi nomi delle colonne
  new_column_names <- unlist(lapply(variables_of_interest, function(var) {
    c(paste0("Mean_", var), paste0("SD_", var), paste0("Trend_", var))
  }))
  
  # Creare un dataframe con i risultati del K-Means
  result_columns <- c(new_column_names, "Cluster")
  results <- transposed_data[, c(result_columns)]
  
  # Creazione del PDF per il plot
  pdf(
    file.path(directory_destinazione, paste0("cluster_", num_clusters, ".pdf")),
    width = 12,
    height = 8.3
  )
  
  # Creare uno scatterplot 3D
  # Mappa di colori per i paesi
  unique_countries <- unique(transposed_data$Country)
  country_color_map <- setNames(rainbow(length(unique_countries)), unique_countries)
  
  # Mappa di simboli per i cluster
  unique_clusters <- sort(unique(transposed_data$Cluster))
  cluster_pch_map <- setNames(1:length(unique_clusters), unique_clusters)
  
  # Creare lo scatterplot 3D con i dati effettivi
  s3d <- scatterplot3d(
    results[[paste0("Mean_", variables_of_interest[1])]],
    results[[paste0("Mean_", variables_of_interest[2])]],
    results[[paste0("Mean_", variables_of_interest[3])]],
    color = "grey",  # Colore temporaneo per tutti i punti
    pch = 19,  # Simbolo temporaneo per tutti i punti
    main = "K-Means Clustering in 3D",
    grid = TRUE,
    col.grid = "grey",
    lty.grid = par("lty"),
    box = FALSE,
    xlab = deparse(variables_of_interest_t[1]),
    ylab = deparse(variables_of_interest_t[2]),
    zlab = deparse(variables_of_interest_t[3])
  )
  
  # Mappa di colori per i paesi e simboli per i cluster (definiti in precedenza)
  
  # Sovrascrivere i punti con colori e simboli specifici
  for (i in seq_len(nrow(results))) {
    s3d.coords <- s3d$xyz.convert(
      results[[paste0("Mean_", variables_of_interest[1])]][i],
      results[[paste0("Mean_", variables_of_interest[2])]][i],
      results[[paste0("Mean_", variables_of_interest[3])]][i]
    )
    points(
      s3d.coords$x, s3d.coords$y, s3d.coords$z,
      col = country_color_map[transposed_data$Country[i]],
      pch = cluster_pch_map[transposed_data$Cluster[i]]
    )
  }
  
  
  addgrids3d( results[[paste0("Mean_", variables_of_interest[1])]],
              results[[paste0("Mean_", variables_of_interest[2])]],
              results[[paste0("Mean_", variables_of_interest[3])]], grid = c("xy", "xz", "yz"))
  # Aggiungi una leggenda
  # Aggiungi la leggenda per i colori (Paesi)
  legend("topright", 
         inset = .05, 
         title = "Paesi", 
         legend = names(country_color_map), 
         col = country_color_map, 
         pch = 19,
         cex = 0.6)
  
  legend("bottomright", 
         inset = .05, 
         title = "Cluster", 
         legend = unique_clusters, 
         pch = cluster_pch_map,
         cex = 0.6)
  dev.off()
  
  print(kmeans_result)
  
  # Calcola la media per ciascuna variabile all'interno di ciascun cluster
  cluster_means <- aggregate(transposed_data[, -1], by = list(Cluster = transposed_data$Cluster), mean)
  
  # Visualizza le medie dei cluster
  print(cluster_means)
}

# Imposta la directory di destinazione per gli scatter plots
sotto_cartella <- "Ob1_K-means"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)

if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

variables_of_interest_ob1 <-
  c(
    "Renewable energy supply, % total energy supply",
    "Environmentally related taxes, % GDP",
    "Terrestrial protected area, % land area"
  )


# Nomi delle variabili di interesse in italiano
variables_of_interest_ob1_t <- c(
  "Fornitura di energia rinnovabile",
  "Tasse legate all'ambiente",
  "Area terrestre protetta"
)
# Utilizza la funzione per eseguire il clustering con parametri personalizzati
num_clusters <- 3  # Numero di cluster desiderato
iter_max <- 100    # Numero massimo di iterazioni
nstart <- 8        # Numero di inizializzazioni casuali

result <- perform_kmeans_clustering(dataset, variables_of_interest_ob1,variables_of_interest_ob1_t, num_clusters, iter_max, nstart,percorso_sotto_cartella)

# Utilizza la funzione per eseguire il clustering con parametri personalizzati
num_clusters <- 4  # Numero di cluster desiderato
iter_max <- 100    # Numero massimo di iterazioni
nstart <- 8        # Numero di inizializzazioni casuali

result <- perform_kmeans_clustering(dataset, variables_of_interest_ob1,variables_of_interest_ob1_t, num_clusters, iter_max, nstart,percorso_sotto_cartella)


# Utilizza la funzione per eseguire il clustering con parametri personalizzati
num_clusters <- 2  # Numero di cluster desiderato
iter_max <- 100    # Numero massimo di iterazioni
nstart <- 8        # Numero di inizializzazioni casuali

result <- perform_kmeans_clustering(dataset, variables_of_interest_ob1,variables_of_interest_ob1_t, num_clusters, iter_max, nstart,percorso_sotto_cartella)

#Metodi gerarchi 
perform_hierarchical_clustering <-
  function(data,
           variables_of_interest,
           variables_of_interest_t,
           methods,
           directory_destinazione,k) {
    # Filtra i dati per le variabili di interesse
    filtered_data <- dataset %>%
      filter(Variable %in% variables_of_interest)
    
    # Gestisci eventuali duplicati
    filtered_data <- filtered_data %>%
      group_by(Country, Variable) %>%
      summarise(
        Mean = mean(Value, na.rm = TRUE),
        SD = sd(Value, na.rm = TRUE),
        Trend = ifelse(sum(!is.na(Value)) > 1, lm(Value ~ YEA, data = cur_data())$coefficients[2], NA),
        .groups = 'drop'
      )
    
    # Usa la funzione pivot_wider
    pivoted_data <- filtered_data %>%
      pivot_wider(names_from = Variable,
                  values_from = c(Mean, SD, Trend),
                  id_cols = Country)
    
    # Rinomina le colonne per evitare nomi che iniziano con numeri e rimuovere spazi
    colnames(pivoted_data) <- gsub(" ", "_", gsub("^(\\d+)", "X\\1_", colnames(pivoted_data)))
    
    
    # Rimuovi la colonna Country
    pivoted_data_matrix <- pivoted_data[,-1]
    rownames(pivoted_data_matrix) <- pivoted_data$Country
    print(pivoted_data)
    results <- list()  # Inizializza una lista per i risultati
 
    for (method in methods) {
      # Esegui il clustering gerarchico
      dist_matrix <- dist(pivoted_data_matrix, method = "euclidean")
      # Per i metodi centroid e median, eleva al quadrato la matrice delle distanze
      
      if (method %in% c("centroid", "median")) {
        dist_matrix <- dist_matrix ^ 2
      }
      
      hc <-
        hclust(dist_matrix, method = method)
      
      # Crea il dendrogramma
      dendrogram_filename <- paste0("dendrogram_", method, ".pdf")
      pdf(
        file.path(directory_destinazione, dendrogram_filename),
        width = 12,
        height = 8.3
      )
      par(mar = c(5, 4, 4, 2) + 0.1)  # Modifica i margin
      plot(hc,
           hang = -1,
           xlab = "Metodo gerarchico agglomerativo",
           sub = method)
      axis(side = 4, at = round(hc$height, 2))
      rect.hclust(hc, k = 4, border = "red")
      rect.hclust(hc, k = 2, border = "blue")
      dev.off()
      
      # Salva il dendrogramma
      results[[method]] <- dendrogram_filename
      
      # Usa cutree per assegnare i cluster
      cluster_assignments <- cutree(hc, k)
      # Salva le assegnazioni dei cluster nei risultati
      results[[paste0("clusters_", method)]] <- cluster_assignments
      
      # Assegnazioni dei cluster con cutree
      cluster_assignments <- cutree(hc, k)
      # Aggiungi le assegnazioni dei cluster ai dati

      pivoted_data_clustered <- cbind(pivoted_data, Cluster = cluster_assignments)
      
      # Assign clusters and aggregate results
        cluster_assignments <- cutree(hc, k)
        pivoted_data_clustered <- cbind(pivoted_data_matrix, Cluster = as.factor(cluster_assignments))
        aggregate_results <- aggregate(. ~ Cluster, data = pivoted_data_clustered, FUN = function(x) c(mean = mean(x, na.rm = TRUE), var = var(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
        results[[paste0("aggregate_", method)]] <- aggregate_results

        # Calculate non-homogeneity measures
        trT <- (nrow(pivoted_data_matrix) - 1) * sum(apply(pivoted_data_matrix, 2, var, na.rm = TRUE))
        agvar <- aggregate(pivoted_data_matrix, list(Cluster = cluster_assignments), var)
        trS <- sum((agvar$Cluster - 1) * rowSums(agvar[, -1], na.rm = TRUE))
        trB <- trT - trS

        results[[paste0("non_omogeneity_within_", method)]] <- trS
        results[[paste0("non_omogeneity_between_", method)]] <- trB

        # Print or save non-homogeneity measures
        print(paste("Non-omogeneity within clusters for method", method, ":", trS))
        print(paste("Non-omogeneity between clusters for method", method, ":", trB))
      
      # Calcola le altezze di aggregazione dal risultato del clustering gerarchico
      heights <- hc$height
      
      # Creazione di un dataframe per lo scree plot
      scree_plot_df <-
        data.frame(NumeroCluster = seq_along(heights),
                   DistanzaAggregazione = rev(heights))
      
      # Creazione dello scree plot utilizzando ggplot2
      scree_plot_filename <- paste0("scree_plot_", method, ".pdf")
      g <-
        ggplot(scree_plot_df,
               aes(x = DistanzaAggregazione , y = NumeroCluster)) +
        geom_line() +  # Linea che collega i punti
        geom_point(
          shape = 21,
          color = "black",
          fill = "#69b3a2",
          size = 3
        ) +  # Punti
        theme_linedraw(base_size = 14) +
        scale_y_continuous(breaks = seq(
          from = 1,
          to = max(scree_plot_df$NumeroCluster),
          by = 4
        )) +
        labs(
          y = "Numero di cluster",
          x = "Distanza di aggregazione",
          title = paste0("Screeplot per il metodo  ", method)
        ) + theme(
          legend.position = "right",
          legend.key.size = unit(0.4, "cm"),
          # Imposta l'angolo e l'allineamento del testo sull'asse x
          axis.title.x = element_text(margin = margin(t = 30)),
          # Imposta il margine per l'etichetta dell'asse x
          axis.title.y = element_text(margin = margin(r = 30)),
          # Imposta il margine per l'etichetta dell'asse y
          plot.title = element_text(hjust = 0.5,
                                    margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
        )
      
      # Salvataggio dello scree plot come file PDF
      ggsave(
        scree_plot_filename,
        plot = g,
        path = directory_destinazione,
        width = 8,
        height = 6
      )
      
      results[["scree_plot_"]] <-
        scree_plot_filename  # Aggiungi lo scree plot ai risultati
     
       # Stampare l'assegnamento degli individui ai cluster sulla console
      cat("Cluster Assignment (Method:", method, "):\n")
      print(cluster_assignments)
      cat("\n")
    }
    return(results)  # Restituisci la lista dei risultati
  }

# Imposta la directory di destinazione per gli scatter plots
sotto_cartella <- "Ob1"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)

if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

# Filtra per le variabili di interesse
variables_of_interest_ob1 <-
  c(
    "Production-based CO2 emissions",
    "Demand-based CO2 emissions",
    
    # Energia
    "Renewable energy supply, % total energy supply",
    "Renewable electricity, % total electricity generation",
    
    #Acqua
    "Population with access to improved drinking water sources, % total population",
    "Water stress, total freshwater abstraction as % total available renewable resources",
    
    # Salute
    "Mortality from exposure to ambient PM2.5",
    "Percentage of population exposed to more than 10 micrograms/m3",
    "Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent",
    
    # Economia
    "Environmentally related taxes, % GDP",
    "Real GDP per capita",
    
    # Risorse Naturali
    "Terrestrial protected area, % land area",
    "Marine protected area, % total exclusive economic zone"
  )

# Esempio di utilizzo
methods <- c("single", "complete", "average", "median", "centroid")

results <-
  perform_hierarchical_clustering(dataset,
                                  variables_of_interest_ob1,
                                  variables_of_interest_ob1_t,
                                  methods,
                                  percorso_sotto_cartella,2)