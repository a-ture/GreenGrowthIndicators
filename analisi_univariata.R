library(ggplot2)
library(dplyr)

# Funzione per creare un nome di file sicuro
crea_nome_file_sicuro <- function(nome_variabile) {
  nome_sicuro <- gsub("%", "percent", nome_variabile)
  nome_sicuro <- gsub("[[:space:]]", "_", nome_sicuro)
  nome_sicuro <- gsub("[^[:alnum:]_]", "", nome_sicuro)
  return(nome_sicuro)
}

# Funzione per creare e salvare un grafico
crea_e_salva_grafico <- function(grafico, nome_file, directory_destinazione) {
  file_path <- file.path(directory_destinazione, nome_file)
  ggsave(file_path, grafico, device = "pdf", width = 29.7, height = 21, units = "cm")
}

# Funzione per creare un Q-Q plot
crea_qq_plot <- function(dataset, titolo) {
  ggplot(dataset, aes(sample = Value)) +
    stat_qq(color = "#2C7BB6") +  # Blu
    stat_qq_line(color = "#31A354") +  # Verde
    ggtitle(titolo) +
    labs(x = "Quantili Teorici", y = "Quantili Campione") +
    theme_linedraw(base_size = 14) +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5, margin = margin(b = 25, t = 15)),
      axis.text = element_text(size = 8),  # Dimensione del testo degli assi ridotta
      axis.title = element_text(size = 10)  # Dimensione delle etichette degli assi ridotta
    )
}

# Funzione per creare un istogramma
crea_istogramma <- function(dataset, titolo, x_lab) {
  ggplot(dataset, aes(x = Value)) +
    geom_histogram(binwidth = 10, fill = "#A8CF97", color = "black", alpha = 0.7) +
    ggtitle(titolo) +
    labs(x = x_lab, y = "Frequenza") +
    theme_linedraw(base_size = 14) +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5, margin = margin(b = 25, t = 15)),
      axis.text = element_text(size = 8),  # Dimensione del testo degli assi ridotta
      axis.title = element_text(size = 10)  # Dimensione delle etichette degli assi ridotta
    )
}

# Funzione per creare la funzione di distribuzione empirica
crea_ecdf <- function(dataset, titolo, x_lab) {
  ggplot(dataset, aes(x = Value)) +
    stat_ecdf(geom = "step", color = "#A8CF97", size = 1) +
    ggtitle(titolo) +
    labs(x = x_lab, y = "Probabilità Cumulativa") +
    theme_linedraw(base_size = 14) +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5, margin = margin(b = 25, t = 15)),
      axis.text = element_text(size = 8),  # Dimensione del testo degli assi ridotta
      axis.title = element_text(size = 10)  # Dimensione delle etichette degli assi ridotta
    )
}

# Funzione per creare la densità
crea_densita <- function(dataset, titolo, x_lab) {
  ggplot(dataset, aes(x = Value)) +
    geom_density(fill = "#A8CF97", alpha = 0.7) +
    ggtitle(titolo) +
    labs(x = x_lab, y = "Densità") +
    theme_linedraw(base_size = 14) +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5, margin = margin(b = 25, t = 15)),
      axis.text = element_text(size = 8),  # Dimensione del testo degli assi ridotta
      axis.title = element_text(size = 10)  # Dimensione delle etichette degli assi ridotta
    )
}

# Funzione per creare un boxplot con riempimento verde e senza leggenda
crea_boxplot <- function(dataset, titolo, x_lab) {
  ggplot(dataset, aes(x = Variable, y = Value)) +
    geom_boxplot(
      notch = TRUE,
      notchwidth = 0.8,
      outlier.size = 3,
      outlier.color = "#00aedb",
      fill = "#A8CF97",  # Riempimento verde
      color = "black",  # Bordo del boxplot
      show.legend = FALSE
    ) +
    ggtitle(titolo) +
    labs(x = x_lab, y = "Valore") +
    theme_linedraw(base_size = 14) +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5, margin = margin(b = 25, t = 15)),
      axis.text = element_text(size = 8),  # Dimensione del testo degli assi ridotta
      axis.title = element_text(size = 10)  # Dimensione delle etichette degli assi ridotta
    )
}

# Funzione per creare grafici per ogni continente con titolo più piccolo
crea_grafico_per_continente <- function(dataset, titolo, x_lab, directory_destinazione, mappatura_continenti) {
  for (cont in names(mappatura_continenti)) {
    dati_continente <- dataset %>%
      filter(Country %in% mappatura_continenti[[cont]]) %>%
      arrange(YEA)
    
    if (nrow(dati_continente) > 0) {
      nome_file <- paste0("grafico_", crea_nome_file_sicuro(cont), ".pdf")
      grafico <- ggplot(dati_continente, aes(x = YEA, y = Value, group = Country, color = Country)) +
        geom_line(alpha = 0.8, size = 1) +
        geom_point(size = 2, shape = 16, fill = "white", show.legend = FALSE) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +
        scale_y_log10() +
        ggtitle(paste(titolo, "-", cont)) +
        labs(x = "Anno", y = x_lab) +
        theme_linedraw(base_size = 14) +
        theme(
          plot.title = element_text(size = 10, hjust = 0.5, margin = margin(b = 25, t = 15)),  # Dimensione del titolo ridotta
          axis.text = element_text(size = 8),  # Dimensione del testo degli assi ridotta
          axis.title = element_text(size = 10)  # Dimensione delle etichette degli assi ridotta
        )
      
      crea_e_salva_grafico(grafico, nome_file, directory_destinazione)
    }
  }
}

# Creare la cartella principale "AnalisiUnivariata"
cartella_principale <- "AnalisiUnivariata"
if (!file.exists(cartella_principale)) {
  dir.create(cartella_principale)
}

# Variabili di interesse
variabili_interesse <- c(
  "Production-based CO2 emissions",
  "Demand-based CO2 emissions",
  "Renewable energy supply, % total energy supply",
  "Renewable electricity, % total electricity generation",
  "Population with access to improved drinking water sources, % total population",
  "Water stress, total freshwater abstraction as % total available renewable resources",
  "Mortality from exposure to ambient PM2.5",
  "Percentage of population exposed to more than 10 micrograms/m3",
  "Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent",
  "Environmentally related taxes, % GDP",
  "Real GDP per capita",
  "Terrestrial protected area, % land area",
  "Marine protected area, % total exclusive economic zone"
)

# Loop attraverso ciascuna variabile
for (variabile in variabili_interesse) {
  
  # Genera un nome di directory sicuro
  nome_cartella_variabile <- crea_nome_file_sicuro(variabile)
  
  # Percorso completo della cartella variabile all'interno di "AnalisiUnivariata"
  directory_destinazione <- file.path(cartella_principale, nome_cartella_variabile)
  
  # Crea la cartella se non esiste
  if (!file.exists(directory_destinazione)) {
    dir.create(directory_destinazione, recursive = TRUE)
  }
  
  # Filtra il dataset per la variabile corrente
  dataset_var <- filter(dataset, Variable == variabile)
  
  # Creazione e salvataggio di grafici
  crea_e_salva_grafico(
    crea_istogramma(dataset_var, 
                    paste("Istogramma -", variabile),
                    "Valore"),
    paste0("istogramma_", crea_nome_file_sicuro(variabile), ".pdf"),
    directory_destinazione
  )
  
  crea_e_salva_grafico(
    crea_ecdf(dataset_var, 
              paste("Funzione di Distribuzione Empirica -", variabile),
              "Valore"),
    paste0("ecdf_", crea_nome_file_sicuro(variabile), ".pdf"),
    directory_destinazione
  )
  
  crea_e_salva_grafico(
    crea_densita(dataset_var, 
                 paste("Funzione di Densità di Probabilità -", variabile),
                 "Valore"),
    paste0("densita_", crea_nome_file_sicuro(variabile), ".pdf"),
    directory_destinazione
  )
  
  crea_e_salva_grafico(
    crea_boxplot(dataset_var, 
                 paste("Boxplot per la variabile:", variabile),
                 "Valore"),
    paste0("boxplot_", crea_nome_file_sicuro(variabile), ".pdf"),
    directory_destinazione
  )
  
  # Creare e salvare il Q-Q plot
  crea_e_salva_grafico(
    crea_qq_plot(dataset_var, 
                 paste("Q-Q Plot per la variabile:", variabile)),
    paste0("qqplot_", crea_nome_file_sicuro(variabile), ".pdf"),
    directory_destinazione
  )
  
  # Crea grafici per ciascun continente
  crea_grafico_per_continente(
    dataset_var,
    paste(variabile, "per continente"),
    "Valore",
    directory_destinazione,
    mappatura_continenti
  )
}
