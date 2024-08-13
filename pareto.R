library(ggplot2)
library(dplyr)

# Imposta la directory di destinazione
directory_destinazione <- "Pareto"

# Assicurati che la sottodirectory esista, altrimenti creala
if (!file.exists(directory_destinazione)) {
  dir.create(directory_destinazione)
}

# Funzione per creare il diagramma di Pareto
crea_pareto_plot <- function(dataset, variabile, titolo, file_name, y_label = "Emissioni Totali") {
  
  # Preparazione dei dati per il Diagramma di Pareto
  dataset_pareto <- dataset %>%
    filter(Variable == variabile) %>%
    group_by(Country) %>%
    summarise(TotalEmissions = sum(Value)) %>%
    arrange(TotalEmissions) %>%
    mutate(CumPercent = rev(cumsum(rev(TotalEmissions)) / sum(TotalEmissions) * 100))
  
  # Verifica della correttezza dei dati
  if (any(is.na(dataset_pareto$TotalEmissions)) || any(is.na(dataset_pareto$CumPercent))) {
    stop("Il dataset contiene valori mancanti o non validi per la variabile selezionata.")
  }
  
  # Calcola il valore massimo delle emissioni totali per l'asse Y secondario
  max_emissions <- max(dataset_pareto$TotalEmissions)
  
  # Creazione del Diagramma di Pareto
  pareto <- ggplot(dataset_pareto, aes(x = reorder(Country, -TotalEmissions), y = TotalEmissions)) +
    geom_bar(stat = "identity", fill = "#A8CF97") +
    geom_line(aes(y = CumPercent * max_emissions / 100, group = 1), color = "black") +
    geom_point(aes(y = CumPercent * max_emissions / 100), color = "black", size = 1) +
    geom_text(aes(y = CumPercent * max_emissions / 100, label = paste0(round(CumPercent, 2), "%")), 
              hjust = -0.2, vjust = -0.5, color = "red", size = 2) +
    scale_y_continuous(sec.axis = sec_axis(~ . * 100 / max_emissions, name = "Percentuale cumulativa (%)")) +
    theme_linedraw(base_size = 14) +
    labs(title = titolo, x = "Paesi", y = y_label) +
    theme(
      legend.position = "right",
      legend.key.size = unit(0.5, "cm"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_text(margin = margin(t = 30)),
      axis.title.y = element_text(margin = margin(r = 30)),
      plot.title = element_text(
        face = 'bold',
        hjust = 0.5,
        margin = margin(b = 25, t = 15)
      )
    )
  
  # Salva il diagramma di Pareto in un file PDF
  ggsave(
    file_name,
    pareto,
    path = directory_destinazione,
    device = "pdf",
    width = 29.7,
    height = 21,
    units = "cm"
  )
}

# Lista delle variabili, titoli, nomi dei file e etichette y
variabili <- c("Production-based CO2 emissions", "Demand-based CO2 emissions", 
               "Renewable energy supply, % total energy supply", 
               "Renewable electricity, % total electricity generation", 
               "Water stress, total freshwater abstraction as % total available renewable resources")

titoli <- c("Diagramma di Pareto delle Emissioni basate sulla produzione",
            "Diagramma di Pareto delle Emissioni basate sulla domanda",
            "Diagramma di Pareto della Fornitura di energia rinnovabile",
            "Diagramma di Pareto dell'Energia elettrica rinnovabile",
            "Diagramma di Pareto dello Stress Idrico")

file_names <- c("pareto_production_based_co2.pdf", "pareto_demand_based_co2.pdf", 
                "pareto_energy.pdf", "pareto_electricity.pdf", "pareto_water.pdf")

y_labels <- c("Emissioni Totali (kt)", "Emissioni Totali (kt)", "Fornitura Totale", 
              "Fornitura Totale", "Prelievo Totale (%)")

# Generazione dei diagrammi di Pareto
for (i in 1:length(variabili)) {
  crea_pareto_plot(dataset, variabili[i], titoli[i], file_names[i], y_labels[i])
}
