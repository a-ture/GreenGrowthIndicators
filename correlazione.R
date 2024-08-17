library(tidyr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(GGally)

# Funzione per creare directory se non esiste
crea_directory <- function(directory) {
  if (!file.exists(directory)) {
    dir.create(directory)
  }
}

# Funzione per salvare grafici
salva_grafico <- function(grafico, percorso, nome_file) {
  ggsave(
    file.path(percorso, nome_file),
    plot = grafico,
    width = 29.7,
    height = 21,
    units = "cm"
  )
}

# Funzione per salvare riassunto di un modello in un file
salva_modello <- function(modello, percorso, nome_file) {
  output_riassunto <- capture.output(summary(modello))
  write(output_riassunto, file = file.path(percorso, nome_file))
}

# Funzione per verificare i livelli di un fattore e assicurarsi che abbia almeno due livelli
verifica_fattori <- function(data, col_name) {
  if (is.factor(data[[col_name]]) && length(levels(data[[col_name]])) < 2) {
    stop(paste("La variabile", col_name, "ha meno di due livelli. Non può essere usata nel modello."))
  }
}

# Funzione per preparare i dati per il modello
prepara_dati_per_modello <- function(data, formula) {
  # Converti eventuali variabili categoriali in fattori
  for (var in all.vars(formula)) {
    if (is.character(data[[var]])) {
      data[[var]] <- as.factor(data[[var]])
    }
  }
  
  # Verifica se i fattori hanno almeno due livelli
  for (var in all.vars(formula)) {
    verifica_fattori(data, var)
  }
  
  # Rimuovi eventuali righe con NA
  data <- data[complete.cases(data[all.vars(formula)]), ]
  
  return(data)
}

# Imposta la directory di destinazione per il heatmap
directory_destinazione <- "Correlazione"
crea_directory(directory_destinazione)

# Seleziona solo le colonne necessarie
selected_columns <- c("Variable", "Country", "YEA", "Value")

# Filtra solo le righe con valori completi nelle colonne selezionate
complete_data <- dataset %>%
  filter(!is.na(Value)) %>%
  select(all_of(selected_columns))

# Trasponi il dataset per avere le variabili come colonne
transposed_data <- complete_data %>%
  pivot_wider(names_from = Variable, values_from = Value)

# Calcola la correlazione tra le variabili
correlation_matrix <- cor(transposed_data[,-c(1:2)], use = "pairwise.complete.obs")

# Salva la matrice di correlazione in un file CSV
write.csv(correlation_matrix, file = "matrice_di_correlazione.csv")

# Trasforma la matrice di correlazione in un formato adatto a ggplot
cor_df <- reshape2::melt(correlation_matrix)

# Creare un grafico a matrice di correlazione
heatmap <- ggplot(data = cor_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    limit = c(-1, 1),
    space = "Lab"
  ) +
  labs(title = "Heatmap della Matrice di Correlazione") +
  theme_linedraw(base_size = 10) +  # Imposta una base più piccola per il testo
  theme(
    legend.position = "right",
    legend.key.size = unit(0.3, "cm"),  # Riduci la dimensione della legenda
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Riduci la dimensione del testo sull'asse x
    axis.text.y = element_text(size = 8),  # Riduci la dimensione del testo sull'asse y
    axis.title.x = element_text(margin = margin(t = 20), size = 10),  # Riduci la dimensione del titolo dell'asse x
    axis.title.y = element_text(margin = margin(r = 20), size = 10),  # Riduci la dimensione del titolo dell'asse y
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 15, t = 10), size = 12)  # Riduci la dimensione del titolo del grafico
  )

# Salva il grafico heatmap
salva_grafico(heatmap, directory_destinazione, "heatmap.pdf")

# Funzione per creare scatter plot con linea di regressione e salvare
crea_scatter_plot <- function(data, x, y, titolo, x_label, y_label, percorso, nome_file, formula = y ~ x) {
  scatter_plot <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), color = Country)) +
    geom_point() +
    geom_smooth(method = "lm", formula = formula, color = "red", fill = "#69b3a2", se = TRUE) +
    labs(title = titolo, x = x_label, y = y_label, color = "Paesi") +
    theme_bw(base_size = 14) +
    theme(
      legend.position = "right",
      legend.key.size = unit(0.4, "cm"),
      axis.title.x = element_text(margin = margin(t = 30)),
      axis.title.y = element_text(margin = margin(r = 30)),
      plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
    )
  salva_grafico(scatter_plot, percorso, nome_file)
}

# Crea sotto-cartella per scatter plots
sotto_cartella_scatter <- file.path(directory_destinazione, "Scatterplots")
crea_directory(sotto_cartella_scatter)

# Crea tutti gli scatter plot e salva, usando modelli non lineari dove appropriato
crea_scatter_plot(transposed_data, 
                  "Production-based CO2 emissions", 
                  "Demand-based CO2 emissions", 
                  "Emissioni basate sulla produzione vs. Emissioni basate sulla domanda", 
                  "Emissioni di CO2 basate sulla produzione", 
                  "Emissioni di CO2 basate sulla domanda",
                  sotto_cartella_scatter, 
                  "scatter_production_vs_demand.pdf")  # Modello lineare

# Modello lineare per scatter_production_vs_demand
modello_production_vs_demand <- lm(`Demand-based CO2 emissions` ~ `Production-based CO2 emissions`, 
                                   data = prepara_dati_per_modello(transposed_data, `Demand-based CO2 emissions` ~ `Production-based CO2 emissions`))
salva_modello(modello_production_vs_demand, sotto_cartella_scatter, "modello_production_vs_demand.txt")

crea_scatter_plot(transposed_data, 
                  "Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent", 
                  "Percentage of population exposed to more than 10 micrograms/m3", 
                  "Costi del Benessere Mortalità PM2.5 vs. Percentuale Popolazione Esposta", 
                  "Costi del Benessere per Mortalità PM2.5 (Equivalente PIL)", 
                  "Percentuale della Popolazione Esposta a oltre 10 microgrammi/m3", 
                  sotto_cartella_scatter, 
                  "scatter_costi_benessere_vs_percentuale_esposizione.pdf",
                  formula = y ~ poly(x, 2))  # Modello polinomiale

# Modello polinomiale per scatter_costi_benessere_vs_percentuale_esposizione
modello_costi_benessere_vs_percentuale_esposizione <- lm(`Percentage of population exposed to more than 10 micrograms/m3` ~ poly(`Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent`, 2), 
                                                         data = prepara_dati_per_modello(transposed_data, `Percentage of population exposed to more than 10 micrograms/m3` ~ poly(`Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent`, 2)))
salva_modello(modello_costi_benessere_vs_percentuale_esposizione, sotto_cartella_scatter, "modello_costi_benessere_vs_percentuale_esposizione.txt")

crea_scatter_plot(transposed_data, 
                  "Demand-based CO2 emissions", 
                  "Environmentally related taxes, % GDP", 
                  "Emissioni di CO2 Basate sulla Domanda vs. Tasse Correlate all’Ambiente", 
                  "Emissioni di CO2 basate sulla domanda", 
                  "Tasse Correlate all’Ambiente (% del PIL)", 
                  sotto_cartella_scatter, 
                  "scatter_demand_vs_taxes.pdf",
                  formula = y ~ poly(x, 2))  # Modello polinomiale

# Modello polinomiale per scatter_demand_vs_taxes
modello_demand_vs_taxes <- lm(`Environmentally related taxes, % GDP` ~ poly(`Demand-based CO2 emissions`, 2), 
                              data = prepara_dati_per_modello(transposed_data, `Environmentally related taxes, % GDP` ~ poly(`Demand-based CO2 emissions`, 2)))
salva_modello(modello_demand_vs_taxes, sotto_cartella_scatter, "modello_demand_vs_taxes.txt")

crea_scatter_plot(transposed_data, 
                  "Mortality from exposure to ambient PM2.5", 
                  "Real GDP per capita", 
                  "Mortalità da Esposizione a PM2.5 Ambientali vs. PIL reale pro capite", 
                  "Mortalità da Esposizione a PM2.5 Ambientali", 
                  "PIL reale pro capite", 
                  sotto_cartella_scatter, 
                  "scatter_mortality_vs_real.pdf",
                  formula = y ~ log(x))  # Modello logaritmico

# Modello logaritmico per scatter_mortality_vs_real
modello_mortality_vs_real <- lm(`Mortality from exposure to ambient PM2.5` ~ log(`Real GDP per capita`), 
                                data = prepara_dati_per_modello(transposed_data, `Mortality from exposure to ambient PM2.5` ~ log(`Real GDP per capita`)))
salva_modello(modello_mortality_vs_real, sotto_cartella_scatter, "modello_mortality_vs_real.txt")

crea_scatter_plot(transposed_data, 
                  "Population with access to improved drinking water sources, % total population", 
                  "Percentage of population exposed to more than 10 micrograms/m3", 
                  "Popolazione con accesso a acqua potabile vs. Percentuale di Popolazione Esposta", 
                  "Popolazione con accesso a acqua potabile migliorata", 
                  "Percentuale di Popolazione Esposta a Più di 10 µg/m³", 
                  sotto_cartella_scatter, 
                  "scatter_population_vs_percent.pdf",
                  formula = y ~ poly(x, 2))  # Modello polinomiale


