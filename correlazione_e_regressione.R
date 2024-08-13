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
crea_scatter_plot <- function(data, x, y, titolo, x_label, y_label, percorso, nome_file) {
  scatter_plot <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), color = Country)) +
    geom_point() +
    geom_smooth(color = "red", fill = "#69b3a2", se = TRUE) +
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

# Crea tutti gli scatter plot e salva
crea_scatter_plot(transposed_data, 
                  "Production-based CO2 emissions", 
                  "Demand-based CO2 emissions", 
                  "Emissioni basate sulla produzione vs. Emissioni basate sulla domanda", 
                  "Emissioni di CO2 basate sulla produzione", 
                  "Emissioni di CO2 basate sulla domanda",
                  sotto_cartella_scatter, 
                  "scatter_production_vs_demand.pdf")

crea_scatter_plot(transposed_data, 
                  "Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent", 
                  "Percentage of population exposed to more than 10 micrograms/m3", 
                  "Costi del Benessere Mortalità PM2.5 vs. Percentuale Popolazione Esposta", 
                  "Costi del Benessere per Mortalità PM2.5 (Equivalente PIL)", 
                  "Percentuale della Popolazione Esposta a oltre 10 microgrammi/m3", 
                  sotto_cartella_scatter, 
                  "scatter_costi_benessere_vs_percentuale_esposizione.pdf")

crea_scatter_plot(transposed_data, 
                  "Demand-based CO2 emissions", 
                  "Environmentally related taxes, % GDP", 
                  "Emissioni di CO2 Basate sulla Domanda vs. Tasse Correlate all’Ambiente", 
                  "Emissioni di CO2 basate sulla domanda", 
                  "Tasse Correlate all’Ambiente (% del PIL)", 
                  sotto_cartella_scatter, 
                  "scatter_demand_vs_taxes.pdf")

crea_scatter_plot(transposed_data, 
                  "Mortality from exposure to ambient PM2.5", 
                  "Real GDP per capita", 
                  "Mortalità da Esposizione a PM2.5 Ambientali vs. PIL reale pro capite", 
                  "Mortalità da Esposizione a PM2.5 Ambientali", 
                  "PIL reale pro capite", 
                  sotto_cartella_scatter, 
                  "scatter_mortality_vs_real.pdf")

crea_scatter_plot(transposed_data, 
                  "Population with access to improved drinking water sources, % total population", 
                  "Percentage of population exposed to more than 10 micrograms/m3", 
                  "Popolazione con accesso a acqua potabile vs. Percentuale di Popolazione Esposta", 
                  "Popolazione con accesso a acqua potabile migliorata", 
                  "Percentuale di Popolazione Esposta a Più di 10 µg/m³", 
                  sotto_cartella_scatter, 
                  "scatter_population_vs_percent.pdf")

crea_scatter_plot(transposed_data, 
                  "Renewable electricity, % total electricity generation", 
                  "Percentage of population exposed to more than 10 micrograms/m3", 
                  "Energia Elettrica Rinnovabile vs. Percentuale di Popolazione Esposta a Più di 10 µg/m³", 
                  "Energia Elettrica Rinnovabile (Percentuale della Generazione Totale di Elettricità)", 
                  "Percentuale di Popolazione Esposta a Più di 10 µg/m³", 
                  sotto_cartella_scatter, 
                  "scatter_electricity_vs_percent.pdf")

crea_scatter_plot(transposed_data, 
                  "Renewable energy supply, % total energy supply", 
                  "Renewable electricity, % total electricity generation", 
                  "Elettricità rinnovabile vs. Fornitura di energia rinnovabile", 
                  "Fornitura di energia rinnovabile, % del totale dell'energia fornita", 
                  "Elettricità rinnovabile, % del totale dell'elettricità generata", 
                  sotto_cartella_scatter, 
                  "scatter_energia_vs_elettricita.pdf")

crea_scatter_plot(transposed_data, 
                  "Renewable energy supply, % total energy supply", 
                  "Water stress, total freshwater abstraction as % total available renewable resources", 
                  "Energia Rinnovabile vs. Stress Idrico", 
                  "Fornitura di energia rinnovabile, % totale", 
                  "Stress Idrico (Percentuale del totale delle risorse rinnovabili disponibili)", 
                  sotto_cartella_scatter, 
                  "scatter_energia_vs_acqua.pdf")

crea_scatter_plot(transposed_data, 
                  "Percentage of population exposed to more than 10 micrograms/m3", 
                  "Mortality from exposure to ambient PM2.5", 
                  "Percentuale della popolazione esposta vs. Mortalità dovuta all'esposizione a PM2.5", 
                  "Percentuale della popolazione esposta a più di 10 µg/m³", 
                  "Mortalità dovuta all'esposizione a PM2.5 ambientali", 
                  sotto_cartella_scatter, 
                  "scatter_esposizione_vs_mortalità.pdf")

crea_scatter_plot(transposed_data, 
                  "Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent", 
                  "Mortality from exposure to ambient PM2.5", 
                  "Mortalità dovuta all'esposizione a PM2.5 vs. Costi del benessere per le mortalità premature", 
                  "Costi del benessere per le mortalità premature dovute all'esposizione a PM2.5 ambientale", 
                  "Mortalità dovuta all'esposizione a PM2.5 ambientale", 
                  sotto_cartella_scatter, 
                  "scatter_mortalità_vs_costi.pdf")

# Imposta la directory di destinazione per la regressione multipla
sotto_cartella_regressione <- file.path(directory_destinazione, "Regressione_Multipla")
crea_directory(sotto_cartella_regressione)

# Creazione del Modello di Regressione Multipla
modello_regressione_multipla <- lm(`Mortality from exposure to ambient PM2.5` ~ 
                                     `Percentage of population exposed to more than 10 micrograms/m3` + 
                                     `Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent`, 
                                   data = transposed_data)

# Salva il riassunto del modello e gli intervalli di confidenza
output_riassunto <- capture.output(summary(modello_regressione_multipla))
write(output_riassunto, file = file.path(sotto_cartella_regressione, "Riassunto_Modello_Regressione_Multipla.txt"))

intervalli_confidenza <- capture.output(confint(modello_regressione_multipla, level = 0.95))
write(intervalli_confidenza, file = file.path(sotto_cartella_regressione, "Intervalli_Confidenza.txt"))

# Grafico dei residui standardizzati
residui_standardizzati <- rstandard(modello_regressione_multipla)
grafico_residui <- ggplot(data = transposed_data, aes(x = fitted(modello_regressione_multipla), y = residui_standardizzati, color = Country)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Valori Predetti", y = "Residui Standardizzati", title = "Grafico dei Residui Standardizzati") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

salva_grafico(grafico_residui, sotto_cartella_regressione, "residui_standardizzati.pdf")

# QQ Plot dei residui
qq_plot <- ggplot(data = transposed_data, aes(sample = residuals(modello_regressione_multipla))) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot dei Residui") +
  theme_bw(base_size = 14) +
  theme(
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

salva_grafico(qq_plot, sotto_cartella_regressione, "qqplot_residui.pdf")

# Grafico Scale-Location
scale_location_plot <- ggplot(data = transposed_data, aes(x = fitted(modello_regressione_multipla), y = sqrt(abs(residuals(modello_regressione_multipla))))) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Valori Predetti", y = "Radice Quadrata dei Residui Assoluti", title = "Grafico Scale-Location") +
  theme_bw(base_size = 14) +
  theme(
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

salva_grafico(scale_location_plot, sotto_cartella_regressione, "scale_location.pdf")

# Validazione incrociata per modello lineare
set.seed(123) # Per riproducibilità
indices <- sample(1:nrow(transposed_data), size = 0.7 * nrow(transposed_data))
training_data <- transposed_data[indices, ]
test_data <- transposed_data[-indices, ]

# Adattare il modello ai dati di training
modello_cv <- lm(`Demand-based CO2 emissions` ~ `Production-based CO2 emissions`, data = training_data)

# Valutare il modello sui dati di test
predizioni <- predict(modello_cv, newdata = test_data)
reale <- test_data$`Demand-based CO2 emissions`

# Calcolare l'errore quadratico medio (MSE)
mse <- mean((reale - predizioni)^2)
cat("L'errore quadratico medio (MSE) sul set di test è:", mse, "\n")
