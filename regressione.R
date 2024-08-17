# Librerie necessarie
library(ggplot2)

# Imposta la directory di destinazione per i grafici scatter
sotto_cartella_scatter <- file.path(directory_destinazione, "Regressione")
crea_directory(sotto_cartella_scatter)

# Funzione per salvare i grafici in modalità A4 orizzontale
salva_grafico <- function(grafico, directory, nome_file) {
  ggsave(
    filename = file.path(directory, nome_file), 
    plot = grafico, 
    width = 29.7,   # Larghezza in cm per A4 orizzontale
    height = 21,    # Altezza in cm per A4 orizzontale
    units = "cm"    # Specifica le unità come centimetri
  )
}


# Funzione per pulire il nome del file e rimuovere i caratteri indesiderati
pulisci_nome_file <- function(nome_file) {
  nome_pulito <- gsub("[-]", " ", nome_file)  # Sostituisci "-" con spazio
  nome_pulito <- gsub("[_]", " ", nome_pulito)  # Sostituisci "-" con spazio
  nome_pulito <- sub("[.]txt$", "", nome_pulito)  # Rimuovi ".txt" solo alla fine
  return(nome_pulito)
}

# Funzione per creare, salvare e visualizzare i modelli
crea_modello <- function(formula, data, modello_file, riassunto_file, confidenza_file, residui_file, qqplot_file, scalelocation_file, sotto_cartella) {
  # Crea il modello
  modello <- lm(formula, data = data)
  
  # Salva il modello
  salva_modello(modello, sotto_cartella, modello_file)
  
  # Salva il riassunto del modello
  output_riassunto <- capture.output(summary(modello))
  write(output_riassunto, file = file.path(sotto_cartella, riassunto_file))
  
  # Salva gli intervalli di confidenza
  intervalli_confidenza <- capture.output(confint(modello, level = 0.95))
  write(intervalli_confidenza, file = file.path(sotto_cartella, confidenza_file))
  
  # Pulisci il nome del file per il titolo del grafico
  titolo_grafico <- pulisci_nome_file(modello_file)
  
  # Crea il grafico dei residui standardizzati con facet per paese e senza leggenda
  residui_standardizzati <- rstandard(modello)
  grafico_residui <- ggplot(data = data, aes(x = fitted(modello), y = residui_standardizzati, color = Country)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    facet_wrap(~ Country) +  # Crea un plot per ogni paese
    labs(x = "Valori Predetti", y = "Residui Standardizzati", title = paste("Residui Standardizzati -", titolo_grafico)) +
    theme_bw(base_size = 14) +
    theme(legend.position = "none")  # Rimuove la leggenda
  salva_grafico(grafico_residui, sotto_cartella, residui_file)
  
  # Crea il QQ plot con facet per paese e ogni facet colorato diversamente
  qq_plot <- ggplot(data = data, aes(sample = residuals(modello), color = Country)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~ Country) +  # Crea un plot per ogni paese
    labs(title = paste("QQ Plot -", titolo_grafico)) +
    theme_bw(base_size = 14) +
    theme(legend.position = "none")  # Rimuove la leggenda
  salva_grafico(qq_plot, sotto_cartella, qqplot_file)
  
  # Crea il grafico Scale-Location con facet per paese e senza leggenda
  scale_location_plot <- ggplot(data = data, aes(x = fitted(modello), y = sqrt(abs(residuals(modello))), color = Country)) +
    geom_point() +
    geom_smooth(se = FALSE, color = "red") +  # Aggiunge la linea di regressione in rosso
    facet_wrap(~ Country) +  # Crea un plot per ogni paese
    labs(x = "Valori Predetti", y = "Radice Quadrata dei Residui Assoluti", title = paste("Scale-Location -", titolo_grafico)) +
    theme_bw(base_size = 14) +
    theme(legend.position = "none")  # Rimuove la leggenda
  
  # Salva il grafico Scale-Location
  salva_grafico(scale_location_plot, sotto_cartella, scalelocation_file)
  
  return(modello)
}


# 1. Modello polinomiale per scatter_population_vs_percent
modello_population_vs_percent <- crea_modello(
  formula = `Percentage of population exposed to more than 10 micrograms/m3` ~ 
    poly(`Population with access to improved drinking water sources, % total population`, 2),
  data = prepara_dati_per_modello(transposed_data, `Percentage of population exposed to more than 10 micrograms/m3` ~ 
                                    poly(`Population with access to improved drinking water sources, % total population`, 2)),
  modello_file = "modello_population_vs_percent.txt",
  riassunto_file = "Riassunto_Modello_Population_vs_Percent.txt",
  confidenza_file = "Intervalli_Confidenza_Population_vs_Percent.txt",
  residui_file = "residui_population_vs_percent.pdf",
  qqplot_file = "qqplot_population_vs_percent.pdf",
  scalelocation_file = "scale_location_population_vs_percent.pdf",
  sotto_cartella = sotto_cartella_scatter
)

# 2. Modello lineare per scatter_electricity_vs_percent
modello_electricity_vs_percent <- crea_modello(
  formula = `Percentage of population exposed to more than 10 micrograms/m3` ~ `Renewable electricity, % total electricity generation`,
  data = prepara_dati_per_modello(transposed_data, `Percentage of population exposed to more than 10 micrograms/m3` ~ 
                                    `Renewable electricity, % total electricity generation`),
  modello_file = "modello_electricity_vs_percent.txt",
  riassunto_file = "Riassunto_Modello_Electricity_vs_Percent.txt",
  confidenza_file = "Intervalli_Confidenza_Electricity_vs_Percent.txt",
  residui_file = "residui_electricity_vs_percent.pdf",
  qqplot_file = "qqplot_electricity_vs_percent.pdf",
  scalelocation_file = "scale_location_electricity_vs_percent.pdf",
  sotto_cartella = sotto_cartella_scatter
)

# 3. Modello lineare per scatter_energia_vs_elettricita
modello_energia_vs_elettricita <- crea_modello(
  formula = `Renewable electricity, % total electricity generation` ~ 
    `Renewable energy supply, % total energy supply`,
  data = prepara_dati_per_modello(transposed_data, `Renewable electricity, % total electricity generation` ~ 
                                    `Renewable energy supply, % total energy supply`),
  modello_file = "modello_energia_vs_elettricita.txt",
  riassunto_file = "Riassunto_Modello_Energia_vs_Elettricita.txt",
  confidenza_file = "Intervalli_Confidenza_Energia_vs_Elettricita.txt",
  residui_file = "residui_energia_vs_elettricita.pdf",
  qqplot_file = "qqplot_energia_vs_elettricita.pdf",
  scalelocation_file = "scale_location_energia_vs_elettricita.pdf",
  sotto_cartella = sotto_cartella_scatter
)

# 4. Modello polinomiale per scatter_energia_vs_acqua
modello_energia_vs_acqua <- crea_modello(
  formula = `Water stress, total freshwater abstraction as % total available renewable resources` ~ 
    poly(`Renewable energy supply, % total energy supply`, 2),
  data = prepara_dati_per_modello(transposed_data, `Water stress, total freshwater abstraction as % total available renewable resources` ~ 
                                    poly(`Renewable energy supply, % total energy supply`, 2)),
  modello_file = "modello_energia_vs_acqua.txt",
  riassunto_file = "Riassunto_Modello_Energia_vs_Acqua.txt",
  confidenza_file = "Intervalli_Confidenza_Energia_vs_Acqua.txt",
  residui_file = "residui_energia_vs_acqua.pdf",
  qqplot_file = "qqplot_energia_vs_acqua.pdf",
  scalelocation_file = "scale_location_energia_vs_acqua.pdf",
  sotto_cartella = sotto_cartella_scatter
)





# 7. Creazione del Modello di Regressione Multipla
sotto_cartella_regressione <- file.path(directory_destinazione, "Regressione_Multipla")
crea_directory(sotto_cartella_regressione)

modello_regressione_multipla <- crea_modello(
  formula = `Mortality from exposure to ambient PM2.5` ~ 
    `Percentage of population exposed to more than 10 micrograms/m3` + 
    `Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent`,
  data = transposed_data,
  modello_file = "modello_regressione_multipla.txt",
  riassunto_file = "Riassunto_Modello_Regressione_Multipla.txt",
  confidenza_file = "Intervalli_Confidenza.txt",
  residui_file = "residui_standardizzati_regressione_multipla.pdf",
  qqplot_file = "qqplot_residui_regressione_multipla.pdf",
  scalelocation_file = "scale_location_regressione_multipla.pdf",
  sotto_cartella = sotto_cartella_regressione
)

# 8. Modello lineare per "Production-based CO2 emissions" e "Demand-based CO2 emissions"
modello_production_vs_demand <- crea_modello(
  formula = `Demand-based CO2 emissions` ~ `Production-based CO2 emissions`,
  data = transposed_data,
  modello_file = "modello_production_vs_demand.txt",
  riassunto_file = "Riassunto_Modello_Production_vs_Demand.txt",
  confidenza_file = "Intervalli_Confidenza_Production_vs_Demand.txt",
  residui_file = "residui_production_vs_demand.pdf",
  qqplot_file = "qqplot_production_vs_demand.pdf",
  scalelocation_file = "scale_location_production_vs_demand.pdf",
  sotto_cartella = sotto_cartella_scatter
)

# 9. Validazione incrociata per modello lineare
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
cat("L'errore quadratico medio (MSE) sul set di test è:", mse, "\n", 
    file = file.path(sotto_cartella_regressione, "Risultati_Validazione_Incrociata.txt"))
