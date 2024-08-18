#Pulizia del dataset, descritto nel capitolo 2

# Carica le librerie necessarie
library(dplyr)
library(readr)
library(mice)
library(zoo)
library(tidyr)
library(stringr)
library(forecast)

# Pulisci l'ambiente di lavoro
rm(list = ls())

# Imposta la directory di lavoro e leggi i file
dataset <- read_csv("dataset/dataset.csv")
dataset1 <- read_csv("dataset/dataset.csv") # Non sembra essere utilizzato ulteriormente

# Stampa le variabili uniche e i paesi unici per un controllo iniziale
print(unique(dataset$Variable))
paesi_unici <- unique(dataset$Country)

# Definisci l'elenco delle variabili da mantenere per l'analisi
variabili <- c(
  # Emissioni di CO2
  "Production-based CO2 emissions",
  "Demand-based CO2 emissions",
  
  # Energia
  "Renewable energy supply, % total energy supply",
  "Renewable electricity, % total electricity generation",
  
  # Acqua
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

# Definisci l'elenco dei paesi da mantenere (inglese e corrispondenti nomi tradotti)
paesi_da_mantenere <- c(
  "Germany", "France", "United Kingdom", "Italy", "Sweden", "Netherlands",
  "Belgium", "Spain", "Poland", "Denmark", "Finland", "Greece", "Hungary",
  "Ireland", "Luxembourg", "Portugal", "United States", "Canada", "Mexico",
  "Japan", "Korea", "New Zealand", "Norway", "China (People's Republic of)",
  "India", "Russia", "Indonesia", "South Africa", "Australia", "Brazil", "Argentina",
  # Varianti di nomi nei dataset aggiuntivi
  "Korea, Rep.", "China", "Russian Federation"
)

# Definisci i nomi tradotti dei paesi
paesi_da_mantenere_tradotto <- c(
  "Germania", "Francia", "Regno Unito", "Italia", "Svezia", "Paesi Bassi",
  "Belgio", "Spagna", "Polonia", "Danimarca", "Finlandia", "Grecia", 
  "Ungheria", "Irlanda", "Lussemburgo", "Portogallo", "Stati Uniti", 
  "Canada", "Messico", "Giappone", "Corea", "Nuova Zelanda", "Norvegia",
  "Cina", "India", "Brasile", "Russia", "Indonesia", "Australia", 
  "Sud Africa", "Argentina"
)

# Filtra il dataset per includere solo i paesi e le variabili selezionati
dataset <- dataset %>%
  filter(Country %in% paesi_da_mantenere) %>%
  filter(Variable %in% variabili & !is.na(Value)) %>%
  filter(YEA >= 1995 & YEA <= 2018)

# Rimuovi le colonne non necessarie per l'analisi
dataset <- dataset %>%
  dplyr::select(-c("COU", "Year", "VAR", "PowerCode", "PowerCode Code", 
            "Reference Period Code", "Reference Period", "Unit Code", 
            "Flag Codes", "Flags"))

# Dizionario di traduzione per i nomi dei paesi
traduzioni <- c(
  "Germany" = "Germania", "France" = "Francia", "United Kingdom" = "Regno Unito", 
  "Italy" = "Italia", "Sweden" = "Svezia", "Netherlands" = "Paesi Bassi", 
  "Belgium" = "Belgio", "Spain" = "Spagna", "Poland" = "Polonia", 
  "Denmark" = "Danimarca", "Finland" = "Finlandia", "Greece" = "Grecia", 
  "Hungary" = "Ungheria", "Ireland" = "Irlanda", "Luxembourg" = "Lussemburgo", 
  "Portugal" = "Portogallo", "United States" = "Stati Uniti", 
  "Canada" = "Canada", "Mexico" = "Messico", "Japan" = "Giappone", 
  "Korea" = "Corea", "New Zealand" = "Nuova Zelanda", "Norway" = "Norvegia", 
  "China (People's Republic of)" = "Cina", "India" = "India", 
  "Russia" = "Russia", "Indonesia" = "Indonesia", "Brazil" = "Brasile", 
  "Australia" = "Australia", "South Africa" = "Sud Africa", "Argentina" = "Argentina", 
  "China" = "Cina", "Korea, Rep." = "Corea", "Russian Federation" = "Russia"
)

# Applica la traduzione dei nomi dei paesi nel dataset
dataset$Country <- traduzioni[dataset$Country]

# Verifica se ci sono NA dopo la traduzione dei nomi dei paesi
sum(is.na(dataset$Country))

# Stampa i nomi dei paesi unici dopo la traduzione per verifica
print(unique(dataset$Country))

# Controlla la dimensione del dataset dopo il filtraggio di paesi e variabili
dim(dataset)

# Controlla la presenza di dati mancanti per ogni colonna del dataset
dati_mancanti <- colSums(is.na(dataset))
print(dati_mancanti)

# Sostituisci i valori NA nella colonna "Unit" con "Non Specificato"
dataset$Unit[is.na(dataset$Unit)] <- "Non Specificato"

# Inizializza un elenco vuoto per conservare i risultati dei paesi con dati per ogni variabile
lista_paesi_per_variabile <- list()

# Ciclo su ciascuna variabile per identificare i paesi con dati disponibili
for (variabile in variabili) {
  dataset_filtrato <- dataset %>% filter(Variable == variabile)
  paesi_con_dati <- unique(dataset_filtrato$Country)
  lista_paesi_per_variabile[[variabile]] <- paesi_con_dati
}

# Visualizza l'elenco dei paesi per variabile
print(lista_paesi_per_variabile)

# Mappatura paese-continente per una successiva analisi regionale
mappatura_continenti <- list(
  Europa_Occidentale = c("Germania", "Francia", "Regno Unito", "Italia", 
                         "Spagna", "Paesi Bassi", "Belgio", "Lussemburgo"),
  Europa_Orientale = c("Polonia", "Ungheria", "Danimarca", "Finlandia", 
                       "Grecia", "Irlanda", "Portogallo", "Norvegia", "Svezia"),
  America = c("Stati Uniti", "Canada", "Messico", "Brasile", "Argentina"),
  Asia = c("Giappone", "Corea", "Pakistan", "Cina", "India", "Russia", "Indonesia"),
  Oceania_E_Africa = c("Nuova Zelanda", "Australia", "Sud Africa", "Nigeria")
)

# Funzione per controllare la disponibilità dei dati per ogni variabile e continente
check_data_availability <- function(variabile, dataset, mappatura_continenti) {
  for (cont in names(mappatura_continenti)) {
    dati_continente <- dataset %>%
      filter(Variable == variabile, Country %in% mappatura_continenti[[cont]]) %>%
      arrange(YEA)
    if (nrow(dati_continente) == 0) {
      print(paste("Nessun dato disponibile per la variabile", variabile, "nel continente", cont))
    }
  }
}

# Esegui la funzione per ogni variabile per controllare la disponibilità dei dati nei continenti
for (variabile in variabili) {
  check_data_availability(variabile, dataset, mappatura_continenti)
}

# Funzione per controllare la disponibilità dei dati per ogni variabile e paese
check_data_availability_per_country <- function(variabile, dataset, paesi) {
  conta <- 0
  for (paese in paesi) {
    dati_paese <- dataset %>%
      filter(Variable == variabile, Country == paese)
    if (nrow(dati_paese) == 0) {
      conta <- conta + 1
    }
  }
  return(conta)
}

# Conta i valori mancanti per ogni variabile e paese
total_missing_values_per_variable <- numeric(length(variabili))

for (i in seq_along(variabili)) {
  total_missing_values_per_variable[i] <- check_data_availability_per_country(variabili[i], dataset, paesi_da_mantenere_tradotto)
  cat("Variabile:", variabili[i], "- Valori mancanti totali:", total_missing_values_per_variable[i], "\n")
}

# Salva il dataset modificato
write_csv(dataset, "datasetCambiato.csv")

# Funzione per calcolare il numero di valori disponibili per ogni paese e variabile
calcola_valori_per_paese_e_variabile <- function(dataset, variabili, paesi) {
  numero_valori_per_paese_e_variabile <- matrix(0, nrow = length(paesi), ncol = length(variabili))
  rownames(numero_valori_per_paese_e_variabile) <- paesi
  colnames(numero_valori_per_paese_e_variabile) <- variabili
  
  for (i in seq_along(variabili)) {
    for (j in seq_along(paesi)) {
      dataset_r <- filter(dataset, Variable == variabili[i] & Country == paesi[j])
      numero_valori_per_paese_e_variabile[j, i] <- length(unique(dataset_r$YEA))
    }
  }
  
  return(numero_valori_per_paese_e_variabile)
}

# Calcola e visualizza il numero di valori disponibili per ogni paese e variabile
numero_valori_per_paese_e_variabile <- calcola_valori_per_paese_e_variabile(dataset, variabili, paesi_da_mantenere_tradotto)

# Visualizza un sommario del dataset e la sua struttura
summary(dataset)
str(dataset)
glimpse(dataset)

# Completa il dataset con tutte le combinazioni di variabile, paese e anno
dataset <- dataset %>%
  complete(Variable, Country, YEA, fill = list(Value = NA, Unit = "Non Specificato")) %>%
  arrange(Variable, Country, YEA)

# Carica e prepara i dataset aggiuntivi (acqua e popolazione)
dataset_acqua <- read.csv("dataset/level_of_water_stress.csv", skip = 4)
dataset_population <- read.csv("dataset/population_water.csv", skip = 4)

# Trasformazione dei dataset aggiuntivi da formato wide a long
dataset_acqua_long <- dataset_acqua %>%
  pivot_longer(cols = matches("^X\\d{4}$"), names_to = "YEA", values_to = "Water_Stress") %>%
  mutate(YEA = str_replace(YEA, "^X", "")) %>%
  mutate(YEA = as.numeric(YEA)) %>%
  filter(YEA >= 1995 & YEA <= 2021) %>%
  dplyr::select(Country = `Country.Name`, YEA, Water_Stress)

dataset_population_long <- dataset_population %>%
  pivot_longer(cols = matches("^X\\d{4}$"), names_to = "YEA", values_to = "Value_Pop") %>%
  mutate(YEA = str_replace(YEA, "^X", "")) %>%
  mutate(YEA = as.numeric(YEA)) %>%
  filter(YEA >= 1995 & YEA <= 2021) %>%
  dplyr::select(Country = `Country.Name`, YEA, Value_Pop)

# Filtra i dataset aggiuntivi per includere solo i paesi selezionati
dataset_acqua_long <- dataset_acqua_long %>%
  filter(Country %in% paesi_da_mantenere)
dataset_population_long <- dataset_population_long %>%
  filter(Country %in% paesi_da_mantenere)

# Applica la traduzione dei nomi dei paesi anche ai dataset aggiuntivi
dataset_acqua_long$Country <- traduzioni[dataset_acqua_long$Country]
dataset_population_long$Country <- traduzioni[dataset_population_long$Country]

# Unisci il dataset principale con i dataset aggiuntivi (acqua e popolazione)
dataset <- dataset %>%
  left_join(dataset_acqua_long, by = c("Country", "YEA")) %>%
  mutate(Value = ifelse(Variable == "Water stress, total freshwater abstraction as % total available renewable resources", Water_Stress, Value)) %>%
  dplyr::select(-Water_Stress) %>%
  left_join(dataset_population_long, by = c("Country", "YEA")) %>%
  mutate(Value = ifelse(Variable == "Population with access to improved drinking water sources, % total population" & is.na(Value), Value_Pop, Value)) %>%
  dplyr::select(-Value_Pop)

# Imputa valori per i paesi privi di accesso a mari (es. Lussemburgo, Ungheria)
dataset <- dataset %>%
  mutate(Value = ifelse(Country %in% c("Lussemburgo", "Ungheria") & Variable == "Marine protected area, % total exclusive economic zone", 0, Value))

# Modelli ARIMA per l'imputazione di dati mancanti (esempio per Russia e Indonesia)
# Previsione delle tasse ambientali utilizzando dati di paesi simili
serie_brasile <- ts(dataset$Value[dataset$Country == "Brasile" & dataset$Variable == "Environmentally related taxes, % GDP"], start = c(1995), frequency = 1)
serie_india <- ts(dataset$Value[dataset$Country == "India" & dataset$Variable == "Environmentally related taxes, % GDP"], start = c(1995), frequency = 1)

# Costruzione dei modelli ARIMA
model_brasile <- auto.arima(serie_brasile)
model_india <- auto.arima(serie_india)

# Previsioni per Russia e Indonesia
previsioni_russia <- forecast(model_brasile, h = 24)
previsioni_indonesia <- forecast(model_india, h = 24)

# Imputazione delle previsioni nel dataset
dataset$Value[dataset$Country == "Russia" & dataset$Variable == "Environmentally related taxes, % GDP" & is.na(dataset$Value)] <- previsioni_russia$mean
dataset$Value[dataset$Country == "Indonesia" & dataset$Variable == "Environmentally related taxes, % GDP" & is.na(dataset$Value)] <- previsioni_indonesia$mean

# Modelli ARIMA per la previsione dell'accesso all'acqua potabile
serie_stati_uniti <- ts(dataset$Value[dataset$Country == "Stati Uniti" & dataset$Variable == "Population with access to improved drinking water sources, % total population"], start = c(1995), frequency = 1)
serie_brasile <- ts(dataset$Value[dataset$Country == "Brasile" & dataset$Variable == "Population with access to improved drinking water sources, % total population"], start = c(1995), frequency = 1)
serie_messico <- ts(dataset$Value[dataset$Country == "Messico" & dataset$Variable == "Population with access to improved drinking water sources, % total population"], start = c(1995), frequency = 1)

# Costruzione dei modelli ARIMA per Cina, India, Sud Africa, Australia, Argentina
model_cina <- auto.arima(serie_brasile)
model_india <- auto.arima(serie_brasile)
model_sud_africa <- auto.arima(serie_messico)
model_australia <- auto.arima(serie_stati_uniti)
model_argentina <- auto.arima(serie_brasile)

# Previsioni per Cina, India, Sud Africa, Australia e Argentina
previsioni_cina <- forecast(model_cina, h = 24)
previsioni_india <- forecast(model_india, h = 24)
previsioni_sud_africa <- forecast(model_sud_africa, h = 24)
previsioni_australia <- forecast(model_australia, h = 24)
previsioni_argentina <- forecast(model_argentina, h = 24)

# Imputazione delle previsioni nel dataset
dataset$Value[dataset$Country == "Cina" & dataset$Variable == "Population with access to improved drinking water sources, % total population" & is.na(dataset$Value)] <- previsioni_cina$mean
dataset$Value[dataset$Country == "India" & dataset$Variable == "Population with access to improved drinking water sources, % total population" & is.na(dataset$Value)] <- previsioni_india$mean
dataset$Value[dataset$Country == "Sud Africa" & dataset$Variable == "Population with access to improved drinking water sources, % total population" & is.na(dataset$Value)] <- previsioni_sud_africa$mean
dataset$Value[dataset$Country == "Australia" & dataset$Variable == "Population with access to improved drinking water sources, % total population" & is.na(dataset$Value)] <- previsioni_australia$mean
dataset$Value[dataset$Country == "Argentina" & dataset$Variable == "Population with access to improved drinking water sources, % total population" & is.na(dataset$Value)] <- previsioni_argentina$mean

# Rimuovi eventuali righe con valori NA
dataset_pulito <- na.omit(dataset)

# Calcola il numero di valori disponibili per paese e variabile nel dataset pulito
numero_valori_per_paese_e_variabile <- calcola_valori_per_paese_e_variabile(dataset_pulito, variabili, paesi_da_mantenere_tradotto)

# Interpolazione dei valori mancanti all'interno della serie temporale per ogni paese e variabile
print("Valori nulli prima dell'interpolazione")
valori_nulli <- sum(is.na(dataset$Value))
print(valori_nulli)

dataset_interpolato <- dataset %>%
  group_by(Country, Variable) %>%
  mutate(Value = na.approx(Value, na.rm = FALSE, rule = 2)) %>%
  ungroup()

dataset <- dataset_interpolato

print("Valori nulli dopo l'interpolazione")
valori_nulli <- sum(is.na(dataset$Value))
print(valori_nulli)


dim(dataset)