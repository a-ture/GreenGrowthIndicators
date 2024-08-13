# Carica le librerie necessarie
library(dplyr)
library(readr)
library(mice)
library(zoo)
library(tidyr)


# Pulisci l'ambiente di lavoro
rm(list = ls())

# Imposta la directory di lavoro e leggi il file
dataset <- read_csv("dataset/dataset.csv")
dataset1 <- read_csv("dataset/dataset.csv")

print(unique(dataset$Variable))
paesi_u <- unique(dataset$Country)

# Estrai l'elenco dei paesi unici
paesi_unici <- unique(dataset$Country)
variabili_unici <- unique(dataset$Variable)

variabili <- c(
  # Co2
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

# Elenco dei paesi da mantenere
paesi_da_mantenere <- c(
  "Germany",
  "France",
  "United Kingdom",
  "Italy",
  "Sweden",
  "Netherlands",
  "Belgium",
  "Spain",
  "Poland",
  "Denmark",
  "Finland",
  "Greece",
  "Hungary",
  "Ireland",
  "Luxembourg",
  "Portugal",
  "United States",
  "Canada",
  "Mexico",
  "Japan",
  "Korea",
  "New Zealand",
  "Norway",
  "China (People's Republic of)",
  "India",
  "Russia",
  "Indonesia",
  "South Africa",
  "Australia",
  "Brazil",
  "South Africa",
  "Argentina",
  #Nei dataset aggiuntivi questi paesi sono scritti in modo diverso
  "Korea, Rep.",
  "China",
  "Russian Federation"
)

paesi_da_mantenere_tradotto <- c(
  "Germania",
  "Francia",
  "Regno Unito",
  "Italia",
  "Svezia",
  "Paesi Bassi",
  "Belgio",
  "Spagna",
  "Polonia",
  "Danimarca",
  "Finlandia",
  "Grecia",
  "Ungheria",
  "Irlanda",
  "Lussemburgo",
  "Portogallo",
  "Stati Uniti",
  "Canada",
  "Messico",
  "Giappone",
  "Corea",
  "Nuova Zelanda",
  "Norvegia",
  "Cina",
  "India",
  "Brasile",
  "Russia",
  "Indonesia",
  "Australia",
  "Sud Africa",
  "Argentina"
)

# Filtra il dataset per includere solo i paesi selezionati
dataset <- dataset %>%
  filter(Country %in% paesi_da_mantenere)

# Filtra il dataset per includere solo le variabili specificate e le righe senza valori mancanti
dataset <- dataset %>%
  filter(Variable %in% variabili & !is.na(Value))

# Filtra il dataset per includere solo i paesi selezionati
dataset <- dataset %>%
  filter(YEA >= "1995" & YEA <= "2018")

# Elimina le colonne non necessarie
dataset <-
  dataset[,-which(
    names(dataset) %in% c(
      "COU",
      "Year",
      "VAR",
      "PowerCode",
      "PowerCode Code",
      "Reference Period Code",
      "Reference Period",
      "Unit Code" ,
      "Flag Codes",
      "Flags"
    )
  )]

# Dizionario di traduzione
traduzioni <- c(
  "Germany" = "Germania",
  "France" = "Francia",
  "United Kingdom" = "Regno Unito",
  "Italy" = "Italia",
  "Sweden" = "Svezia",
  "Netherlands" = "Paesi Bassi",
  "Belgium" = "Belgio",
  "Spain" = "Spagna",
  "Poland" = "Polonia",
  "Denmark" = "Danimarca",
  "Finland" = "Finlandia",
  "Greece" = "Grecia",
  "Hungary" = "Ungheria",
  "Ireland" = "Irlanda",
  "Luxembourg" = "Lussemburgo",
  "Portugal" = "Portogallo",
  "United States" = "Stati Uniti",
  "Canada" = "Canada",
  "Mexico" = "Messico",
  "Japan" = "Giappone",
  "Korea" = "Corea",
  "New Zealand" = "Nuova Zelanda",
  "Norway" = "Norvegia",
  "China (People's Republic of)" = "Cina",
  "India" = "India",
  "Russia" = "Russia",
  "Taiwan" = "Taiwan",
  "Indonesia" =  "Indonesia",
  "Brazil" = "Brasile",
  "Australia" = "Australia",
  "South Africa" = "Sud Africa",
  "Argentina" = "Argentina",
  "Thailand" = "Tailanda",
  "China" = "Cina",
  "Korea, Rep." = "Corea",
  "Russian Federation" = "Russia"
)

# Esegui la traduzione
dataset$Country <- traduzioni[dataset$Country]

# Controlla se ci sono NA dopo la traduzione
sum(is.na(dataset$Country))

# Stampa i paesi unici dopo la traduzione
print(unique(dataset$Country))

# Calcola il numero totale di dati mancanti per ogni colonna
dati_mancanti <- colSums(is.na(dataset))

# Visualizza i dati mancanti
print(dati_mancanti)

# Estrai l'elenco degli indicatori unici
indicatori_unici <- unique(dataset$Variable)

# Sostituisci NA con una categoria specifica
dataset$Unit[is.na(dataset$Unit)] <- "Non Specificato"

# Creare un elenco vuoto per conservare i risultati
lista_paesi_per_variabile <- list()

# Ciclo su ciascuna variabile
for (variabile in variabili) {
  dataset_filtrato <- dataset %>% filter(Variable == variabile)
  paesi_con_dati <- unique(dataset_filtrato$Country)
  lista_paesi_per_variabile[[variabile]] <- paesi_con_dati
}

# Visualizzare l'elenco dei paesi per variabile
lista_paesi_per_variabile

# Mappatura paese-continente
mappatura_continenti <- list(
  Europa_Occidentale = c(
    "Germania",
    "Francia",
    "Regno Unito",
    "Italia",
    "Spagna",
    "Paesi Bassi",
    "Belgio",
    "Lussemburgo"
  ),
  Europa_Orientale = c(
    "Polonia",
    "Ungheria",
    "Danimarca",
    "Finlandia",
    "Grecia",
    "Irlanda",
    "Portogallo",
    "Norvegia",
    "Svezia"
  ),
  America = c("Stati Uniti",
              "Canada",
              "Messico",
              "Brasile",
              "Argentina"),
  Asia = c(
    "Giappone",
    "Corea",
    "Pakistan",
    "Cina",
    "India",
    "Russia",
    "Indonesia"
  ),
  Oceania_E_Africa = c("Nuova Zelanda",
                       "Australia", "Sud Africa", "Nigeria")
)

# Funzione per controllare la disponibilità dei dati per ogni variabile e continente
check_data_availability <-
  function(variabile, dataset, mappatura_continenti) {
    for (cont in names(mappatura_continenti)) {
      dati_continente <- dataset %>%
        filter(Variable == variabile, Country %in% mappatura_continenti[[cont]]) %>%
        arrange(YEA)
      if (nrow(dati_continente) == 0) {
        print(
          paste(
            "Nessun dato disponibile per la variabile",
            variabile,
            "nel continente",
            cont
          )
        )
      }
    }
  }

# Esegui la funzione per ogni variabile
for (variabile in variabili) {
  check_data_availability(variabile, dataset, mappatura_continenti)
}

# Funzione per controllare la disponibilità dei dati per ogni variabile e paese
check_data_availability_per_country <-
  function(variabile, dataset, paesi) {
    conta <- 0
    for (paese in paesi) {
      dati_paese <- dataset %>%
        filter(Variable == variabile, Country == paese)
      if (nrow(dati_paese) == 0) {
        # print(paste(
        #   "Nessun dato disponibile per la variabile",
        #   variabile,
        #   "nel paese",
        #   paese
        # ))
        conta <- conta + 1
      }
    }
    return(conta)
  }

# Esegui la funzione per tutte le variabili e paesi nel dataset
total_missing_values_per_variable <- numeric(length(variabili))

# Esegui la funzione per ogni variabile e per ogni paese
for (i in seq_along(variabili)) {
  total_missing_values_per_variable[i] <-
    check_data_availability_per_country(variabili[i], dataset, paesi_da_mantenere_tradotto)
  cat(
    "Variabile:",
    variabili[i],
    "- Valori mancanti totali:",
    total_missing_values_per_variable[i],
    "\n"
  )
}

write_csv(dataset, paste0("datasetCambiato", ".csv"))

# Funzione per calcolare il numero di valori disponibili per ogni paese e variabile
calcola_valori_per_paese_e_variabile <-
  function(dataset, variabili, paesi) {
    # Inizializza una matrice per contenere il numero di valori per paese e variabile
    numero_valori_per_paese_e_variabile <-
      matrix(0, nrow = length(paesi), ncol = length(variabili))
    rownames(numero_valori_per_paese_e_variabile) <- paesi
    colnames(numero_valori_per_paese_e_variabile) <- variabili
    
    # Calcola il numero di valori per ogni paese e variabile
    for (i in seq_along(variabili)) {
      for (j in seq_along(paesi)) {
        dataset_r <-
          filter(dataset, Variable == variabili[i] &
                   Country == paesi[j])
        numero_valori_per_paese_e_variabile[j, i] <-
          length(unique(dataset_r$YEA))
      }
    }
    
    return(numero_valori_per_paese_e_variabile)
  }

# Utilizza la funzione
numero_valori_per_paese_e_variabile <-
  calcola_valori_per_paese_e_variabile(dataset, variabili, paesi_da_mantenere_tradotto)

summary(dataset)

# Struttura del dataset
str(dataset)

# Panoramica rapida del dataset
glimpse(dataset)

# Completa il dataset con tutte le combinazioni di variabile, paese e anno
dataset <- dataset %>%
  complete(Variable, Country, YEA, fill = list(Value = NA, Unit = "Non Specificato")) %>%
  arrange(Variable, Country, YEA)

dataset_acqua <-
  read.csv("dataset/level_of_water_stress.csv", skip = 4)
problems(dataset_acqua)

dataset_population <-
  read.csv("dataset/population_water.csv", skip = 4)
problems(dataset_population)

# Trasformazione di dataset_acqua da wide a long
dataset_acqua_long <- dataset_acqua %>%
  pivot_longer(
    cols = matches("^X\\d{4}$"),
    # Seleziona colonne che corrispondono al pattern degli anni
    names_to = "YEA",
    values_to = "Water_Stress"
  ) %>%
  select(Country = `Country.Name`, YEA, Water_Stress)

# Trasformazione di dataset_terra da wide a long
dataset_population_long <- dataset_population %>%
  pivot_longer(
    cols = matches("^X\\d{4}$"),
    # Seleziona colonne che corrispondono al pattern degli anni
    names_to = "YEA",
    values_to = "Value_Pop"
  ) %>%
  select(Country = `Country.Name`, YEA, Value_Pop)

library(stringr)

dataset_acqua_long <- dataset_acqua %>%
  pivot_longer(
    cols = matches("^X\\d{4}$"),
    names_to = "YEA",
    values_to = "Water_Stress"
  ) %>%
  mutate(YEA = str_replace(YEA, "^X", "")) %>%
  select(Country = `Country.Name`, YEA, Water_Stress)

dataset_population_long <- dataset_population %>%
  pivot_longer(
    cols = matches("^X\\d{4}$"),
    names_to = "YEA",
    values_to = "Value_Pop"
  ) %>%
  mutate(YEA = str_replace(YEA, "^X", "")) %>%
  select(Country = `Country.Name`, YEA, Value_Pop)

# Assicurati che YEA sia un valore numerico
dataset_acqua_long <- dataset_acqua_long %>%
  mutate(YEA = as.numeric(YEA)) %>%
  filter(YEA >= 1995 & YEA <= 2021)

dataset_population_long <- dataset_population_long %>%
  mutate(YEA = as.numeric(YEA)) %>%
  filter(YEA >= 1995 & YEA <= 2021)

# Filtra il dataset per includere solo i paesi selezionati
dataset_acqua_long <- dataset_acqua_long %>%
  filter(Country %in% paesi_da_mantenere)

dataset_population_long <- dataset_population_long %>%
  filter(Country %in% paesi_da_mantenere)

# Esegui la traduzione
dataset_acqua_long$Country <- traduzioni[dataset_acqua_long$Country]

# Esegui la traduzione
dataset_population_long$Country <-
  traduzioni[dataset_population_long$Country]

# Controlla nuovamente i valori nulli
print("Valori nulli prima acqua")
valori_nulli_dopo <- sum(is.na(dataset$Value))
print(valori_nulli_dopo)

# Unisci il dataset principale con dataset_acqua_long basandoti su 'Country' e 'YEA'
dataset <- dataset %>%
  left_join(dataset_acqua_long, by = c("Country", "YEA"))

# Aggiorna i valori della variabile "Water stress, total freshwater abstraction as % total available renewable resources"
# nel dataset principale con i valori da dataset_acqua_long
dataset <- dataset %>%
  mutate(
    Value = ifelse(
      Variable == "Water stress, total freshwater abstraction as % total available renewable resources",
      Water_Stress,
      Value
    )
  )

# A questo punto, puoi rimuovere le colonne extra aggiunte da dataset_acqua_long se non sono più necessarie
dataset <- select(dataset, -Water_Stress)

# Controlla nuovamente i valori nulli
print("Valori nulli dopo acqua")
valori_nulli_dopo <- sum(is.na(dataset$Value))
print(valori_nulli_dopo)

# Controlla nuovamente i valori nulli
print("Valori nulli prima popolazione")
valori_nulli_dopo <- sum(is.na(dataset$Value))
print(valori_nulli_dopo)

# Assumi che la colonna con i valori sia 'Land_Protected' (aggiusta se necessario)
# Unione del dataset principale con dataset_terra_long
dataset <-
  left_join(dataset, dataset_population_long, by = c("Country", "YEA"))

# Sostituisci i valori NA per la variabile specifica
dataset <- dataset %>%
  mutate(
    Value = ifelse(
      Variable == "Population with access to improved drinking water sources, % total population" &
        is.na(Value),
      Value_Pop,
      Value
    )
  )

# Rimuovi le colonne extra non necessarie
dataset <- select(dataset, -Value_Pop)

dataset <- dataset %>%
  mutate(
    Value = ifelse(
      Country %in% c("Lussemburgo", "Ungheria") &
        Variable == "Marine protected area, % total exclusive economic zone",
      0,
      Value
    )
  )

library(forecast)

# Estrarre le serie temporali per il Brasile e l'India
serie_brasile <-
  ts(dataset$Value[dataset$Country == "Brasile" &
                     dataset$Variable == "Environmentally related taxes, % GDP"], start = c(1995), frequency =
       1)
serie_india <-
  ts(dataset$Value[dataset$Country == "India" &
                     dataset$Variable == "Environmentally related taxes, % GDP"], start = c(1995), frequency =
       1)

# Modellare la serie temporale (esempio con ARIMA)
model_brasile <- auto.arima(serie_brasile)
model_india <- auto.arima(serie_india)

# Prevedere i valori per gli anni mancanti per la Russia e l'Indonesia
previsioni_russia <- forecast(model_brasile, h = 24)
previsioni_indonesia <- forecast(model_india, h = 24)

# Imputare i valori previsti nel dataset
# Nota: Assicurati che gli anni delle previsioni corrispondano agli anni mancanti nel dataset
dataset$Value[dataset$Country == "Russia" &
                dataset$Variable == "Environmentally related taxes, % GDP" &
                is.na(dataset$Value)] <- previsioni_russia$mean
dataset$Value[dataset$Country == "Indonesia" &
                dataset$Variable == "Environmentally related taxes, % GDP" &
                is.na(dataset$Value)] <- previsioni_indonesia$mean


serie_stati_uniti <-
  ts(dataset$Value[dataset$Country == "Stati Uniti" &
                     dataset$Variable == "Population with access to improved drinking water sources, % total population"], start = c(1995), frequency =
       1)

serie_brasile <-
  ts(dataset$Value[dataset$Country == "Brasile" &
                     dataset$Variable == "Population with access to improved drinking water sources, % total population"], start = c(1995), frequency =
       1)

serie_indonesia <-
  ts(dataset$Value[dataset$Country == "Indonesia" &
                     dataset$Variable == "Population with access to improved drinking water sources, % total population"], start = c(1995), frequency =
       1)

serie_messico <-
  ts(dataset$Value[dataset$Country == "Messico" &
                     dataset$Variable == "Population with access to improved drinking water sources, % total population"], start = c(1995), frequency =
       1)

# Modellare la serie temporale (esempio con ARIMA)
model_cina <- auto.arima(serie_indonesia)
model_india <- auto.arima(serie_brasile)
model_sud_africa  <- auto.arima(serie_messico)
model_australia <- auto.arima(serie_stati_uniti)
model_argentina  <- auto.arima(serie_brasile)

# Prevedere i valori per gli anni mancanti per la Russia e l'Indonesia
previsioni_cina <- forecast(model_cina, h = 24)
previsioni_india <- forecast(model_india, h = 24)
previsioni_sud_africa <- forecast(model_sud_africa, h = 24)
previsioni_australia <- forecast(model_australia, h = 24)
previsioni_argentina <- forecast(model_argentina, h = 24)

# Imputare i valori previsti nel dataset
dataset$Value[dataset$Country == "Cina" &
                dataset$Variable == "Population with access to improved drinking water sources, % total population" &
                is.na(dataset$Value)] <- previsioni_cina$mean
dataset$Value[dataset$Country == "India" &
                dataset$Variable == "Population with access to improved drinking water sources, % total population" &
                is.na(dataset$Value)] <- previsioni_india$mean
dataset$Value[dataset$Country == "Sud Africa" &
                dataset$Variable == "Population with access to improved drinking water sources, % total population" &
                is.na(dataset$Value)] <- previsioni_sud_africa$mean
dataset$Value[dataset$Country == "Australia" &
                dataset$Variable == "Population with access to improved drinking water sources, % total population" &
                is.na(dataset$Value)] <- previsioni_australia$mean
dataset$Value[dataset$Country == "Argentina" &
                dataset$Variable == "Population with access to improved drinking water sources, % total population" &
                is.na(dataset$Value)] <- previsioni_argentina$mean

# Rimuovere le righe con valori NA
dataset_pulito <- na.omit(dataset)

# Utilizza la funzione
numero_valori_per_paese_e_variabile <-
  calcola_valori_per_paese_e_variabile(dataset_pulito, variabili, paesi_da_mantenere_tradotto)

print("Valori nulli prima interpolazione")
valori_nulli <- sum(is.na(dataset$Value))
print(valori_nulli)

# Esegui l'interpolazione in base alla serie storica per ogni paese e variabile
dataset_interpolato <- dataset %>%
  group_by(Country, Variable) %>%
  mutate(Value = na.approx(Value, na.rm = FALSE, rule = 2)) %>%
  ungroup()

dataset <- dataset_interpolato

print("Valori nulli dopo interpolazione")
valori_nulli <- sum(is.na(dataset$Value))
print(valori_nulli)




