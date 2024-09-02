# Creare una copia del dataset originale
dataset_copy <- dataset

# Dataset di esempio con variabili selezionate
variabili_selezionate <- c(
  "Production-based CO2 emissions",
  "Demand-based CO2 emissions",
  "Renewable electricity, % total electricity generation",
  "Real GDP per capita"
)

# Data frame per conservare i risultati
risultati <- data.frame(
  Variabile = character(),
  Media = numeric(),
  Deviazione_Standard = numeric(),
  Campione = integer(),
  stringsAsFactors = FALSE
)

# Iterare attraverso ciascuna variabile e calcolare i parametri statistici
for (variabile in variabili_selezionate) {
  data_subset <- dataset %>%
    filter(Country == "Spagna" & Variable == variabile & !is.na(Value))
  
  media <- mean(data_subset$Value, na.rm = TRUE)
  deviazione_standard <- sd(data_subset$Value, na.rm = TRUE)
  n <- nrow(data_subset)
  
  risultati <- rbind(risultati, data.frame(
    Variabile = variabile,
    Media = round(media, 2),
    Deviazione_Standard = round(deviazione_standard, 2),
    Campione = n
  ))
}

# Stampare i risultati in formato tabellare
print(risultati)

# Creare la cartella "inferenzastatistica" se non esiste già
dir.create("inferenzastatistica", showWarnings = FALSE)

# Funzione per applicare il metodo dei momenti
method_of_moments <- function(data) {
  sample_mean <- mean(data$Value, na.rm = TRUE)
  sample_variance <- var(data$Value, na.rm = TRUE)
  
  estimated_mu <- sample_mean
  estimated_sigma_squared <- sample_variance
  
  return(list(mu = estimated_mu, sigma_squared = estimated_sigma_squared))
}

# Funzione per calcolare l'intervallo di confidenza per la differenza tra due medie con varianze note
confidence_interval_difference_means_known_vars <- function(mean1, mean2, var1, var2, n1, n2, conf_level = 0.95) {
  z_crit <- qnorm(1 - (1 - conf_level) / 2)
  margin_of_error <- z_crit * sqrt(var1 / n1 + var2 / n2)
  
  lower_bound <- (mean1 - mean2) - margin_of_error
  upper_bound <- (mean1 - mean2) + margin_of_error
  
  return(c(lower_bound, upper_bound))
}

# Funzione per calcolare l'intervallo di confidenza usando il metodo pivotale
confidence_interval_pivot <- function(data, sigma, conf_level = 0.95) {
  n <- length(data)
  sample_mean <- mean(data, na.rm = TRUE)
  
  # Quantili critici
  alpha <- 1 - conf_level
  z_crit <- qnorm(1 - alpha/2)  # Es. per un intervallo di confidenza del 95%, z_crit ≈ 1.96
  
  # Intervallo di confidenza
  margin_of_error <- z_crit * (sigma / sqrt(n))
  lower_bound <- sample_mean - margin_of_error
  upper_bound <- sample_mean + margin_of_error
  
  return(c(lower_bound, upper_bound))
}

# Funzione per calcolare intervalli basati sui quantili
calculate_intervals <- function(data, num_intervals) {
  sample_mean <- mean(data, na.rm = TRUE)
  sample_sd <- sd(data, na.rm = TRUE)
  
  # Calcolare i punti di rottura utilizzando i quantili della distribuzione normale
  breaks <- qnorm(seq(0, 1, length.out = num_intervals + 1), mean = sample_mean, sd = sample_sd)
  
  return(breaks)
}

paesi_selezionati <- c("Spagna")
variabili_selezionate <- c(
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

# Funzione per eseguire il test chi-quadrato con intervalli personalizzati
chi_square_test_custom <- function(data, breaks, variable_name) {
  # Assicurarsi che i dati siano numerici
  if (!is.numeric(data)) {
    stop("'data' deve essere numerico")
  }
  
  # Categorizzare i dati in intervalli
  categories <- cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE)
  
  # Calcolare le frequenze osservate e attese
  observed_frequencies <- table(categories)
  expected_frequencies <- rep(length(data) / length(observed_frequencies), length(observed_frequencies))
  
  # Stampare i limiti degli intervalli, il numero di elementi in ciascun intervallo e il nome della variabile
  cat("Variabile:", variable_name, "\n")
  print("Limiti degli intervalli e numero di elementi:")
  for (i in 1:(length(breaks) - 1)) {
    cat("Intervallo", i, ": [", breaks[i], ",", breaks[i + 1], "), Frequenza:", observed_frequencies[i], "\n")
  }
  
  # Verificare che le frequenze attese siano tutte maggiori o uguali a 5
  if (all(expected_frequencies >= 5)) {
    chi2_test <- chisq.test(observed_frequencies, p = expected_frequencies / sum(expected_frequencies))
    
    chi2_value <- chi2_test$statistic
    df <- length(observed_frequencies) - 3
    chi2_crit_upper <- qchisq(0.975, df)
    chi2_crit_lower <- qchisq(0.025, df)
    
    normal_distribution <- (chi2_value > chi2_crit_lower && chi2_value < chi2_crit_upper)
    
    return(list(p_value = chi2_test$p.value, chi2_value = chi2_value, chi2_crit_upper = chi2_crit_upper, chi2_crit_lower = chi2_crit_lower, normal_distribution = normal_distribution))
  } else {
    warning("Frequenze attese troppo basse per il test del chi-quadrato")
    return(list(p_value = NA, chi2_value = NA, chi2_crit_upper = NA, chi2_crit_lower = NA, normal_distribution = NA))
  }
}




# Fase di esplorazione e verifica delle ipotesi
chi_square_results <- list()
method_of_moments_results <- list()

for (country in paesi_selezionati) {
  for (variable in variabili_selezionate) {
    data_subset <- dataset_copy %>%
      filter(Country == country & Variable == variable & !is.na(Value))
    
    if (nrow(data_subset) > 10) {
      # Metodo dei momenti
      moments_result <- method_of_moments(data_subset)
      method_of_moments_results[[paste(country, variable, sep = " - ")]] <- moments_result
      
      # Test chi-quadrato
      breaks_4 <- calculate_intervals(data_subset$Value, num_intervals = 4)
      chi2_result_4 <- chi_square_test_custom(data_subset$Value, breaks = breaks_4,variable)
      
      chi_square_results[[paste(country, variable, sep = " - ")]] <- chi2_result_4
    }
  }
}

# Salvare i risultati del metodo dei momenti in un file CSV nella cartella "inferenzastatistica"
method_of_moments_df <- do.call(rbind, lapply(method_of_moments_results, function(x) t(as.data.frame(x))))
write.csv(method_of_moments_df, "inferenzastatistica/method_of_moments_results.csv", row.names = TRUE)

# Salvare i risultati del test Chi-quadrato in un file CSV nella cartella "inferenzastatistica"
chi_square_df <- do.call(rbind, lapply(chi_square_results, as.data.frame))
write.csv(chi_square_df, "inferenzastatistica/chi_square_results.csv", row.names = TRUE)

# Confronto della normalità in due diversi gruppi temporali per un singolo paese
paesi <- c("Germania")
compare_normality_results <- list()

compare_normality <- function(data, group1_year, group2_year, variable, num_intervals = 4) {
  data_group1 <- data %>%
    filter(YEA == group1_year & Variable == variable & Country %in% paesi) %>%
    pull(Value)
  
  data_group2 <- data %>%
    filter(YEA == group2_year & Variable == variable & Country %in% paesi) %>%
    pull(Value)
  
  result_group1 <- if (length(data_group1) > 10) chi_square_test_custom(data_group1, calculate_intervals(data_group1, num_intervals,variable)) else NULL
  result_group2 <- if (length(data_group2) > 10) chi_square_test_custom(data_group2, calculate_intervals(data_group2, num_intervals,variable)) else NULL
  
  return(list(group1 = result_group1, group2 = result_group2))
}

# Esempio di confronto tra due gruppi temporali
compare_normality_results <- compare_normality(dataset_copy, 1990, 2000, "Real GDP per capita")
compare_normality_df <- do.call(rbind, lapply(compare_normality_results, as.data.frame))
write.csv(compare_normality_df, "inferenzastatistica/compare_normality_results.csv", row.names = TRUE)

# Creare un data frame per raccogliere tutti gli intervalli di confidenza con le etichette appropriate
confidence_intervals_df <- data.frame(
  Country = character(),
  Variable = character(),
  Method = character(),
  Lower_Bound = numeric(),
  Upper_Bound = numeric(),
  stringsAsFactors = FALSE
)

variabili_normali <- c(
  "Production-based CO2 emissions",
  "Demand-based CO2 emissions",
  "Renewable electricity, % total electricity generation",
  "Real GDP per capita",
  "Mortality from exposure to ambient PM2.5",
  "Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent",
  "Environmentally related taxes, % GDP",
  "Terrestrial protected area, % land area"
  
)

# Iterazione su ciascuna variabile normale per calcolare gli intervalli di confidenza
for (variable in variabili_normali) {
  data_subset <- dataset %>%
    filter(Country == country & Variable == variable & !is.na(Value)) %>%
    pull(Value)
  
  if (length(data_subset) > 10) {
    sigma_known <- sd(data_subset, na.rm = TRUE)
    mu_known <- mean(data_subset, na.rm = TRUE)
    
    # Intervalli di confidenza per la media con varianza nota
    conf_interval_i <- confidence_interval_mean_var_known(data_subset, sigma = sigma_known)
    confidence_intervals_df <- rbind(confidence_intervals_df, data.frame(
      Country = country,
      Variable = variable,
      Method = "Mean with Known Variance",
      Lower_Bound = conf_interval_i[1],
      Upper_Bound = conf_interval_i[2]
    ))
    
    # Intervalli di confidenza per la media con varianza non nota
    conf_interval_ii <- confidence_interval_mean_var_unknown(data_subset)
    confidence_intervals_df <- rbind(confidence_intervals_df, data.frame(
      Country = country,
      Variable = variable,
      Method = "Mean with Unknown Variance",
      Lower_Bound = conf_interval_ii[1],
      Upper_Bound = conf_interval_ii[2]
    ))
    
    # Intervalli di confidenza per la varianza con media nota
    conf_interval_iii <- confidence_interval_variance_mean_known(data_subset, mu = mu_known)
    confidence_intervals_df <- rbind(confidence_intervals_df, data.frame(
      Country = country,
      Variable = variable,
      Method = "Variance with Known Mean",
      Lower_Bound = conf_interval_iii[1],
      Upper_Bound = conf_interval_iii[2]
    ))
    
    # Intervalli di confidenza per la varianza con media non nota
    conf_interval_iv <- confidence_interval_variance_mean_unknown(data_subset)
    confidence_intervals_df <- rbind(confidence_intervals_df, data.frame(
      Country = country,
      Variable = variable,
      Method = "Variance with Unknown Mean",
      Lower_Bound = conf_interval_iv[1],
      Upper_Bound = conf_interval_iv[2]
    ))
  }
}

# Salvare gli intervalli di confidenza in un file CSV nella cartella "inferenzastatistica"
write.csv(confidence_intervals_df, "inferenzastatistica/confidence_intervals_all_cases_labeled.csv", row.names = FALSE)

# Confronto tra due paesi su tutta la serie storica per variabili normali
country1 <- "Italia"
country2 <- "Spagna"
variabili_normali <- c(
  "Production-based CO2 emissions",
  "Demand-based CO2 emissions",
  "Renewable electricity, % total electricity generation",
  "Real GDP per capita"
)

compare_whole_series_results <- list()

# Confronto tra due paesi su tutta la serie storica per variabili normali con varianze note
compare_whole_series_with_known_vars <- function(data, country1, country2, variable, alpha = 0.05) {
  data_country1 <- data %>%
    filter(Country == country1 & Variable == variable & !is.na(Value)) %>%
    pull(Value)
  
  data_country2 <- data %>%
    filter(Country == country2 & Variable == variable & !is.na(Value)) %>%
    pull(Value)
  
  mean1 <- mean(data_country1)
  mean2 <- mean(data_country2)
  var1 <- var(data_country1)
  var2 <- var(data_country2)
  n1 <- length(data_country1)
  n2 <- length(data_country2)
  
  conf_interval <- confidence_interval_difference_means_known_vars(mean1, mean2, var1, var2, n1, n2, conf_level = 0.95)
  
  return(list(
    mean_country1 = mean1,
    mean_country2 = mean2,
    lower_bound = conf_interval[1],
    upper_bound = conf_interval[2]
  ))
}

# Esegui il confronto per ciascuna variabile selezionata tra Italia e Spagna
compare_whole_series_known_vars_results <- list()

for (variable in variabili_normali) {
  result <- compare_whole_series_with_known_vars(dataset_copy, country1, country2, variable)
  compare_whole_series_known_vars_results[[variable]] <- result
}

# Salvare i risultati del confronto in un file CSV nella cartella "inferenzastatistica"
compare_whole_series_known_vars_df <- do.call(rbind, lapply(compare_whole_series_known_vars_results, as.data.frame))
write.csv(compare_whole_series_known_vars_df, "inferenzastatistica/compare_whole_series_known_vars_results.csv", row.names = TRUE)

# Funzione per calcolare la statistica t, valore critico e p-value
calcola_statistica_test <- function(mean1, mean2, var1, var2, n1, n2, alpha) {
  t_statistic <- (mean1 - mean2) / sqrt(var1 / n1 + var2 / n2)
  critical_value <- qnorm(alpha, lower.tail = TRUE)
  pvalue <- pnorm(t_statistic, lower.tail = TRUE)
  
  return(list(t_statistic = t_statistic, critical_value = critical_value, pvalue = pvalue))
}

# Calcolo delle medie, varianze e dimensioni campionarie per ciascuna variabile
mean_italia_production_co2 <- mean(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Production-based CO2 emissions"], na.rm = TRUE)
mean_spagna_production_co2 <- mean(dataset$Value[dataset$Country == "Spagna" & dataset$Variable == "Production-based CO2 emissions"], na.rm = TRUE)
var_italia_production_co2 <- var(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Production-based CO2 emissions"], na.rm = TRUE)
var_spagna_production_co2 <- var(dataset$Value[dataset$Country == "Spagna" & dataset$Variable == "Production-based CO2 emissions"], na.rm = TRUE)
n_italia_production_co2 <- sum(!is.na(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Production-based CO2 emissions"]))
n_spagna_production_co2 <- sum(!is.na(dataset$Value[dataset$Country == "Spagna" & dataset$Variable == "Production-based CO2 emissions"]))

mean_italia_demand_co2 <- mean(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Demand-based CO2 emissions"], na.rm = TRUE)
mean_spagna_demand_co2 <- mean(dataset$Value[dataset$Country == "Spagna" & dataset$Variable == "Demand-based CO2 emissions"], na.rm = TRUE)
var_italia_demand_co2 <- var(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Demand-based CO2 emissions"], na.rm = TRUE)
var_spagna_demand_co2 <- var(dataset$Value[dataset$Country == "Spagna" & dataset$Variable == "Demand-based CO2 emissions"], na.rm = TRUE)
n_italia_demand_co2 <- sum(!is.na(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Demand-based CO2 emissions"]))
n_spagna_demand_co2 <- sum(!is.na(dataset$Value[dataset$Country == "Spagna" & dataset$Variable == "Demand-based CO2 emissions"]))

mean_italia_renewable <- mean(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Renewable electricity, % total electricity generation"], na.rm = TRUE)
mean_spagna_renewable <- mean(dataset$Value[dataset$Country == "Spagna" & dataset$Variable == "Renewable electricity, % total electricity generation"], na.rm = TRUE)
var_italia_renewable <- var(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Renewable electricity, % total electricity generation"], na.rm = TRUE)
var_spagna_renewable <- var(dataset$Value[dataset$Country == "Spagna" & dataset$Variable == "Renewable electricity, % total electricity generation"], na.rm = TRUE)
n_italia_renewable <- sum(!is.na(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Renewable electricity, % total electricity generation"]))
n_spagna_renewable <- sum(!is.na(dataset$Value[dataset$Country == "Spagna" & dataset$Variable == "Renewable electricity, % total electricity generation"]))

mean_italia_gdp <- mean(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Real GDP per capita"], na.rm = TRUE)
mean_spagna_gdp <- mean(dataset$Value[dataset$Country == "Spagna" & dataset$Variable == "Real GDP per capita"], na.rm = TRUE)
var_italia_gdp <- var(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Real GDP per capita"], na.rm = TRUE)
var_spagna_gdp <- var(dataset$Value[dataset$Country == "Spagna" & dataset$Variable == "Real GDP per capita"], na.rm = TRUE)
n_italia_gdp <- sum(!is.na(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Real GDP per capita"]))
n_spagna_gdp <- sum(!is.na(dataset$Value[dataset$Country == "Spagna" & dataset$Variable == "Real GDP per capita"]))

# Ora puoi usare questi valori per costruire il dataframe confronti
confronti <- data.frame(
  variabile = c("Production-based CO2 emissions", "Demand-based CO2 emissions",
                "Renewable electricity, % total electricity generation", "Real GDP per capita"),
  mean1 = c(mean_italia_production_co2, mean_italia_demand_co2, mean_italia_renewable, mean_italia_gdp),
  mean2 = c(mean_spagna_production_co2, mean_spagna_demand_co2, mean_spagna_renewable, mean_spagna_gdp),
  var1 = c(var_italia_production_co2, var_italia_demand_co2, var_italia_renewable, var_italia_gdp),
  var2 = c(var_spagna_production_co2, var_spagna_demand_co2, var_spagna_renewable, var_spagna_gdp),
  n1 = c(n_italia_production_co2, n_italia_demand_co2, n_italia_renewable, n_italia_gdp),
  n2 = c(n_spagna_production_co2, n_spagna_demand_co2, n_spagna_renewable, n_spagna_gdp),
  alpha = 0.05
)



# Calcola statistiche di test, valori critici e p-values per ogni variabile
for (i in 1:nrow(confronti)) {
  risultati <- calcola_statistica_test(confronti$mean1[i], confronti$mean2[i], 
                                       confronti$var1[i], confronti$var2[i], 
                                       confronti$n1[i], confronti$n2[i], confronti$alpha[i])
  confronti$t_statistic[i] <- risultati$t_statistic
  confronti$critical_value[i] <- risultati$critical_value
  confronti$pvalue[i] <- risultati$pvalue
}

# Creazione della cartella per salvare i grafici se non esiste già
dir.create("inferenzastatistica", showWarnings = FALSE)

### TEST PER LE EMISSIONI DI CO2 ###

# Valore ipotizzato per la media delle emissioni di CO2 (in milioni di tonnellate)
mu0_co2 <- 300

# Calcolo della media campionaria e della deviazione standard campionaria per l'Italia
mean_italia_co2 <- mean(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Production-based CO2 emissions"], na.rm = TRUE)
sd_italia_co2 <- sd(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Production-based CO2 emissions"], na.rm = TRUE)
n_italia_co2 <- sum(!is.na(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Production-based CO2 emissions"]))

# Statistica del test Z per un test unilaterale sinistro
z_statistic_co2 <- (mean_italia_co2 - mu0_co2) / (sd_italia_co2 / sqrt(n_italia_co2))

# Calcolo del p-value per un test unilaterale sinistro
p_value_co2 <- pnorm(z_statistic_co2)

# Livello di significatività
alpha <- 0.05
z_critico_co2 <- qnorm(alpha)

# Stampa dei risultati per il test sulle emissioni di CO2
cat("Statistica Z per le emissioni di CO2:", z_statistic_co2, "\n")
cat("p-value per le emissioni di CO2:", p_value_co2, "\n")

# Decisione
if (p_value_co2 < alpha) {
  decisione_co2 <- "Rifiutiamo l'ipotesi nulla"
} else {
  decisione_co2 <- "Non rifiutiamo l'ipotesi nulla"
}

print(decisione_co2)

# Creazione del grafico per il test sulle emissioni di CO2
png("inferenzastatistica/grafico_test_unilaterale_sinistro_co2.png", width = 800, height = 600)

curve(dnorm(x), from = -4, to = 4, col = "blue", lwd = 2, ylab = "Densità", xlab = "Z", main = "Test unilaterale sinistro (CO2)")
abline(v = z_critico_co2, col = "red", lty = 2)  # Linea verticale al valore critico

# Area di rifiuto
x_vals_co2 <- seq(-4, z_critico_co2, length = 100)
polygon(c(x_vals_co2, z_critico_co2), c(dnorm(x_vals_co2), 0), col = rgb(1, 0, 0, 0.5), border = NA)

# Aggiungere la statistica Z osservata
abline(v = z_statistic_co2, col = "green", lwd = 2)

# Etichette
text(z_statistic_co2, 0.2, paste("Z osservato =", round(z_statistic_co2, 2)), pos = 4, col = "green")
text(z_critico_co2, 0.05, expression(paste("Z", alpha)), pos = 4, col = "red")
text(-3, 0.1, "Regione di rifiuto", col = "red", pos = 4)
text(-1, 0.15, "Regione di accettazione", col = "blue", pos = 4)  # Posizione x = 1, y = 0.15

dev.off()

### TEST PER L'ENERGIA RINNOVABILE ###

# Valore ipotizzato per la percentuale di energia rinnovabile
mu0_renewable <- 50  # Ad esempio, 50%

# Calcolo della media campionaria e della deviazione standard campionaria per l'Italia
mean_italia_renewable <- mean(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Renewable electricity, % total electricity generation"], na.rm = TRUE)
sd_italia_renewable <- sd(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Renewable electricity, % total electricity generation"], na.rm = TRUE)
n_italia_renewable <- sum(!is.na(dataset$Value[dataset$Country == "Italia" & dataset$Variable == "Renewable electricity, % total electricity generation"]))

# Statistica del test Z per un test unilaterale destro
z_statistic_renewable <- (mean_italia_renewable - mu0_renewable) / (sd_italia_renewable / sqrt(n_italia_renewable))

# Calcolo del p-value per un test unilaterale destro
p_value_renewable <- 1 - pnorm(z_statistic_renewable)

# Livello di significatività
z_critico_renewable <- qnorm(1 - alpha)

# Stampa dei risultati per il test sull'energia rinnovabile
cat("Statistica Z per l'energia rinnovabile:", z_statistic_renewable, "\n")
cat("p-value per l'energia rinnovabile:", p_value_renewable, "\n")

# Decisione
if (p_value_renewable < alpha) {
  decisione_renewable <- "Rifiutiamo l'ipotesi nulla: la media della percentuale di energia rinnovabile è significativamente superiore a 50%."
} else {
  decisione_renewable <- "Non rifiutiamo l'ipotesi nulla: non ci sono prove sufficienti per dire che la media della percentuale di energia rinnovabile è superiore a 50%."
}

print(decisione_renewable)

# Creazione del grafico per il test sull'energia rinnovabile
png("inferenzastatistica/grafico_test_unilaterale_destro_renewable.png", width = 800, height = 600)

curve(dnorm(x), from = -4, to = 4, col = "blue", lwd = 2, ylab = "Densità", xlab = "Z", main = "Test unilaterale destro (Energia Rinnovabile)")
abline(v = z_critico_renewable, col = "red", lty = 2)  # Linea verticale al valore critico

# Area di rifiuto
x_vals_renewable <- seq(z_critico_renewable, 4, length = 100)
polygon(c(z_critico_renewable, x_vals_renewable), c(0, dnorm(x_vals_renewable)), col = rgb(1, 0, 0, 0.5), border = NA)

# Aggiungere la statistica Z osservata
abline(v = z_statistic_renewable, col = "green", lwd = 2)

# Etichette
text(z_statistic_renewable, 0.2, paste("Z osservato =", round(z_statistic_renewable, 2)), pos = 4, col = "green")
text(z_critico_renewable, 0.05, expression(paste("Z", alpha)), pos = 4, col = "red")
text(2.5, 0.1, "Regione di rifiuto", col = "red", pos = 4)
text(-1, 0.15, "Regione di accettazione", col = "blue", pos = 4)  # Posizione x = -1, y = 0.15

dev.off()


