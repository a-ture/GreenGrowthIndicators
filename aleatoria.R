# Creare una copia del dataset originale
dataset_copy <- dataset

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
chi_square_test_custom <- function(data, breaks) {
  # Assicurarsi che i dati siano numerici
  if (!is.numeric(data)) {
    stop("'data' deve essere numerico")
  }
  
  categories <- cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE)
  
  observed_frequencies <- table(categories)
  expected_frequencies <- rep(length(data) / length(observed_frequencies), length(observed_frequencies))
  
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
      chi2_result_4 <- chi_square_test_custom(data_subset$Value, breaks = breaks_4)
      
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
  
  result_group1 <- if (length(data_group1) > 10) chi_square_test_custom(data_group1, calculate_intervals(data_group1, num_intervals)) else NULL
  result_group2 <- if (length(data_group2) > 10) chi_square_test_custom(data_group2, calculate_intervals(data_group2, num_intervals)) else NULL
  
  return(list(group1 = result_group1, group2 = result_group2))
}

# Esempio di confronto tra due gruppi temporali
compare_normality_results <- compare_normality(dataset_copy, 1990, 2000, "Real GDP per capita")
compare_normality_df <- do.call(rbind, lapply(compare_normality_results, as.data.frame))
write.csv(compare_normality_df, "inferenzastatistica/compare_normality_results.csv", row.names = TRUE)

# Calcolo degli intervalli di confidenza
confidence_intervals <- list()

for (country in paesi_selezionati) {
  for (variable in variabili_selezionate) {
    data_subset <- dataset %>%
      filter(Country == country & Variable == variable & !is.na(Value))
    
    if (nrow(data_subset) > 10) {
      chi2_result <- chi_square_test_custom(data_subset$Value, calculate_intervals(data_subset$Value, 4))
      
      if (chi2_result$normal_distribution) {
        sigma_known <- sd(data_subset$Value, na.rm = TRUE)
        mu_known <- mean(data_subset$Value, na.rm = TRUE)
        
        conf_interval_i <- confidence_interval_mean_var_known(data_subset$Value, sigma = sigma_known)
        conf_interval_ii <- confidence_interval_mean_var_unknown(data_subset$Value)
        conf_interval_iii <- confidence_interval_variance_mean_known(data_subset$Value, mu = mu_known)
        conf_interval_iv <- confidence_interval_variance_mean_unknown(data_subset$Value)
        
        confidence_intervals[[paste(country, variable, "Mean_Var_Known", sep = " - ")]] <- conf_interval_i
        confidence_intervals[[paste(country, variable, "Mean_Var_Unknown", sep = " - ")]] <- conf_interval_ii
        confidence_intervals[[paste(country, variable, "Var_Mean_Known", sep = " - ")]] <- conf_interval_iii
        confidence_intervals[[paste(country, variable, "Var_Mean_Unknown", sep = " - ")]] <- conf_interval_iv
      }
    }
  }
}

# Salvare gli intervalli di confidenza in un file CSV nella cartella "inferenzastatistica"
confidence_intervals_df <- do.call(rbind, lapply(confidence_intervals, function(x) t(as.data.frame(x))))
write.csv(confidence_intervals_df, "inferenzastatistica/confidence_intervals.csv", row.names = TRUE)

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

# Funzione per generare il grafico della distribuzione di Student
crea_grafico_student <- function(t_statistic, critical_value, pvalue, n, file_name) {
  file_name <- gsub(" ", "_", file_name) 
  file_name <- gsub("[^A-Za-z0-9_]", "", file_name) 
  file_name <- paste0(file_name, ".png")
  file_path <- file.path("inferenzastatistica", file_name)
  
  png(file_path, width = 800, height = 600)
  
  curve(dt(x, df = n - 1), from = -3, to = 15, axes = FALSE,
        ylim = c(0, 0.5), xlab = "", ylab = "", 
        main = paste("Densità di Student -", file_name))
  
  text(0, 0.45, "Regione di\naccettazione", cex = 0.8)
  
  axis(1, at = c(-3, critical_value, 0, t_statistic, 15), 
       labels = c("", round(critical_value, 6), "", round(t_statistic, 2), ""))
  
  vals <- seq(-3, critical_value, length = 100)
  polygon(c(-3, vals, critical_value), c(0, dt(vals, df = n - 1), 0), 
          density = 20, angle = 45, col = "red")
  
  abline(h = 0)
  
  text(critical_value - 0.5, 0.05, expression(alpha), cex = 0.8)
  text(critical_value - 1, 0.1, "Regione di\nrifiuto", cex = 0.8)
  
  text(0, 0.1, expression(1 - alpha), cex = 0.8)
  
  box()
  
  dev.off()
}

# Itera su ogni variabile e crea il grafico corrispondente
for (i in 1:nrow(confronti)) {
  file_name <- paste0("grafico_densita_student_", gsub(" ", "_", confronti$variabile[i]))
  
  crea_grafico_student(confronti$t_statistic[i], confronti$critical_value[i], 
                       confronti$pvalue[i], n, file_name)
}

