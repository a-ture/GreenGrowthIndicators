library(dplyr)

# Funzione per applicare il metodo dei momenti
method_of_moments <- function(data) {
  sample_mean <- mean(data$Value)
  sample_variance <- var(data$Value)
  
  estimated_mu <- sample_mean
  estimated_sigma_squared <- sample_variance
  
  return(list(mu = estimated_mu, sigma_squared = estimated_sigma_squared))
}

# Confidence interval functions
confidence_interval_mean_var_known <- function(data, sigma, conf_level = 0.95) {
  n <- length(data)
  sample_mean <- mean(data)
  z_crit <- qnorm(1 - (1 - conf_level) / 2)
  margin_of_error <- z_crit * (sigma / sqrt(n))
  return(c(sample_mean - margin_of_error, sample_mean + margin_of_error))
}

confidence_interval_mean_var_unknown <- function(data, conf_level = 0.95) {
  n <- length(data)
  sample_mean <- mean(data)
  sample_sd <- sd(data)
  t_crit <- qt(1 - (1 - conf_level) / 2, df = n - 1)
  margin_of_error <- t_crit * (sample_sd / sqrt(n))
  return(c(sample_mean - margin_of_error, sample_mean + margin_of_error))
}

confidence_interval_variance_mean_known <- function(data, mu, conf_level = 0.95) {
  n <- length(data)
  sum_squared_diffs <- sum((data - mu)^2)
  chi2_lower <- qchisq((1 - conf_level) / 2, df = n)
  chi2_upper <- qchisq(1 - (1 - conf_level) / 2, df = n)
  return(c(sum_squared_diffs / chi2_upper, sum_squared_diffs / chi2_lower))
}

confidence_interval_variance_mean_unknown <- function(data, conf_level = 0.95) {
  n <- length(data)
  sample_var <- var(data)
  chi2_lower <- qchisq((1 - conf_level) / 2, df = n - 1)
  chi2_upper <- qchisq(1 - (1 - conf_level) / 2, df = n - 1)
  return(c((n - 1) * sample_var / chi2_upper, (n - 1) * sample_var / chi2_lower))
}

# Definisci l'elenco dei paesi e variabili da analizzare
paesi_selezionati <- c("Italia")
variabili_selezionate <- c(
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

library(dplyr)

# Funzione per applicare il test del chi-quadrato con suddivisione in 4 intervalli
chi_square_test_custom <- function(data) {
  sample_mean <- mean(data$Value)
  sample_sd <- sd(data$Value)
  
  # Calcolo dei limiti degli intervalli usando i quantili della distribuzione normale
  breaks <- qnorm(seq(0, 1, length.out = 5), mean = sample_mean, sd = sample_sd)
  
  # Stampa i limiti degli intervalli
  cat("Limiti degli intervalli per la variabile:", unique(data$Variable), "\n")
  cat("Limite sinistro: ", breaks[1], "\n")
  for (i in 2:length(breaks)) {
    cat("Intervallo I", i-1, ": [", breaks[i-1], ", ", breaks[i], ") \n", sep = "")
  }
  cat("\n")
  
  # Suddivisione dei dati nei 4 intervalli
  categories <- cut(data$Value, breaks = breaks, include.lowest = TRUE, right = FALSE)
  
  # Calcolo delle frequenze osservate
  observed_frequencies <- table(categories)
  
  # Stampa delle frequenze osservate
  cat("Frequenze osservate in ciascun intervallo:\n")
  print(observed_frequencies)
  cat("\n")
  
  # Calcolo delle frequenze attese (tutti gli intervalli dovrebbero avere uguale frequenza)
  expected_frequencies <- rep(nrow(data) / length(observed_frequencies), length(observed_frequencies))
  
  # Esegui il test del chi-quadrato se le frequenze attese sono >= 5
  if (all(expected_frequencies >= 5)) {
    chi2_test <- chisq.test(observed_frequencies, p = expected_frequencies / sum(expected_frequencies))
    
    # Estrai e stampa il valore del chi-quadrato osservato
    chi2_value <- chi2_test$statistic
    cat("Valore del Chi-quadrato osservato (χ²):", chi2_value, "\n")
    
    # Calcolo dei limiti critici per il chi-quadrato
    df <- length(observed_frequencies) - 3  # gradi di libertà r - k - 1 = 4 - 2 - 1 = 1
    chi2_crit_upper <- qchisq(0.975, df)  # χ²(α/2, df)
    chi2_crit_lower <- qchisq(0.025, df)  # χ²(1-α/2, df)
    cat("Valori critici del Chi-quadrato: χ²(α/2, df) =", chi2_crit_upper, ", χ²(1-α/2, df) =", chi2_crit_lower, "\n")
    
    # Confronto per l'accettazione dell'ipotesi H0
    if (chi2_value > chi2_crit_lower && chi2_value < chi2_crit_upper) {
      cat("L'ipotesi H0 può essere accettata: la popolazione segue una distribuzione normale.\n")
      normal_distribution <- TRUE
    } else {
      cat("L'ipotesi H0 viene rifiutata: la popolazione non segue una distribuzione normale.\n")
      normal_distribution <- FALSE
    }
    
    return(list(p_value = chi2_test$p.value, chi2_value = chi2_value, chi2_crit_upper = chi2_crit_upper, chi2_crit_lower = chi2_crit_lower, normal_distribution = normal_distribution))
  } else {
    warning("Frequenze attese troppo basse per il test del chi-quadrato")
    return(list(p_value = NA, chi2_value = NA, chi2_crit_upper = NA, chi2_crit_lower = NA, normal_distribution = NA))
  }
}

# Funzione per calcolare l'intervallo di confidenza usando il metodo pivotale
confidence_interval_pivot <- function(data, sigma, conf_level = 0.95) {
  n <- length(data)
  sample_mean <- mean(data)
  
  # Quantili critici
  alpha <- 1 - conf_level
  z_crit <- qnorm(1 - alpha/2)  # Es. per un intervallo di confidenza del 95%, z_crit ≈ 1.96
  
  # Intervallo di confidenza
  margin_of_error <- z_crit * (sigma / sqrt(n))
  lower_bound <- sample_mean - margin_of_error
  upper_bound <- sample_mean + margin_of_error
  
  return(c(lower_bound, upper_bound))
}

# Esempio di applicazione nel ciclo per ogni variabile e paese
results <- list()
moments_results <- list()
confidence_intervals <- list()

for (country in paesi_selezionati) {
  for (variable in variabili_selezionate) {
    data_subset <- dataset %>%
      filter(Country == country & Variable == variable & !is.na(Value))
    
    # Calcola i parametri iniziali per questa variabile
    sample_mean_initial <- mean(data_subset$Value)
    sample_sd_initial <- sd(data_subset$Value)
    sample_size_initial <- nrow(data_subset)
    
    cat("Parametri iniziali per", country, "-", variable, ":\n")
    cat("Media campionaria:", sample_mean_initial, "\n")
    cat("Deviazione Standard Campionaria:", sample_sd_initial, "\n")
    cat("Dimensione del Campione:", sample_size_initial, "\n\n")
    
    if (nrow(data_subset) > 10) {
      # Calcola i limiti degli intervalli e il valore del Chi-quadrato
      chi2_result <- chi_square_test_custom(data_subset)
      
      # Salva sia il P-Value che il valore del chi-quadrato e i limiti critici
      results[[paste(country, variable, sep = " - ")]] <- c(chi2_result$p_value, chi2_result$chi2_value, chi2_result$chi2_crit_upper, chi2_result$chi2_crit_lower)
      
      # Applica il metodo dei momenti solo se la distribuzione è normale
      if (chi2_result$normal_distribution) {
        moments_result <- method_of_moments(data_subset)
        moments_results[[paste(country, variable, sep = " - ")]] <- moments_result
        
        # Calcola l'intervallo di confidenza usando il metodo pivotale
        conf_interval <- confidence_interval_pivot(data_subset$Value, sigma = sample_sd_initial)
        confidence_intervals[[paste(country, variable, sep = " - ")]] <- conf_interval
        
      } else {
        moments_results[[paste(country, variable, sep = " - ")]] <- NA
        confidence_intervals[[paste(country, variable, sep = " - ")]] <- NA
      }
    }
  }
}

# Salva i risultati del test del chi-quadrato in un file CSV
results_df <- do.call(rbind, lapply(names(results), function(name) {
  data.frame(Country_Variable = name, P_Value = results[[name]][1], Chi_Square_Value = results[[name]][2], Chi2_Crit_Upper = results[[name]][3], Chi2_Crit_Lower = results[[name]][4])
}))

write.csv(results_df, "chi_square_results.csv", row.names = FALSE)

# Salva i risultati del metodo dei momenti in un file CSV solo per le variabili con distribuzione normale
moments_df <- do.call(rbind, lapply(names(moments_results), function(name) {
  if (is.list(moments_results[[name]]) && !is.null(moments_results[[name]]$mu) && !is.na(moments_results[[name]]$mu)) {
    data.frame(Country_Variable = name,
               Mu = moments_results[[name]]$mu,
               Sigma_Squared = moments_results[[name]]$sigma_squared)
  } else {
    NULL
  }
}))

write.csv(moments_df, "moments_results.csv", row.names = FALSE)

confidence_interval_mean_var_known <- function(data, sigma, conf_level = 0.95) {
  n <- length(data)
  sample_mean <- mean(data)
  alpha <- 1 - conf_level
  z_crit <- qnorm(1 - alpha/2)
  
  margin_of_error <- z_crit * (sigma / sqrt(n))
  lower_bound <- sample_mean - margin_of_error
  upper_bound <- sample_mean + margin_of_error
  
  return(c(lower_bound, upper_bound))
}

confidence_interval_mean_var_unknown <- function(data, conf_level = 0.95) {
  n <- length(data)
  sample_mean <- mean(data)
  sample_sd <- sd(data)
  alpha <- 1 - conf_level
  t_crit <- qt(1 - alpha/2, df = n - 1)
  
  margin_of_error <- t_crit * (sample_sd / sqrt(n))
  lower_bound <- sample_mean - margin_of_error
  upper_bound <- sample_mean + margin_of_error
  
  return(c(lower_bound, upper_bound))
}

confidence_interval_variance_mean_known <- function(data, mu, conf_level = 0.95) {
  n <- length(data)
  squared_diffs <- (data - mu)^2
  sum_squared_diffs <- sum(squared_diffs)
  
  alpha <- 1 - conf_level
  chi2_lower <- qchisq(alpha/2, df = n)
  chi2_upper <- qchisq(1 - alpha/2, df = n)
  
  lower_bound <- sum_squared_diffs / chi2_upper
  upper_bound <- sum_squared_diffs / chi2_lower
  
  return(c(lower_bound, upper_bound))
}

confidence_interval_variance_mean_unknown <- function(data, conf_level = 0.95) {
  n <- length(data)
  sample_var <- var(data)
  
  alpha <- 1 - conf_level
  chi2_lower <- qchisq(alpha/2, df = n - 1)
  chi2_upper <- qchisq(1 - alpha/2, df = n - 1)
  
  lower_bound <- (n - 1) * sample_var / chi2_upper
  upper_bound <- (n - 1) * sample_var / chi2_lower
  
  return(c(lower_bound, upper_bound))
}


results <- list()
moments_results <- list()
confidence_intervals <- list()

for (country in paesi_selezionati) {
  for (variable in variabili_selezionate) {
    data_subset <- dataset %>%
      filter(Country == country & Variable == variable & !is.na(Value))
    
    if (nrow(data_subset) > 10) {
      chi2_result <- chi_square_test_custom(data_subset)
      
      if (chi2_result$normal_distribution) {
        # Calcola gli intervalli di confidenza per tutti i casi
        sigma_known <- sd(data_subset$Value)  # supponendo che la varianza sia nota
        mu_known <- mean(data_subset$Value)   # supponendo che la media sia nota
        
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

# Salva i risultati in un file CSV
conf_intervals_df <- do.call(rbind, lapply(names(confidence_intervals), function(name) {
  data.frame(Country_Variable = name,
             Lower_Bound = confidence_intervals[[name]][1],
             Upper_Bound = confidence_intervals[[name]][2])
}))

write.csv(conf_intervals_df, "confidence_intervals_results.csv", row.names = FALSE)



# Creare la colonna Group per indicare i dati prima e dopo il 2010
dataset <- dataset %>%
  mutate(Group = ifelse(YEA < 2010, "Before 2010", "After 2010"))

# Funzione per il confronto tra due popolazioni
compare_two_populations <- function(data, group1, group2, variable) {
  # Filtra i dati per i due gruppi
  data_group1 <- data %>% filter(Variable == variable & Group == group1)
  data_group2 <- data %>% filter(Variable == variable & Group == group2)
  
  # Verifica che entrambe le popolazioni abbiano abbastanza dati
  if (nrow(data_group1) > 1 & nrow(data_group2) > 1) {
    # Esegui il t-test per confrontare le due popolazioni
    t_test_result <- t.test(data_group1$Value, data_group2$Value, var.equal = TRUE)
    
    # Estrai i risultati del t-test
    mean_group1 <- mean(data_group1$Value)
    mean_group2 <- mean(data_group2$Value)
    p_value <- t_test_result$p.value
    confidence_interval <- t_test_result$conf.int
    
    # Stampa i risultati
    cat("Confronto tra", group1, "e", group2, "per la variabile", variable, ":\n")
    cat("Media di", group1, ":", mean_group1, "\n")
    cat("Media di", group2, ":", mean_group2, "\n")
    cat("P-value:", p_value, "\n")
    cat("Intervallo di confidenza al 95% per la differenza delle medie:", confidence_interval, "\n")
    cat("\n")
    
    # Restituisci i risultati in una lista
    return(list(mean_group1 = mean_group1, mean_group2 = mean_group2, p_value = p_value, confidence_interval = confidence_interval))
  } else {
    warning("Uno dei gruppi ha meno di 2 osservazioni.")
    return(NULL)
  }
}

library(dplyr)

# Function to create a Group column based on a year threshold
create_group_column <- function(data, year_threshold, new_column_name = "Group", year_column_name = "YEA") {
  data %>%
    mutate(!!new_column_name := ifelse(.data[[year_column_name]] < year_threshold, 
                                       paste("Before", year_threshold), 
                                       paste("After", year_threshold)))
}

# Function for comparing two populations
compare_two_populations <- function(data, group1, group2, variable) {
  data_group1 <- data %>% filter(Variable == variable & Group == group1)
  data_group2 <- data %>% filter(Variable == variable & Group == group2)
  
  if (nrow(data_group1) > 1 & nrow(data_group2) > 1) {
    t_test_result <- t.test(data_group1$Value, data_group2$Value, var.equal = TRUE)
    mean_group1 <- mean(data_group1$Value)
    mean_group2 <- mean(data_group2$Value)
    p_value <- t_test_result$p.value
    confidence_interval <- t_test_result$conf.int
    
    # Print the results
    cat("Comparison between", group1, "and", group2, "for variable", variable, ":\n")
    cat("Mean of", group1, ":", mean_group1, "\n")
    cat("Mean of", group2, ":", mean_group2, "\n")
    cat("P-value:", p_value, "\n")
    cat("95% Confidence Interval for the difference in means:", confidence_interval, "\n")
    cat("\n")
    
    # Return results in a list
    return(list(mean_group1 = mean_group1, mean_group2 = mean_group2, p_value = p_value, confidence_interval = confidence_interval))
  } else {
    warning("One of the groups has fewer than 2 observations.")
    return(NULL)
  }
}

# Function to perform and save comparison results
perform_comparison_and_save <- function(data, variable_of_interest, year_threshold, filename) {
  # Make a copy of the dataset to avoid modifying the original
  data_copy <- data
  
  # Create Group column based on the given year threshold in the copied dataset
  data_copy <- create_group_column(data_copy, year_threshold)
  
  # Define group names based on year threshold
  group1 <- paste("Before", year_threshold)
  group2 <- paste("After", year_threshold)
  
  # Perform comparison between the two groups for the variable of interest
  comparison_result <- compare_two_populations(data_copy, group1, group2, variable_of_interest)
  
  # Save the results to a CSV file if the comparison was successful
  if (!is.null(comparison_result)) {
    comparison_results_df <- data.frame(
      Variable = variable_of_interest,
      Mean_Group1 = comparison_result$mean_group1,
      Mean_Group2 = comparison_result$mean_group2,
      P_Value = comparison_result$p_value,
      Lower_CI = comparison_result$confidence_interval[1],
      Upper_CI = comparison_result$confidence_interval[2]
    )
    
    write.csv(comparison_results_df, filename, row.names = FALSE)
  }
}

# Example usage: Compare renewable energy supply before and after 2010
perform_comparison_and_save(dataset, 
                            variable_of_interest = "Renewable energy supply, % total energy supply", 
                            year_threshold = 2010, 
                            filename = "renewable_energy_comparison_results.csv")

# Example usage: Compare production-based CO2 emissions before and after 2008
perform_comparison_and_save(dataset, 
                            variable_of_interest = "Production-based CO2 emissions", 
                            year_threshold = 2008, 
                            filename = "co2_emissions_comparison_results.csv")


# Example usage: Compare environmentally related taxes before and after 2015
perform_comparison_and_save(dataset, 
                            variable_of_interest = "Environmentally related taxes, % GDP", 
                            year_threshold = 2015, 
                            filename = "environmental_taxes_comparison_results.csv")

