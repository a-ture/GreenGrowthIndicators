# Loop attraverso le variabili
for (variabile in variabili){
  dataset_test <- filter(dataset, Variable == variabile)
  print(variabile)

  # Trasformazione logaritmica con il logaritmo naturale
  dati_trasformati <- log(dataset_test$Value)
  
  # Stampare il risultato del test
  print(risultato_test_shapiro)
  
  # Esegui il test di Shapiro-Wilk sulla variabile "Environmentally related taxes, % GDP"
  risultato_test_shapiro <- shapiro.test(dataset_test$Value)
  
  # Verifica se i dati seguono una distribuzione normale
  if (risultato_test_shapiro$p.value > 0.05) {
    cat("Il test di Shapiro-Wilk indica che la variabile segue una distribuzione normale (non possiamo rigettare l'ipotesi nulla).\n")
  } else {
    cat("Il test di Shapiro-Wilk indica che la variabile non segue una distribuzione normale (rigettiamo l'ipotesi nulla).\n")
  }
  
  library(fitdistrplus)
  normal_dist <- fitdist(dataset_test$Value, "norm")
  plot(normal_dist)

}

dataset_test <- filter(dataset, Variable == "Environmentally related taxes, % GDP")

# Istogramma
hist(dataset_test$Value, breaks = 20, main = "Istogramma - Environmentally related taxes, % GDP", xlab = "Valore")

# Grafico QQ-Plot
qqnorm(dataset_test$Value, main = "Grafico QQ-Plot - Environmentally related taxes, % GDP")
qqline(dataset_test$Value)


library(fitdistrplus)

# Fit della distribuzione, ad esempio, normale
fit_norm <- fitdist(dataset_test$Value, "norm")

# Visualizza il risultato del fit
summary(fit_norm)
plot(fit_norm)

fit_t <- fitdist(dataset_test$Value, "t")
fit_lnorm <- fitdist(dataset_test$Value, "lnorm")


