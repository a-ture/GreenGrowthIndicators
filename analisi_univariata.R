library(ggplot2)
library(dplyr)
library(scales)
library(e1071)
library(rnaturalearth)
library(rnaturalearthdata)
library(psych)

# Carica le librerie necessarie
library(dplyr)
library(ggplot2)



# Imposta la directory di destinazione
directory_destinazione <- "EmissioniCO2"

# Assicurati che la sottodirectory esista, altrimenti creala
if (!file.exists(directory_destinazione)) {
  dir.create(directory_destinazione)
}

# Specifica il nome della sotto-cartella
sotto_cartella <- "ProductionBasedCO2Emissions"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)
if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

# Filtra il dataset per la variabile Demand-based CO2 emissions
dataset_co2_prod <-
  filter(dataset, Variable == "Production-based CO2 emissions")

# Crea l'istogramma per "Production-based CO2 emissions"
histogram_co2_prod <- ggplot(dataset_co2_prod, aes(x = Value)) +
  geom_histogram(
    binwidth = 100,
    fill = "#A8CF97",
    color = "black",
    alpha = 0.7
  ) +
  ggtitle(bquote("Istogramma delle Emissioni di CO"[2] * " basate sulla produzione")) +
  labs(x = expression("Produttività basata sulla CO"[2] * " prodotta (USD/kg)"),
       y = "Frequenza") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
    margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva l'istogramma in un file PDF
ggsave(
  "istogramma_produzione_co2.pdf",
  histogram_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
ecdf_co2_prod <- ggplot(dataset_co2_prod, aes(x = Value)) +
  stat_ecdf(geom = "step",
            color = "#A8CF97",
            size = 1) +
  ggtitle(bquote(
    "Funzione di Distribuzione Empirica - Emissioni CO"[2] * " basate sulla produzione"
  )) +
  labs(x = "Emissioni di CO2",
       y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
    margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva la funzione di distribuzione empirica in un file PDF
ggsave(
  "ecdf_produzione_co2.pdf",
  ecdf_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Funzione di densità di probabilità
densita_probabilita <- ggplot(dataset_co2_prod, aes(x = Value)) +
  geom_density(fill = "#A8CF97", alpha = 0.7) +
  labs(title = "Funzione di Densità di Probabilità",
       x = "Valore",
       y = "Densità")  + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
    margin = margin(b = 25, t = 15))
  )

# Salva il grafico della funzione di densità di probabilità in un file PDF
ggsave(
  "densita_probabilita_produzione.pdf",
  densita_probabilita,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Calcolo delle statistiche descrittive
statistiche_descrittive <- dataset_co2_prod %>%
  summarise(
    Media = mean(Value, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Deviazione_Standard = sd(Value, na.rm = TRUE),
    Minimo = min(Value, na.rm = TRUE),
    Massimo = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Calcolo di altri indici
frequenza_assoluta <- sum(!is.na(dataset_co2_prod$Value))
frequenza_relativa <- frequenza_assoluta / nrow(dataset_co2_prod)
simmetria <- skewness(dataset_co2_prod$Value)
curtosi <- kurtosis(dataset_co2_prod$Value)
deviazione_standard <- sd(dataset_co2_prod$Value, na.rm = TRUE)
coefficiente_di_variazione <-
  deviazione_standard / mean(dataset_co2_prod$Value, na.rm = TRUE)
varianza <- var(dataset_co2_prod$Value, na.rm = TRUE)

# Stampa delle statistiche
print("Production-based CO2 emissions")
print(statistiche_descrittive)
cat("Frequenza assoluta:", frequenza_assoluta, "\n")
cat("Frequenza relativa:", frequenza_relativa, "\n")
cat("Simmetria:", simmetria, "\n")
cat("Curtosi:", curtosi, "\n")
cat("Deviazione standard:", deviazione_standard, "\n")
cat("Coefficienti di variazione:",
    coefficiente_di_variazione,
    "\n")
cat("Varianza:", varianza, "\n")

# Crea un box plot per ciascuna variabile
grafico <-
  ggplot(dataset_co2_prod, aes(x = Variable, y = Value)) +
  ggtitle(bquote(
    "Boxplot per la variabile: Emissione di CO"[2] * " basate sulla produzione"
  )) +
  geom_boxplot(
    show.legend = FALSE,
    color = "black",
    fill = "#A8CF97",
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.size=3,
    outlier.color = "#00aedb"
  ) +
  scale_y_log10()+
  labs(x = "Variabile",
       y = expression("Emissioni di  CO"[2] * " in scala logaritmica")) +
  theme_linedraw(base_size = 14) + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(
      face = 'bold',
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    )
  )

# Visualizza il grafico
ggsave(
  "boxplot_produzione.pdf",
  grafico,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

for (cont in names(mappatura_continenti)) {
  dati_continente <- dataset_co2_prod %>%
    filter(Country %in% mappatura_continenti[[cont]]) %>%
    arrange(YEA)
  # Se ci sono dati, crea un grafico per il continente
  if (nrow(dati_continente) > 0) {
    nome_file <- paste0("grafico_produzione_", cont, ".pdf")
    # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
    X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
    X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Calcola il minimo e il massimo valore per l'asse y
    Y_MIN <- 0
    Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
    
    # Crea un grafico per "Production-based CO2 emissions" con ggplot
    grafico_co2_prod <-
      ggplot(dati_continente,
             aes(
               x = YEA,
               y = Value,
               group = Country,
               color = Country
             )) +
      geom_line(alpha = 0.8,
                size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
      geom_point(
        aes(group = Country),
        size = 2,
        shape = 16,
        fill = "white",
        show.legend = FALSE
      ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
      scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
      ggtitle(bquote("Emissioni di CO"[2] * " basate sulla produzione in" ~ .(cont))) +
      theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
      scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x
      scale_y_log10() +  # Imposta i punti di interruzione per l'asse y
      labs(
        color = "Paesi",
        x = "Anno",
        # Etichetta dell'asse x
        y = expression("Produttività basata sulla CO"[2] * " prodotta (USD/kg) in scala logaritmica")
      ) +  # Etichetta dell'asse y con notazione CO2
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        # Imposta la posizione della legenda e le dimensioni della chiave
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Imposta l'angolo e l'allineamento del testo sull'asse x
        axis.title.x = element_text(margin = margin(t = 30)),
        # Imposta il margine per l'etichetta dell'asse x
        axis.title.y = element_text(margin = margin(r = 30)),
        # Imposta il margine per l'etichetta dell'asse y
        plot.title = element_text(
          face = 'bold',
          hjust = 0.5,
          margin = margin(b = 25, t = 15)
        )  # Imposta l'allineamento e il margine per il titolo del grafico
      )
    
    # Salva il grafico
    ggsave(
      nome_file,
      grafico_co2_prod,
      path = percorso_sotto_cartella,
      device = "pdf",
      width = 29.7,
      height = 21,
      units = "cm"
    )
  } else {
    print(paste("Nessun dato disponibile per il continente:", cont))
  }
}

# Calcola la media delle emissioni per ogni paese
media_emissioni_per_paese <- dataset_co2_prod %>%
  group_by(Country) %>%
  summarise(MediaEmissioni = mean(Value, na.rm = TRUE))

# Ordina il dataset in base alla media delle emissioni in ordine decrescente
media_emissioni_per_paese <- media_emissioni_per_paese %>%
  arrange(desc(MediaEmissioni))

# Crea un grafico a barre
grafico_barre <-
  ggplot(media_emissioni_per_paese,
         aes(
           x = reorder(Country, MediaEmissioni),
           y = MediaEmissioni,
           fill = Country
         )) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +
  theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
  ggtitle(bquote("Emissioni di CO"[2] * " basate sulla produzione ")) +
  labs(
    fill = "Paesi",
    x = "Paesi",
    # Etichetta dell'asse x
    y = expression("Produttività basata sulla CO"[2] * " prodotta (USD/kg), scala logaritmica")
  ) +
  scale_y_log10() +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta la posizione della legenda e le dimensioni della chiave
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(
      face = 'bold',
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    )  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Visualizza il grafico
ggsave(
  "grafico_a_barre.pdf",
  grafico_barre,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Specifica il nome della sotto-cartella
sotto_cartella <- "Demand-basedCO2emissions"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)
if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

# Filtra il dataset per la variabile Demand-based CO2 emissions
dataset_co2_prod <-
  filter(dataset, Variable == "Demand-based CO2 emissions")

# Crea l'istogramma per "Production-based CO2 emissions"
histogram_co2_prod <- ggplot(dataset_co2_prod, aes(x = Value)) +
  geom_histogram(
    binwidth = 100,
    fill = "#A8CF97",
    color = "black",
    alpha = 0.7
  ) +
  ggtitle(bquote("Istogramma delle Emissioni di CO"[2] * " basate sulla domanda")) +
  labs(x = expression("Domanda basata sulla CO"[2] * " prodotta (USD/kg)"),
       y = "Frequenza") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
    margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva l'istogramma in un file PDF
ggsave(
  "istogramma_domanda_co2.pdf",
  histogram_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
ecdf_co2_prod <- ggplot(dataset_co2_prod, aes(x = Value)) +
  stat_ecdf(geom = "step",
            color = "#A8CF97",
            size = 1) +
  ggtitle(bquote(
    "Funzione di Distribuzione Empirica - Emissioni CO"[2] * " basate sulla domanda"
  )) +
  labs(x = "Emissioni di CO2",
       y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
    margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )


# Salva la funzione di distribuzione empirica in un file PDF
ggsave(
  "ecdf_produzione_co2.pdf",
  ecdf_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Funzione di densità di probabilità
densita_probabilita <- ggplot(dataset_co2_prod, aes(x = Value)) +
  geom_density(fill = "#A8CF97", alpha = 0.7) +
  labs(title = "Funzione di Densità di Probabilità",
       x = "Valore",
       y = "Densità") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )

# Salva il grafico della funzione di densità di probabilità in un file PDF
ggsave(
  "densita_probabilita_domanda.pdf",
  densita_probabilita,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)


# Calcolo delle statistiche descrittive
statistiche_descrittive <- dataset_co2_prod %>%
  summarise(
    Media = mean(Value, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Deviazione_Standard = sd(Value, na.rm = TRUE),
    Minimo = min(Value, na.rm = TRUE),
    Massimo = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Calcolo di altri indici
frequenza_assoluta <- sum(!is.na(dataset_co2_prod$Value))
frequenza_relativa <- frequenza_assoluta / nrow(dataset_co2_prod)
simmetria <- skewness(dataset_co2_prod$Value)
curtosi <- kurtosis(dataset_co2_prod$Value)
deviazione_standard <- sd(dataset_co2_prod$Value, na.rm = TRUE)
coefficiente_di_variazione <-
  deviazione_standard / mean(dataset_co2_prod$Value, na.rm = TRUE)
varianza <- var(dataset_co2_prod$Value, na.rm = TRUE)

# Stampa delle statistiche
print("Demand-based CO2 emissions")
print(statistiche_descrittive)
cat("Frequenza assoluta:", frequenza_assoluta, "\n")
cat("Frequenza relativa:", frequenza_relativa, "\n")
cat("Simmetria:", simmetria, "\n")
cat("Curtosi:", curtosi, "\n")
cat("Deviazione standard:", deviazione_standard, "\n")
cat("Coefficienti di variazione:",
    coefficiente_di_variazione,
    "\n")
cat("Varianza:", varianza, "\n")

# Crea un box plot per ciascuna variabile
grafico <-
  ggplot(dataset_co2_prod, aes(x = Variable, y = Value, fill = Variable)) +
  ggtitle(bquote(
    "Boxplot per la variabile: Emissione di CO"[2] * " basate sulla domanda"
  )) +
  geom_boxplot(
    show.legend = FALSE,
    color = "black",
    fill = "#A8CF97",
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.size=3,
    outlier.color = "#00aedb"
  ) +
  scale_y_log10() +
  labs(x = "Variabile",
       y = expression("Emissioni di  CO"[2] * "in scala logartimica")) +
  theme_linedraw(base_size = 14) + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(
      face = 'bold',
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    )
  )

# Visualizza il grafico
ggsave(
  "boxplot_domanda.pdf",
  grafico,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Per ogni continente, crea un grafico
for (cont in names(mappatura_continenti)) {
  dati_continente <- dataset_co2_prod %>%
    filter(Country %in% mappatura_continenti[[cont]]) %>%
    arrange(YEA)
  # Se ci sono dati, crea un grafico per il continente
  if (nrow(dati_continente) > 0) {
    nome_file <-
      paste0("grafico_emissioni_basate_sulla_domanda_", cont, ".pdf")
    # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
    X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
    X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
    
    # Calcola il minimo e il massimo valore per l'asse y
    Y_MIN <- 0
    Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Crea un grafico per "Production-based CO2 emissions" con ggplot
    grafico_co2_prod <-
      ggplot(dati_continente,
             aes(
               x = YEA,
               y = Value,
               group = Country,
               color = Country
             )) +
      geom_line(alpha = 0.8, size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
      geom_point(
        size = 2,
        shape = 16,
        fill = "white",
        show.legend = FALSE
      ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
      scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
      theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
      scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x
      scale_y_log10() +  # Imposta i punti di interruzione per l'asse y
      ggtitle(bquote("Emissioni di CO"[2] * " della domanda per" ~ .(cont))) +
      labs(
        color = "Paesi",
        x = "Anno",
        # Etichetta dell'asse x
        y = expression("Produttività basata sulla CO"[2] * " della domanda in  (USD/kg) in scala logaritmica")
      ) +  # Etichetta dell'asse y con notazione CO2
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        # Imposta la posizione della legenda e le dimensioni della chiave
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Imposta l'angolo e l'allineamento del testo sull'asse x
        axis.title.x = element_text(margin = margin(t = 30)),
        # Imposta il margine per l'etichetta dell'asse x
        axis.title.y = element_text(margin = margin(r = 30)),
        # Imposta il margine per l'etichetta dell'asse y
        plot.title = element_text(
          face = 'bold',
          hjust = 0.5,
          margin = margin(b = 25, t = 15)
        )  # Imposta l'allineamento e il margine per il titolo del grafico
      )
    
    # Salva il grafico
    ggsave(
      nome_file,
      grafico_co2_prod,
      path = percorso_sotto_cartella,
      device = "pdf",
      width = 29.7,
      height = 21,
      units = "cm"
    )
  } else {
    print(paste("Nessun dato disponibile per il continente:", cont))
  }
}

# Imposta la directory di destinazione
directory_destinazione <- "AcquaERisorseNaturali"

# Assicurati che la sottodirectory esista, altrimenti creala
if (!file.exists(directory_destinazione)) {
  dir.create(directory_destinazione)
}

# Specifica il nome della sotto-cartella
sotto_cartella <- "PopulationWater"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)
if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

dataset_r <-
  filter(dataset, Variable == "Population with access to improved drinking water sources, % total population")

# Calcolo delle statistiche descrittive
statistiche_descrittive <- dataset_r %>%
  summarise(
    Media = mean(Value, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Deviazione_Standard = sd(Value, na.rm = TRUE),
    Minimo = min(Value, na.rm = TRUE),
    Massimo = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Calcolo di altri indici
frequenza_assoluta <- sum(!is.na(dataset_r$Value))
frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
simmetria <- skewness(dataset_r$Value)
curtosi <- kurtosis(dataset_r$Value)
deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
coefficiente_di_variazione <-
  deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
varianza <- var(dataset_r$Value, na.rm = TRUE)

# Stampa delle statistiche
print("Population with access to improved drinking water sources, % total population")
print(statistiche_descrittive)
cat("Frequenza assoluta:", frequenza_assoluta, "\n")
cat("Frequenza relativa:", frequenza_relativa, "\n")
cat("Simmetria:", simmetria, "\n")
cat("Curtosi:", curtosi, "\n")
cat("Deviazione standard:", deviazione_standard, "\n")
cat("Coefficienti di variazione:",
    coefficiente_di_variazione,
    "\n")
cat("Varianza:", varianza, "\n")

# Crea un box plot per ciascuna variabile
grafico <-
  ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
  ggtitle(bquote(
    "Boxplot per la variabile: Accesso alle Fonti d'Acqua Potabile Migliorate"
  )) +
  geom_boxplot(
    show.legend = FALSE,
    color = "black",
    fill = "#A8CF97",
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.size=3,
    outlier.color = "#00aedb"
  ) +
  labs(x = "Variabile", y = expression("Percentuale di Popolazione (%)")) +
  theme_linedraw(base_size = 14) + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )

# Visualizza il grafico
ggsave(
  "boxplot_population_water.pdf",
  grafico,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea l'istogramma per "Production-based CO2 emissions"
histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  geom_histogram(
    binwidth = 10,
    fill = "#A8CF97",
    color = "black",
    alpha = 0.7
  ) +
  ggtitle(
    bquote(
      "Tendenza dell'Accesso alle Fonti d'Acqua Potabile Migliorate nella Popolazione"
    )
  ) +
  labs(x = expression("Percentuale di Popolazione con Accesso a Fonti d'Acqua Potabile Migliorate rispetto alla Popolazione Totale (%)"),
       y = "Frequenza") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva l'istogramma in un file PDF
ggsave(
  "istogramma_population.pdf",
  histogram_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  stat_ecdf(geom = "step",
            color = "#A8CF97",
            size = 1) +
  ggtitle(
    bquote(
      "Funzione di Distribuzione Empirica - Accesso alle Fonti d'Acqua Potabile Migliorate")
  ) +
  labs(x = expression("Percentuale di Popolazione Servita da Fonti d'Acqua Potabile Migliorate rispetto alla Popolazione Totale (%)"),
       y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )


# Salva la funzione di distribuzione empirica in un file PDF
ggsave(
  "ecdf_population_water.pdf",
  ecdf_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Funzione di densità di probabilità
densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
  geom_density(fill = "#A8CF97", alpha = 0.7) +
  labs(title = "Funzione di Densità di Probabilità",
       x = "Valore",
       y = "Densità") +   theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )

# Salva il grafico della funzione di densità di probabilità in un file PDF
ggsave(
  "densita_population_water.pdf",
  densita_probabilita,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Per ogni continente, crea un grafico
for (cont in names(mappatura_continenti)) {
  dati_continente <- dataset_r %>%
    filter(Country %in% mappatura_continenti[[cont]]) %>%
    arrange(YEA)
  # Se ci sono dati, crea un grafico per il continente
  if (nrow(dati_continente) > 0) {
    nome_file <-
      paste0("grafico_serie_storica_population", cont, ".pdf")
    # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
    X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
    X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Calcola il minimo e il massimo valore per l'asse y
    Y_MIN <- 0
    Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Crea un grafico per "Production-based CO2 emissions" con ggplot
    grafico_foresta <-
      ggplot(dati_continente,
             aes(
               x = YEA,
               y = Value,
               group = Country,
               color = Country
             )) +
      geom_line(alpha = 0.8, size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
      geom_point(
        size = 2,
        shape = 16,
        fill = "white",
        show.legend = FALSE
      ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
      scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
      theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
      scale_x_continuous() +  # Imposta i punti di interruzione per l'asse x
      scale_y_continuous() +  # Imposta i punti di interruzione per l'asse y
      ggtitle(bquote("Tendenza dell'Accesso alle Fonti d'Acqua Potabile Migliorate nella Popolazione in " ~ .(cont))) +
      labs(color = "Paesi",
           x = "Anno",
           # Etichetta dell'asse x
           y = expression("Percentuale di Popolazione (%)")) +  # Etichetta dell'asse y con notazione CO2
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        # Imposta la posizione della legenda e le dimensioni della chiave
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Imposta l'angolo e l'allineamento del testo sull'asse x
        axis.title.x = element_text(margin = margin(t = 30)),
        # Imposta il margine per l'etichetta dell'asse x
        axis.title.y = element_text(margin = margin(r = 30)),
        # Imposta il margine per l'etichetta dell'asse y
        plot.title = element_text(
          face = 'bold',
          hjust = 0.5,
          margin = margin(b = 25, t = 15)
        )  # Imposta l'allineamento e il margine per il titolo del grafico
      )
    
    # Salva il grafico
    ggsave(
      nome_file,
      grafico_foresta,
      path = percorso_sotto_cartella,
      device = "pdf",
      width = 29.7,
      height = 21,
      units = "cm"
    )
  } else {
    print(paste("Nessun dato disponibile per il continente:", cont))
  }
}


# Specifica il nome della sotto-cartella
sotto_cartella <- "WaterStress"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)
if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

dataset_r <-
  filter(
    dataset,
    Variable == "Water stress, total freshwater abstraction as % total available renewable resources"
  )

# Crea l'istogramma per "Production-based CO2 emissions"
histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  geom_histogram(
    binwidth = 10,
    fill = "#A8CF97",
    color = "black",
    alpha = 0.7
  ) +
  ggtitle("Distribuzione dello stress idrico: Prelievo totale di acqua dolce") +
  labs(x = "Stress idrico, prelievo totale di acqua dolce come % delle risorse rinnovabili totali disponibili",
       y = "Frequenza")+ theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta la posizione della legenda e le dimensioni della chiave
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva l'istogramma in un file PDF
ggsave(
  "istogramma_water_stress.pdf",
  histogram_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  stat_ecdf(geom = "step",
            color = "#A8CF97",
            size = 1) +
  ggtitle(
    bquote(
      "Funzione di Distribuzione Empirica -  Stress idrico, estrazione totale di acqua dolce"
    )
  ) +
  labs(x = expression("Emissioni di CO"[2]),
       y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )


# Salva la funzione di distribuzione empirica in un file PDF
ggsave(
  "ecdf_water_stress.pdf",
  ecdf_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Funzione di densità di probabilità
densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
  geom_density(fill = "#A8CF97", alpha = 0.7) +
  labs(title = "Funzione di Densità di Probabilità",
       x = "Valore",
       y = "Densità") +   theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )

# Salva il grafico della funzione di densità di probabilità in un file PDF
ggsave(
  "densita_water_stress.pdf",
  densita_probabilita,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Per ogni continente, crea un grafico
for (cont in names(mappatura_continenti)) {
  dati_continente <- dataset_r %>%
    filter(Country %in% mappatura_continenti[[cont]]) %>%
    arrange(YEA)
  # Se ci sono dati, crea un grafico per il continente
  if (nrow(dati_continente) > 0) {
    nome_file <-
      paste0("grafico_serie_storica_water_", cont, ".pdf")
    # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
    X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
    X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Calcola il minimo e il massimo valore per l'asse y
    Y_MIN <- 0
    Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Crea un grafico per "Production-based CO2 emissions" con ggplot
    grafico_foresta <-
      ggplot(dati_continente,
             aes(
               x = YEA,
               y = Value,
               group = Country,
               color = Country
             )) +
      geom_line(alpha = 0.8, size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
      geom_point(
        size = 2,
        shape = 16,
        fill = "white",
        show.legend = FALSE
      ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
      scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
      theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
      scale_x_continuous() +  # Imposta i punti di interruzione per l'asse x
      scale_y_continuous() +  # Imposta i punti di interruzione per l'asse y
      ggtitle(bquote("Andamento dell'utlizzo delle risorse idriche in " ~ .(cont))) +
      labs(color = "Paesi",
           x = "Anno",
           # Etichetta dell'asse x
           y = expression("Percentuale")) +  # Etichetta dell'asse y con notazione CO2
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        # Imposta la posizione della legenda e le dimensioni della chiave
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Imposta l'angolo e l'allineamento del testo sull'asse x
        axis.title.x = element_text(margin = margin(t = 30)),
        # Imposta il margine per l'etichetta dell'asse x
        axis.title.y = element_text(margin = margin(r = 30)),
        # Imposta il margine per l'etichetta dell'asse y
        plot.title = element_text(
          face = 'bold',
          hjust = 0.5,
          margin = margin(b = 25, t = 15)
        )  # Imposta l'allineamento e il margine per il titolo del grafico
      )
    
    # Salva il grafico
    ggsave(
      nome_file,
      grafico_foresta,
      path = percorso_sotto_cartella,
      device = "pdf",
      width = 29.7,
      height = 21,
      units = "cm"
    )
  } else {
    print(paste("Nessun dato disponibile per il continente:", cont))
  }
}

# Calcolo delle statistiche descrittive
statistiche_descrittive <- dataset_r %>%
  summarise(
    Media = mean(Value, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Deviazione_Standard = sd(Value, na.rm = TRUE),
    Minimo = min(Value, na.rm = TRUE),
    Massimo = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Calcolo di altri indici
frequenza_assoluta <- sum(!is.na(dataset_r$Value))
frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
simmetria <- skewness(dataset_r$Value)
curtosi <- kurtosis(dataset_r$Value)
deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
coefficiente_di_variazione <-
  deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
varianza <- var(dataset_r$Value, na.rm = TRUE)

# Stampa delle statistiche
print("Water stress, total freshwater abstraction as % total available renewable resources")
print(statistiche_descrittive)
cat("Frequenza assoluta:", frequenza_assoluta, "\n")
cat("Frequenza relativa:", frequenza_relativa, "\n")
cat("Simmetria:", simmetria, "\n")
cat("Curtosi:", curtosi, "\n")
cat("Deviazione standard:", deviazione_standard, "\n")
cat("Coefficienti di variazione:",
    coefficiente_di_variazione,
    "\n")
cat("Varianza:", varianza, "\n")

# Crea un box plot per ciascuna variabile
grafico <-
  ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
  ggtitle(bquote(
    "Boxplot per la variabile: Stress idrico, estrazione totale di acqua dolce"
  )) +
  geom_boxplot(
    show.legend = FALSE,
    color = "black",
    fill = "#A8CF97",
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.size=3,
    outlier.color = "#00aedb"
  ) +
  labs(x = "Variabile", y = expression("Percentuale di stress idrico")) +
  theme_linedraw(base_size = 14) + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )

# Visualizza il grafico
ggsave(
  "boxplot_acqua.pdf",
  grafico,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

#Seleziona solo le variabili di interesse
dataset_r <- dataset %>%
  filter(
    Variable == "Water stress, total freshwater abstraction as % total available renewable resources"
  )

# Aggiungi la variabile Continente al dataset
dataset_r <- dataset_r %>%
  mutate(
    Continente = case_when(
      Country %in% mappatura_continenti$Europa_Occidentale ~ "Europa Oc.",
      Country %in% mappatura_continenti$Europa_Orientale ~ "Europa Or.",
      Country %in% mappatura_continenti$America ~ "America",
      Country %in% mappatura_continenti$Asia ~ "Asia",
      Country %in% mappatura_continenti$Oceania ~ "Oceania",
      Country %in% mappatura_continenti$Africa ~ "Africa",
      TRUE ~ "Altro"
    )
  )

# Crea le categorie di water stress in base ai tuoi dati
categorie_water_stress <- dataset_r %>%
  count(Country,
        YEA,
        Continente,
        Categoria = cut(
          Value,
          breaks = c(0, 10, 20, 40, 100),
          labels = c("Basso", "Moderato", "Medio-alto", "Alto")
        ))


# Crea il grafico a barre con faceting per ogni anno
grafico_barre_faceting <-
  ggplot(categorie_water_stress,
         aes(x = Continente, y = n, fill = Categoria)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribuzione del Water Stress per Continente",
       x = "Continente",
       y = "Numero di Paesi") +
  scale_fill_manual(
    values = c(
      "Basso" = "green",
      "Moderato" = "yellow",
      "Medio-alto" = "orange",
      "Alto" = "red"
    )
  ) +
  theme_minimal(base_size = 14) +
  facet_wrap( ~ YEA,  ncol = 4) +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.4, "cm"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(
      face = 'bold',
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    )
  )

# Visualizza il grafico
ggsave(
  "water_stress_continenti.pdf",
  grafico_barre_faceting,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 42,
  height = 29.7,
  units = "cm"
)


dataset_r1 <- dataset_r %>%
  filter(YEA < "2005")

# Crea un istogramma per ciascun anno e continente
grafico_istogrammi <-
  ggplot(dataset_r1, aes(x = Continente, y = Value, fill = Country)) +
  geom_col(position = "dodge", width = 0.5) +
  facet_wrap(~ YEA, scales = "free_x", ncol = 3) +
  ggtitle("Livelli di Water Stress  (Anno per Anno)") +
  labs(x = "Continente", y = "Percentuale di water stress") +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.position = "bottom",
    # Posiziona la legenda sotto il grafico
    legend.box = "horizontal"
  )


# Visualizza il grafico
ggsave(
  "water_stress_paesi_1.pdf",
  grafico_istogrammi,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 42,
  height = 29.7,
  units = "cm"
)

dataset_r2 <- dataset_r %>%
  filter(YEA >= "2005")

# Crea un istogramma per ciascun anno e continente
grafico_istogrammi <-
  ggplot(dataset_r2, aes(x = Continente, y = Value, fill = Country)) +
  geom_col(position = "dodge", width = 0.5) +
  facet_wrap(~ YEA, scales = "free_x", ncol = 3) +
  ggtitle("Livelli di Water Stress  (Anno per Anno)") +
  labs(x = "Continente", y = "Percentuale di water stress") +
  theme(
    legend.key.size = unit(0.4, "cm"),
    legend.position = "bottom",
    # Posiziona la legenda sotto il grafico
    legend.box = "horizontal"
  )


# Visualizza il grafico
ggsave(
  "water_stress_paesi_2.pdf",
  grafico_istogrammi,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 42,
  height = 29.7,
  units = "cm"
)

# Imposta la directory di destinazione
directory_destinazione <- "EfficienzaEnergeticaFontiRinnovabili"

# Assicurati che la sottodirectory esista, altrimenti creala
if (!file.exists(directory_destinazione)) {
  dir.create(directory_destinazione)
}

# # Efficienza Energetica e Uso di Risorse Rinnovabili
# "Renewable energy supply, % total energy supply",
# "Renewable electricity, % total electricity generation",
# "Energy consumption in agriculture, % total energy consumption",
# "Energy consumption in transport, % total energy consumption",
# "Energy consumption in industry, % total energy consumption",

# Specifica il nome della sotto-cartella
sotto_cartella <- "EnergiaRinnovabile"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)
if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

# Filtra il dataset per la variabile "Renewable energy supply, % total energy supply"
dataset_r <- filter(dataset, Variable == "Renewable energy supply, % total energy supply")

# Crea l'istogramma 
histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  geom_histogram(
    binwidth = 10,
    fill = "#A8CF97",
    color = "black",
    alpha = 0.7
  ) +
  ggtitle(
    bquote(
      "Fornitura di energia rinnovabile, % del totale dell'approvvigionamento energetico"
    )
  ) +
  labs(x = expression("Percentuale di energia rinnovabile rispetto al totale"),
       y = "Frequenza",
  ) + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta la posizione della legenda e le dimensioni della chiave

    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva l'istogramma in un file PDF
ggsave(
  "istogramma_energia_rinnovabile.pdf",
  histogram_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  stat_ecdf(geom = "step",
            color = "#A8CF97",
            size = 1) +
  ggtitle(
    bquote(
      "Funzione di Distribuzione Empirica - Percentuale di energia rinnovabile rispetto al totale"
    )
  ) +
  labs(x = expression("Percentuale di energia rinnovabile rispetto al totale"),
       y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )


# Salva la funzione di distribuzione empirica in un file PDF
ggsave(
  "ecdf_energia_rinnovabile.pdf",
  ecdf_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Funzione di densità di probabilità
densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
  geom_density(fill = "#A8CF97", alpha = 0.7) +
  labs(title = "Funzione di Densità di Probabilità",
       x = "Valore",
       y = "Densità") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )

# Salva il grafico della funzione di densità di probabilità in un file PDF
ggsave(
  "densita_energia_rinnovabile.pdf",
  densita_probabilita,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Calcolo delle statistiche descrittive
statistiche_descrittive <- dataset_r %>%
  summarise(
    Media = mean(Value, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Deviazione_Standard = sd(Value, na.rm = TRUE),
    Minimo = min(Value, na.rm = TRUE),
    Massimo = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Calcolo di altri indici
frequenza_assoluta <- sum(!is.na(dataset_r$Value))
frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
simmetria <- skewness(dataset_r$Value)
curtosi <- kurtosis(dataset_r$Value)
deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
varianza <- var(dataset_r$Value, na.rm = TRUE)

# Stampa delle statistiche
print("Renewable energy supply, % total energy supply")
print(statistiche_descrittive)
cat("Frequenza assoluta:", frequenza_assoluta, "\n")
cat("Frequenza relativa:", frequenza_relativa, "\n")
cat("Simmetria:", simmetria, "\n")
cat("Curtosi:", curtosi, "\n")
cat("Deviazione standard:", deviazione_standard, "\n")
cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
cat("Varianza:", varianza, "\n")

# Crea un box plot per la variabile "Renewable energy supply, % total energy supply"
grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
  ggtitle(bquote(
    "Boxplot per la variabile: Fornitura di energia rinnovabile"
  )) +
  geom_boxplot(
    show.legend = FALSE,
    color = "black",
    fill = "#A8CF97",
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.size=3,
    outlier.color = "#00aedb"
  ) +
  scale_y_log10() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Variabile", y = "Renewable Energy Supply (% total energy supply)") +
  theme_linedraw(base_size = 14) + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    )
  )

# Visualizza il grafico
ggsave(
  "boxplot_renewable_energy_supply.pdf",
  grafico,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Per ogni continente, crea un grafico
for (cont in names(mappatura_continenti)) {
  dati_continente <- dataset_r %>%
    filter(Country %in% mappatura_continenti[[cont]]) %>%
    arrange(YEA)
  # Se ci sono dati, crea un grafico per il continente
  if (nrow(dati_continente) > 0) {
    nome_file <-
      paste0("grafico_energia_rinnovabile_", cont, ".pdf")
    # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
    X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
    X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
    
    # Calcola il minimo e il massimo valore per l'asse y
    Y_MIN <- 0
    Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Crea un grafico per "Production-based CO2 emissions" con ggplot
    grafico_co2_prod <-
      ggplot(dati_continente,
             aes(
               x = YEA,
               y = Value,
               group = Country,
               color = Country
             )) +
      geom_line(alpha = 0.8, size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
      geom_point(
        size = 2,
        shape = 16,
        fill = "white",
        show.legend = FALSE
      ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
      scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
      theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
      scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x
      # Imposta i punti di interruzione per l'asse y
      ggtitle(bquote("Andamento Storico dell'Energia Sostenibile in" ~ .(cont))) +
      labs(
        color = "Paesi",
        x = "Anno",
        # Etichetta dell'asse x
        y = expression("Quota di Energia Sostenibile sull'Approvvigionamento Totale (%)")
      ) +  # Etichetta dell'asse y con notazione CO2
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        # Imposta la posizione della legenda e le dimensioni della chiave
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Imposta l'angolo e l'allineamento del testo sull'asse x
        axis.title.x = element_text(margin = margin(t = 30)),
        # Imposta il margine per l'etichetta dell'asse x
        axis.title.y = element_text(margin = margin(r = 30)),
        # Imposta il margine per l'etichetta dell'asse y
        plot.title = element_text(
          face = 'bold',
          hjust = 0.5,
          margin = margin(b = 25, t = 15)
        )  # Imposta l'allineamento e il margine per il titolo del grafico
      )
    
    # Salva il grafico
    ggsave(
      nome_file,
      grafico_co2_prod,
      path = percorso_sotto_cartella,
      device = "pdf",
      width = 29.7,
      height = 21,
      units = "cm"
    )
  } else {
    print(paste("Nessun dato disponibile per il continente:", cont))
  }
}

# Specifica il nome della sotto-cartella
sotto_cartella <- "EnergiaElettricaRinnovabile"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)
if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

# Filtra il dataset per la variabile "Renewable electricity, % total electricity generation"
dataset_r <- filter(dataset, Variable == "Renewable electricity, % total electricity generation")

# Crea l'istogramma 
histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  geom_histogram(
    binwidth = 10,
    fill = "#A8CF97",
    color = "black",
    alpha = 0.7
  ) +
  ggtitle(
    bquote(
      "Percentuale della fornitura di energia elettrica rinnovabile"
    )
  ) +
  labs(x = expression("Percentuale di energia elettrica rinnovabile rispetto al totale"),
       y = "Frequenza",
  ) + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta la posizione della legenda e le dimensioni della chiave

    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva l'istogramma in un file PDF
ggsave(
  "istogramma_energia_elettrica_rinnovabile.pdf",
  histogram_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  stat_ecdf(geom = "step",
            color = "#A8CF97",
            size = 1) +
  ggtitle(
    bquote(
      "Funzione di Distribuzione Empirica - Percentuale di energia rinnovabile rispetto al totale"
    )
  ) +
  labs(x = expression("Percentuale di energia elettrica rinnovabile rispetto al totale"),
       y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )


# Salva la funzione di distribuzione empirica in un file PDF
ggsave(
  "ecdf_energia_elettrica_rinnovabile.pdf",
  ecdf_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Funzione di densità di probabilità
densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
  geom_density(fill = "#A8CF97", alpha = 0.7) +
  labs(title = "Funzione di Densità di Probabilità",
       x = "Valore",
       y = "Densità") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )

# Salva il grafico della funzione di densità di probabilità in un file PDF
ggsave(
  "densita_energia_elettrica_rinnovabile.pdf",
  densita_probabilita,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Calcolo delle statistiche descrittive
statistiche_descrittive <- dataset_r %>%
  summarise(
    Media = mean(Value, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Deviazione_Standard = sd(Value, na.rm = TRUE),
    Minimo = min(Value, na.rm = TRUE),
    Massimo = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Calcolo di altri indici
frequenza_assoluta <- sum(!is.na(dataset_r$Value))
frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
simmetria <- skewness(dataset_r$Value)
curtosi <- kurtosis(dataset_r$Value)
deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
varianza <- var(dataset_r$Value, na.rm = TRUE)

# Stampa delle statistiche
print("Renewable electricity, % total electricity generation")
print(statistiche_descrittive)
cat("Frequenza assoluta:", frequenza_assoluta, "\n")
cat("Frequenza relativa:", frequenza_relativa, "\n")
cat("Simmetria:", simmetria, "\n")
cat("Curtosi:", curtosi, "\n")
cat("Deviazione standard:", deviazione_standard, "\n")
cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
cat("Varianza:", varianza, "\n")

# Crea un box plot per la variabile "Renewable electricity, % total electricity generation"
grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
  ggtitle(bquote(
    "Boxplot per la variabile:  Percentuale di energia elettrica rinnovabile rispetto al totale"
  )) +
  geom_boxplot(
    show.legend = FALSE,
    color = "black",
    fill = "#A8CF97",
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.size=3,
    outlier.color = "#00aedb"
  ) +
  scale_y_continuous() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Variabile", y = " Percentuale di energia rinnovabile rispetto al totale") +
  theme_linedraw(base_size = 14) + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    )
  )

# Visualizza il grafico
ggsave(
  "boxplot_renewable_electricity.pdf",
  grafico,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Per ogni continente, crea un grafico
for (cont in names(mappatura_continenti)) {
  dati_continente <- dataset_r %>%
    filter(Country %in% mappatura_continenti[[cont]]) %>%
    arrange(YEA)
  # Se ci sono dati, crea un grafico per il continente
  if (nrow(dati_continente) > 0) {
    nome_file <-
      paste0("grafico_energia_elettrica_", cont, ".pdf")
    # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
    X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
    X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
    
    # Calcola il minimo e il massimo valore per l'asse y
    Y_MIN <- 0
    Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Crea un grafico per "Production-based CO2 emissions" con ggplot
    grafico_co2_prod <-
      ggplot(dati_continente,
             aes(
               x = YEA,
               y = Value,
               group = Country,
               color = Country
             )) +
      geom_line(alpha = 0.8, size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
      geom_point(
        size = 2,
        shape = 16,
        fill = "white",
        show.legend = FALSE
      ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
      scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
      theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
      scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x # Imposta i punti di interruzione per l'asse y
      ggtitle(bquote("Andamento Storico  della Percentuale di Elettricità da Fonti Rinnovabili (%) in" ~ .(cont))) +
      labs(
        color = "Paesi",
        x = "Anno",
        # Etichetta dell'asse x
        y = expression("Quota di Elettricità Rinnovabile (%)")
      ) +  # Etichetta dell'asse y con notazione CO2
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        # Imposta la posizione della legenda e le dimensioni della chiave
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Imposta l'angolo e l'allineamento del testo sull'asse x
        axis.title.x = element_text(margin = margin(t = 30)),
        # Imposta il margine per l'etichetta dell'asse x
        axis.title.y = element_text(margin = margin(r = 30)),
        # Imposta il margine per l'etichetta dell'asse y
        plot.title = element_text(
          face = 'bold',
          hjust = 0.5,
          margin = margin(b = 25, t = 15)
        )  # Imposta l'allineamento e il margine per il titolo del grafico
      )
    
    # Salva il grafico
    ggsave(
      nome_file,
      grafico_co2_prod,
      path = percorso_sotto_cartella,
      device = "pdf",
      width = 29.7,
      height = 21,
      units = "cm"
    )
  } else {
    print(paste("Nessun dato disponibile per il continente:", cont))
  }
}

# # Specifica il nome della sotto-cartella
# sotto_cartella <- "EnergiaAgricoltura"
# 
# # Crea la sotto-cartella
# percorso_sotto_cartella <-
#   file.path(directory_destinazione, sotto_cartella)
# if (!file.exists(percorso_sotto_cartella)) {
#   dir.create(percorso_sotto_cartella)
# }
# 
# # Filtra il dataset per la variabile "Energy consumption in agriculture, % total energy consumption"
# dataset_r <- filter(dataset, Variable == "Energy consumption in agriculture, % total energy consumption")
# 
# # Crea l'istogramma 
# histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
#   geom_histogram(
#     fill = "#A8CF97",
#     color = "black",
#     alpha = 0.7
#   ) +
#   ggtitle(
#     bquote(
#       "Consumo energetico nell'agricoltura, % del totale del consumo energetico"
#     )
#   ) +
#   labs(x = expression("Percentuale di consumo energetico in agricoltura rispetto al totale"),
#        y = "Frequenza",
#   ) + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     # Imposta l'angolo e l'allineamento del testo sull'asse x
#     axis.title.x = element_text(margin = margin(t = 30)),
#     # Imposta il margine per l'etichetta dell'asse x
#     axis.title.y = element_text(margin = margin(r = 30)),
#     # Imposta il margine per l'etichetta dell'asse y
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
#   )
# 
# # Salva l'istogramma in un file PDF
# ggsave(
#   "istogramma_energia_agricoltura.pdf",
#   histogram_co2_prod,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
# ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
#   stat_ecdf(geom = "step",
#             color = "#A8CF97",
#             size = 1) +
#   ggtitle(
#     bquote(
#       "Funzione di Distribuzione Empirica - Percentuale di consumo energetico in agricoltura"
#     )
#   ) +
#   labs(x = expression("Percentuale di consumo energetico in agricoltura rispetto al totale"),
#        y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     # Imposta l'angolo e l'allineamento del testo sull'asse x
#     axis.title.x = element_text(margin = margin(t = 30)),
#     # Imposta il margine per l'etichetta dell'asse x
#     axis.title.y = element_text(margin = margin(r = 30)),
#     # Imposta il margine per l'etichetta dell'asse y
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
#   )
# 
# 
# # Salva la funzione di distribuzione empirica in un file PDF
# ggsave(
#   "ecdf_energia_agricoltura.pdf",
#   ecdf_co2_prod,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Funzione di densità di probabilità
# densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
#   geom_density(fill = "#A8CF97", alpha = 0.7) +
#   labs(title = "Funzione di Densità di Probabilità",
#        x = "Valore",
#        y = "Densità") + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     axis.title.x = element_text(margin = margin(t = 30)),
#     axis.title.y = element_text(margin = margin(r = 30)),
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))
#   )
# 
# # Salva il grafico della funzione di densità di probabilità in un file PDF
# ggsave(
#   "densita_energia_agricoltura.pdf",
#   densita_probabilita,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Calcolo delle statistiche descrittive
# statistiche_descrittive <- dataset_r %>%
#   summarise(
#     Media = mean(Value, na.rm = TRUE),
#     Mediana = median(Value, na.rm = TRUE),
#     Deviazione_Standard = sd(Value, na.rm = TRUE),
#     Minimo = min(Value, na.rm = TRUE),
#     Massimo = max(Value, na.rm = TRUE),
#     Q1 = quantile(Value, 0.25, na.rm = TRUE),
#     Q3 = quantile(Value, 0.75, na.rm = TRUE)
#   )
# 
# # Calcolo di altri indici
# frequenza_assoluta <- sum(!is.na(dataset_r$Value))
# frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
# simmetria <- skewness(dataset_r$Value)
# curtosi <- kurtosis(dataset_r$Value)
# deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
# coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
# varianza <- var(dataset_r$Value, na.rm = TRUE)
# 
# # Stampa delle statistiche
# print("Energy consumption in agriculture, % total energy consumption")
# print(statistiche_descrittive)
# cat("Frequenza assoluta:", frequenza_assoluta, "\n")
# cat("Frequenza relativa:", frequenza_relativa, "\n")
# cat("Simmetria:", simmetria, "\n")
# cat("Curtosi:", curtosi, "\n")
# cat("Deviazione standard:", deviazione_standard, "\n")
# cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
# cat("Varianza:", varianza, "\n")
# 
# # Crea un box plot per la variabile "Energy consumption in agriculture, % total energy consumption"
# grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
#   ggtitle(bquote(
#     "Boxplot per la variabile: Percentuale di consumo energetico in agricoltura rispetto al totale"
#   )) +
#   geom_boxplot(
#     show.legend = FALSE,
#     color = "black",
#     fill = "#A8CF97",
#     # Notch?
#     notch=TRUE,
#     notchwidth = 0.8,
#     # custom outliers
#     outlier.size=3,
#     outlier.color = "#00aedb"
#   ) +
#   scale_y_continuous() +
#   scale_fill_brewer(palette = "Set2") +
#   labs(x = "Variabile", y = "Percentuale di consumo energetico in agricoltura rispetto al totale") +
#   theme_linedraw(base_size = 14) + theme(
#     axis.text.x = element_blank(),
#     axis.title.x = element_text(margin = margin(t = 30)),
#     axis.title.y = element_text(margin = margin(r = 30)),
#     plot.title = element_text(
#       hjust = 0.5,
#       margin = margin(b = 25, t = 15)
#     )
#   )
# 
# # Visualizza il grafico
# ggsave(
#   "boxplot_energy_consumption_agriculture.pdf",
#   grafico,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Per ogni continente, crea un grafico
# for (cont in names(mappatura_continenti)) {
#   dati_continente <- dataset_r %>%
#     filter(Country %in% mappatura_continenti[[cont]]) %>%
#     arrange(YEA)
#   # Se ci sono dati, crea un grafico per il continente
#   if (nrow(dati_continente) > 0) {
#     nome_file <-
#       paste0("grafico_energia_agricoltura_", cont, ".pdf")
#     # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
#     X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
#     X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
#     
#     # Calcola il minimo e il massimo valore per l'asse y
#     Y_MIN <- 0
#     Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
#     cont <- gsub("_", " ", cont)
#     # Crea un grafico per "Production-based CO2 emissions" con ggplot
#     grafico_co2_prod <-
#       ggplot(dati_continente,
#              aes(
#                x = YEA,
#                y = Value,
#                group = Country,
#                color = Country
#              )) +
#       geom_line(alpha = 0.8, size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
#       geom_point(
#         size = 2,
#         shape = 16,
#         fill = "white",
#         show.legend = FALSE
#       ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
#       scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
#       theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
#       scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x # Imposta i punti di interruzione per l'asse y
#       ggtitle(bquote("Contributo dell'Agricoltura al Consumo Totale di Energia (%) in" ~ .(cont))) +
#       labs(
#         color = "Paesi",
#         x = "Anno",
#         # Etichetta dell'asse x
#         y = expression("Frazione di Consumo Energetico Agricolo (%)")
#       ) +  # Etichetta dell'asse y con notazione CO2
#       theme(
#         legend.position = "right",
#         legend.key.size = unit(0.5, "cm"),
#         # Imposta la posizione della legenda e le dimensioni della chiave
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         # Imposta l'angolo e l'allineamento del testo sull'asse x
#         axis.title.x = element_text(margin = margin(t = 30)),
#         # Imposta il margine per l'etichetta dell'asse x
#         axis.title.y = element_text(margin = margin(r = 30)),
#         # Imposta il margine per l'etichetta dell'asse y
#         plot.title = element_text(
#           face = 'bold',
#           hjust = 0.5,
#           margin = margin(b = 25, t = 15)
#         )  # Imposta l'allineamento e il margine per il titolo del grafico
#       )
#     
#     # Salva il grafico
#     ggsave(
#       nome_file,
#       grafico_co2_prod,
#       path = percorso_sotto_cartella,
#       device = "pdf",
#       width = 29.7,
#       height = 21,
#       units = "cm"
#     )
#   } else {
#     print(paste("Nessun dato disponibile per il continente:", cont))
#   }
# }
# 
# # Specifica il nome della sotto-cartella
# sotto_cartella <- "EnergiaTrasporto"
# 
# # Crea la sotto-cartella
# percorso_sotto_cartella <-
#   file.path(directory_destinazione, sotto_cartella)
# if (!file.exists(percorso_sotto_cartella)) {
#   dir.create(percorso_sotto_cartella)
# }
# # Filtra il dataset per la variabile "Energy consumption in transport, % total energy consumption"
# dataset_r <- filter(dataset, Variable == "Energy consumption in transport, % total energy consumption")
# 
# # Crea l'istogramma 
# histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
#   geom_histogram(
#     binwidth = 10,
#     fill = "#A8CF97",
#     color = "black",
#     alpha = 0.7
#   ) +
#   ggtitle(
#     bquote(
#       "Consumo energetico nel trasporto, % del totale del consumo energetico"
#     )
#   ) +
#   labs(x = expression("Percentuale di consumo energetico nel trasporto rispetto al totale"),
#        y = "Frequenza",
#   ) + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
# 
#     # Imposta l'angolo e l'allineamento del testo sull'asse x
#     axis.title.x = element_text(margin = margin(t = 30)),
#     # Imposta il margine per l'etichetta dell'asse x
#     axis.title.y = element_text(margin = margin(r = 30)),
#     # Imposta il margine per l'etichetta dell'asse y
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
#   )
# 
# # Salva l'istogramma in un file PDF
# ggsave(
#   "istogramma_energia_trasporto.pdf",
#   histogram_co2_prod,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
# ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
#   stat_ecdf(geom = "step",
#             color = "#A8CF97",
#             size = 1) +
#   ggtitle(
#     bquote(
#       "Funzione di Distribuzione Empirica - Percentuale di consumo energetico nel trasporto"
#     )
#   ) +
#   labs(x = expression("Percentuale di consumo energetico in trasporto rispetto al totale"),
#        y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     # Imposta l'angolo e l'allineamento del testo sull'asse x
#     axis.title.x = element_text(margin = margin(t = 30)),
#     # Imposta il margine per l'etichetta dell'asse x
#     axis.title.y = element_text(margin = margin(r = 30)),
#     # Imposta il margine per l'etichetta dell'asse y
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
#   )
# 
# 
# # Salva la funzione di distribuzione empirica in un file PDF
# ggsave(
#   "ecdf_energia_trasporto.pdf",
#   ecdf_co2_prod,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Funzione di densità di probabilità
# densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
#   geom_density(fill = "#A8CF97", alpha = 0.7) +
#   labs(title = "Funzione di Densità di Probabilità",
#        x = "Valore",
#        y = "Densità") + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_text(margin = margin(t = 30)),
#     axis.title.y = element_text(margin = margin(r = 30)),
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))
#   )
# 
# # Salva il grafico della funzione di densità di probabilità in un file PDF
# ggsave(
#   "densita_energia_trasporto.pdf",
#   densita_probabilita,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Calcolo delle statistiche descrittive
# statistiche_descrittive <- dataset_r %>%
#   summarise(
#     Media = mean(Value, na.rm = TRUE),
#     Mediana = median(Value, na.rm = TRUE),
#     Deviazione_Standard = sd(Value, na.rm = TRUE),
#     Minimo = min(Value, na.rm = TRUE),
#     Massimo = max(Value, na.rm = TRUE),
#     Q1 = quantile(Value, 0.25, na.rm = TRUE),
#     Q3 = quantile(Value, 0.75, na.rm = TRUE)
#   )
# 
# # Calcolo di altri indici
# frequenza_assoluta <- sum(!is.na(dataset_r$Value))
# frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
# simmetria <- skewness(dataset_r$Value)
# curtosi <- kurtosis(dataset_r$Value)
# deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
# coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
# varianza <- var(dataset_r$Value, na.rm = TRUE)
# 
# # Stampa delle statistiche
# print("Energy consumption in transport, % total energy consumption")
# print(statistiche_descrittive)
# cat("Frequenza assoluta:", frequenza_assoluta, "\n")
# cat("Frequenza relativa:", frequenza_relativa, "\n")
# cat("Simmetria:", simmetria, "\n")
# cat("Curtosi:", curtosi, "\n")
# cat("Deviazione standard:", deviazione_standard, "\n")
# cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
# cat("Varianza:", varianza, "\n")
# 
# # Crea un box plot per la variabile "Energy consumption in transport, % total energy consumption"
# grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
#   ggtitle(bquote(
#     "Boxplot per la variabile: Percentuale di consumo energetico nel trasporto rispetto al totale"
#   )) +
#   geom_boxplot(
#     show.legend = FALSE,
#     color = "black",
#     fill = "#A8CF97",
#     # Notch?
#     notch=TRUE,
#     notchwidth = 0.8,
#     # custom outliers
#     outlier.size=3,
#     outlier.color = "#00aedb"
#   ) +
#   scale_y_continuous() +
#   scale_fill_brewer(palette = "Set2") +
#   labs(x = "Variabile", y = " Percentuale di consumo energetico nel trasporto rispetto al totale") +
#   theme_linedraw(base_size = 14) + theme(
#     axis.text.x = element_blank(),
#     axis.title.x = element_text(margin = margin(t = 30)),
#     axis.title.y = element_text(margin = margin(r = 30)),
#     plot.title = element_text(
#       hjust = 0.5,
#       margin = margin(b = 25, t = 15)
#     )
#   )
# 
# # Visualizza il grafico
# ggsave(
#   "boxplot_energy_consumption_transport.pdf",
#   grafico,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Specifica il nome della sotto-cartella
# sotto_cartella <- "EnergiaIndustria"
# 
# # Crea la sotto-cartella
# percorso_sotto_cartella <-
#   file.path(directory_destinazione, sotto_cartella)
# if (!file.exists(percorso_sotto_cartella)) {
#   dir.create(percorso_sotto_cartella)
# }
# 
# # Filtra il dataset per la variabile "Energy consumption in industry, % total energy consumption"
# dataset_r <- filter(dataset, Variable == "Energy consumption in industry, % total energy consumption")
# 
# 
# # Crea l'istogramma 
# histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
#   geom_histogram(
#     binwidth = 10,
#     fill = "#A8CF97",
#     color = "black",
#     alpha = 0.7
#   ) +
#   ggtitle(
#     bquote(
#       "Consumo energetico nel trasporto, % del totale del consumo energetico"
#     )
#   ) +
#   labs(x = expression("Percentuale di consumo energetico nell'industria rispetto al totale"),
#        y = "Frequenza",
#   ) + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     # Imposta l'angolo e l'allineamento del testo sull'asse x
#     axis.title.x = element_text(margin = margin(t = 30)),
#     # Imposta il margine per l'etichetta dell'asse x
#     axis.title.y = element_text(margin = margin(r = 30)),
#     # Imposta il margine per l'etichetta dell'asse y
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
#   )
# 
# # Salva l'istogramma in un file PDF
# ggsave(
#   "istogramma_energia_industria.pdf",
#   histogram_co2_prod,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
# ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
#   stat_ecdf(geom = "step",
#             color = "#A8CF97",
#             size = 1) +
#   ggtitle(
#     bquote(
#       "Funzione di Distribuzione Empirica - Consumo energetico nell'industria"
#     )
#   ) +
#   labs(x = expression("Percentuale di consumo energetico in trasporto rispetto al totale"),
#        y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     # Imposta l'angolo e l'allineamento del testo sull'asse x
#     axis.title.x = element_text(margin = margin(t = 30)),
#     # Imposta il margine per l'etichetta dell'asse x
#     axis.title.y = element_text(margin = margin(r = 30)),
#     # Imposta il margine per l'etichetta dell'asse y
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
#   )
# 
# 
# # Salva la funzione di distribuzione empirica in un file PDF
# ggsave(
#   "ecdf_energia_industria.pdf",
#   ecdf_co2_prod,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Funzione di densità di probabilità
# densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
#   geom_density(fill = "#A8CF97", alpha = 0.7) +
#   labs(title = "Funzione di Densità di Probabilità",
#        x = "Valore",
#        y = "Densità") + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_text(margin = margin(t = 30)),
#     axis.title.y = element_text(margin = margin(r = 30)),
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))
#   )
# 
# # Salva il grafico della funzione di densità di probabilità in un file PDF
# ggsave(
#   "densita_energia_industria.pdf",
#   densita_probabilita,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Calcolo delle statistiche descrittive
# statistiche_descrittive <- dataset_r %>%
#   summarise(
#     Media = mean(Value, na.rm = TRUE),
#     Mediana = median(Value, na.rm = TRUE),
#     Deviazione_Standard = sd(Value, na.rm = TRUE),
#     Minimo = min(Value, na.rm = TRUE),
#     Massimo = max(Value, na.rm = TRUE),
#     Q1 = quantile(Value, 0.25, na.rm = TRUE),
#     Q3 = quantile(Value, 0.75, na.rm = TRUE)
#   )
# 
# # Calcolo di altri indici
# frequenza_assoluta <- sum(!is.na(dataset_r$Value))
# frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
# simmetria <- skewness(dataset_r$Value)
# curtosi <- kurtosis(dataset_r$Value)
# deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
# coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
# varianza <- var(dataset_r$Value, na.rm = TRUE)
# 
# # Stampa delle statistiche
# print("Energy consumption in industry, % total energy consumption")
# print(statistiche_descrittive)
# cat("Frequenza assoluta:", frequenza_assoluta, "\n")
# cat("Frequenza relativa:", frequenza_relativa, "\n")
# cat("Simmetria:", simmetria, "\n")
# cat("Curtosi:", curtosi, "\n")
# cat("Deviazione standard:", deviazione_standard, "\n")
# cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
# cat("Varianza:", varianza, "\n")
# 
# # Crea un box plot per la variabile "Energy consumption in industry, % total energy consumption"
# grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
#   ggtitle(bquote(
#     "Boxplot per la variabile: Percentuale di consumo energetico nell'industria"
#   )) +
#   geom_boxplot(
#     show.legend = FALSE,
#     color = "black",
#     fill = "#A8CF97",
#     # Notch?
#     notch=TRUE,
#     notchwidth = 0.8,
#     # custom outliers
#     outlier.size=3,
#     outlier.color = "#00aedb"
#   ) +
#   scale_y_continuous() +
#   scale_fill_brewer(palette = "Set2") +
#   labs(x = "Variabile", y = "Percentuale di consumo energetico nell'industria rispetto al totale") +
#   theme_linedraw(base_size = 14) + theme(
#     axis.text.x = element_blank(),
#     axis.title.x = element_text(margin = margin(t = 30)),
#     axis.title.y = element_text(margin = margin(r = 30)),
#     plot.title = element_text(
#       hjust = 0.5,
#       margin = margin(b = 25, t = 15)
#     )
#   )
# 
# # Visualizza il grafico
# ggsave(
#   "boxplot_energy_consumption_industry.pdf",
#   grafico,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Per ogni continente, crea un grafico
# for (cont in names(mappatura_continenti)) {
#   dati_continente <- dataset_r %>%
#     filter(Country %in% mappatura_continenti[[cont]]) %>%
#     arrange(YEA)
#   # Se ci sono dati, crea un grafico per il continente
#   if (nrow(dati_continente) > 0) {
#     nome_file <-
#       paste0("grafico_energia_industria_", cont, ".pdf")
#     # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
#     X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
#     X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
#     
#     # Calcola il minimo e il massimo valore per l'asse y
#     Y_MIN <- 0
#     Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
#     cont <- gsub("_", " ", cont)
#     # Crea un grafico per "Production-based CO2 emissions" con ggplot
#     grafico_co2_prod <-
#       ggplot(dati_continente,
#              aes(
#                x = YEA,
#                y = Value,
#                group = Country,
#                color = Country
#              )) +
#       geom_line(alpha = 0.8, size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
#       geom_point(
#         size = 2,
#         shape = 16,
#         fill = "white",
#         show.legend = FALSE
#       ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
#       scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
#       theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
#       scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x # Imposta i punti di interruzione per l'asse y
#       ggtitle(bquote("Contributo dell'Industria al Consumo Totale di Energia (%) in" ~ .(cont))) +
#       labs(
#         color = "Paesi",
#         x = "Anno",
#         # Etichetta dell'asse x
#         y = expression("Quota di Energia Consumata nell'Industria (%)")
#       ) +  # Etichetta dell'asse y con notazione CO2
#       theme(
#         legend.position = "right",
#         legend.key.size = unit(0.5, "cm"),
#         # Imposta la posizione della legenda e le dimensioni della chiave
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         # Imposta l'angolo e l'allineamento del testo sull'asse x
#         axis.title.x = element_text(margin = margin(t = 30)),
#         # Imposta il margine per l'etichetta dell'asse x
#         axis.title.y = element_text(margin = margin(r = 30)),
#         # Imposta il margine per l'etichetta dell'asse y
#         plot.title = element_text(
#           face = 'bold',
#           hjust = 0.5,
#           margin = margin(b = 25, t = 15)
#         )  # Imposta l'allineamento e il margine per il titolo del grafico
#       )
#     
#     # Salva il grafico
#     ggsave(
#       nome_file,
#       grafico_co2_prod,
#       path = percorso_sotto_cartella,
#       device = "pdf",
#       width = 29.7,
#       height = 21,
#       units = "cm"
#     )
#   } else {
#     print(paste("Nessun dato disponibile per il continente:", cont))
#   }
# }
# 
# # Specifica il nome della sotto-cartella
# sotto_cartella <- "EnergiaTrasporto"
# 
# # Crea la sotto-cartella
# percorso_sotto_cartella <-
#   file.path(directory_destinazione, sotto_cartella)
# if (!file.exists(percorso_sotto_cartella)) {
#   dir.create(percorso_sotto_cartella)
# }
# 
# 
# # Per ogni continente, crea un grafico
# for (cont in names(mappatura_continenti)) {
#   dati_continente <- dataset_r %>%
#     filter(Country %in% mappatura_continenti[[cont]]) %>%
#     arrange(YEA)
#   # Se ci sono dati, crea un grafico per il continente
#   if (nrow(dati_continente) > 0) {
#     nome_file <-
#       paste0("grafico_energia_trasporto_", cont, ".pdf")
#     # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
#     X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
#     X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
#     
#     # Calcola il minimo e il massimo valore per l'asse y
#     Y_MIN <- 0
#     Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
#     cont <- gsub("_", " ", cont)
#     # Crea un grafico per "Production-based CO2 emissions" con ggplot
#     grafico_co2_prod <-
#       ggplot(dati_continente,
#              aes(
#                x = YEA,
#                y = Value,
#                group = Country,
#                color = Country
#              )) +
#       geom_line(alpha = 0.8, size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
#       geom_point(
#         size = 2,
#         shape = 16,
#         fill = "white",
#         show.legend = FALSE
#       ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
#       scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
#       theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
#       scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x # Imposta i punti di interruzione per l'asse y
#       ggtitle(bquote("Contributo del Trasporto al Consumo Totale di Energia (%) in" ~ .(cont))) +
#       labs(
#         color = "Paesi",
#         x = "Anno",
#         # Etichetta dell'asse x
#         y = expression("Frazione di Consumo Energetico Trasporto (%)")
#       ) +  # Etichetta dell'asse y con notazione CO2
#       theme(
#         legend.position = "right",
#         legend.key.size = unit(0.5, "cm"),
#         # Imposta la posizione della legenda e le dimensioni della chiave
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         # Imposta l'angolo e l'allineamento del testo sull'asse x
#         axis.title.x = element_text(margin = margin(t = 30)),
#         # Imposta il margine per l'etichetta dell'asse x
#         axis.title.y = element_text(margin = margin(r = 30)),
#         # Imposta il margine per l'etichetta dell'asse y
#         plot.title = element_text(
#           face = 'bold',
#           hjust = 0.5,
#           margin = margin(b = 25, t = 15)
#         )  # Imposta l'allineamento e il margine per il titolo del grafico
#       )
#     
#     # Salva il grafico
#     ggsave(
#       nome_file,
#       grafico_co2_prod,
#       path = percorso_sotto_cartella,
#       device = "pdf",
#       width = 29.7,
#       height = 21,
#       units = "cm"
#     )
#   } else {
#     print(paste("Nessun dato disponibile per il continente:", cont))
#   }
# }

# Imposta la directory di destinazione
directory_destinazione <- "Salute"

# Assicurati che la sottodirectory esista, altrimenti creala
if (!file.exists(directory_destinazione)) {
  dir.create(directory_destinazione)
}

# Assicurati che la sottodirectory esista, altrimenti creala
if (!file.exists(directory_destinazione)) {
  dir.create(directory_destinazione)
}

# Specifica il nome della sotto-cartella
sotto_cartella <- "Mortality"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)
if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

# Filtra il dataset per la variabile "Mortality from exposure to ambient PM2.5"
dataset_r <- filter(dataset, Variable == "Mortality from exposure to ambient PM2.5")

# Crea l'istogramma per "Production-based CO2 emissions"
histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  geom_histogram(
    binwidth = 10,
    fill = "#A8CF97",
    color = "black",
    alpha = 0.7
  ) +
  ggtitle(
    bquote(
      "Mortalità dovuta all'Esposizione a PM2.5 nell'Ambiente"
    )
  ) +
  labs(x = expression("Tasso di Mortalità per Esposizione a PM2.5 Ambientale"),
       y = "Frequenza",
  ) + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta la posizione della legenda e le dimensioni della chiave
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva l'istogramma in un file PDF
ggsave(
  "istogramma_mortality.pdf",
  histogram_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  stat_ecdf(geom = "step",
            color = "#A8CF97",
            size = 1) +
  ggtitle(
    bquote(
      "Funzione di Distribuzione Empirica - Mortalità dovuta all'Esposizione a PM2.5 nell'Ambiente"
    )
  ) +
  labs(x = expression("Numero di Morti per Esposizione a PM2.5 nell'Ambiente"),
       y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )


# Salva la funzione di distribuzione empirica in un file PDF
ggsave(
  "ecdf_mortality.pdf",
  ecdf_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Funzione di densità di probabilità
densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
  geom_density(fill = "#A8CF97", alpha = 0.7) +
  labs(title = "Funzione di Densità di Probabilità",
       x = "Valore",
       y = "Densità") +   theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),

    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )

# Salva il grafico della funzione di densità di probabilità in un file PDF
ggsave(
  "densita_mortality.pdf",
  densita_probabilita,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Calcolo delle statistiche descrittive
statistiche_descrittive <- dataset_r %>%
  summarise(
    Media = mean(Value, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Deviazione_Standard = sd(Value, na.rm = TRUE),
    Minimo = min(Value, na.rm = TRUE),
    Massimo = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Calcolo di altri indici
frequenza_assoluta <- sum(!is.na(dataset_r$Value))
frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
simmetria <- skewness(dataset_r$Value)
curtosi <- kurtosis(dataset_r$Value)
deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
varianza <- var(dataset_r$Value, na.rm = TRUE)

# Stampa delle statistiche
print("Mortality from exposure to ambient PM2.5")
print(statistiche_descrittive)
cat("Frequenza assoluta:", frequenza_assoluta, "\n")
cat("Frequenza relativa:", frequenza_relativa, "\n")
cat("Simmetria:", simmetria, "\n")
cat("Curtosi:", curtosi, "\n")
cat("Deviazione standard:", deviazione_standard, "\n")
cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
cat("Varianza:", varianza, "\n")

# Crea un box plot per la variabile "Mortality from exposure to ambient PM2.5"
grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
  ggtitle(bquote(
    "Boxplot per la variabile: Mortalità dovuta all'Esposizione a PM2.5 nell'Ambiente"
  )) +
  geom_boxplot(
    show.legend = FALSE,
    color = "black",
    fill = "#A8CF97",
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.size=3,
    outlier.color = "#00aedb"
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Variabile", y = "Mortalità dovuta all'Esposizione a PM2.5 nell'Ambiente") +
  theme_linedraw(base_size = 14) + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    )
  )

# Visualizza il grafico
ggsave(
  "boxplot_mortality_exposure_ambient_pm2.5.pdf",
  grafico,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

for (cont in names(mappatura_continenti)) {
  dati_continente <- dataset_r %>%
    filter(Country %in% mappatura_continenti[[cont]]) %>%
    arrange(YEA)
  # Se ci sono dati, crea un grafico per il continente
  if (nrow(dati_continente) > 0) {
    nome_file <- paste0("grafico_mortalita_", cont, ".pdf")
    # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
    X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
    X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Calcola il minimo e il massimo valore per l'asse y
    Y_MIN <- 0
    Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
    
    # Crea un grafico per "Production-based CO2 emissions" con ggplot
    grafico_mortalita <-
      ggplot(dati_continente,
             aes(
               x = YEA,
               y = Value,
               group = Country,
               color = Country
             )) +
      geom_line(alpha = 0.8,
                size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
      geom_point(
        aes(group = Country),
        size = 2,
        shape = 16,
        fill = "white",
        show.legend = FALSE
      ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
      scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
      ggtitle(bquote("Mortalità da esposizione a PM2.5 in" ~ .(cont))) +
      theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
      scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x
      labs(
        color = "Paesi",
        x = "Anno",
        # Etichetta dell'asse x
        y = expression("Mortalità per esposizione a PM2.5 (decessi per milione di abitanti)")
      ) +  # Etichetta dell'asse y con notazione CO2
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        # Imposta la posizione della legenda e le dimensioni della chiave
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Imposta l'angolo e l'allineamento del testo sull'asse x
        axis.title.x = element_text(margin = margin(t = 30)),
        # Imposta il margine per l'etichetta dell'asse x
        axis.title.y = element_text(margin = margin(r = 30)),
        # Imposta il margine per l'etichetta dell'asse y
        plot.title = element_text(
          face = 'bold',
          hjust = 0.5,
          margin = margin(b = 25, t = 15)
        )  # Imposta l'allineamento e il margine per il titolo del grafico
      )
    
    # Salva il grafico
    ggsave(
      nome_file,
      grafico_mortalita,
      path = percorso_sotto_cartella,
      device = "pdf",
      width = 29.7,
      height = 21,
      units = "cm"
    )
  } else {
    print(paste("Nessun dato disponibile per il continente:", cont))
  }
}

# Specifica il nome della sotto-cartella
sotto_cartella <- "PopulationExposed"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)
if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

# Filtra il dataset per la variabile "Percentage of population exposed to more than 10 micrograms/m3"
dataset_r <- filter(dataset, Variable == "Percentage of population exposed to more than 10 micrograms/m3")

# Crea l'istogramma per "Production-based CO2 emissions"
histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  geom_histogram(
    binwidth = 10,
    fill = "#A8CF97",
    color = "black",
    alpha = 0.7
  ) +
  ggtitle(
    bquote(
      "Analisi dell'Esposizione Elevata a PM2.5 nella Popolazione"
    )
  ) +
  labs(x = expression("Percentuale di Popolazione Esposta a più di 10 microgrammi/m"^3),
       y = "Frequenza",
  ) + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva l'istogramma in un file PDF
ggsave(
  "istogramma_exposed.pdf",
  histogram_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  stat_ecdf(geom = "step",
            color = "#A8CF97",
            size = 1) +
  ggtitle(
    bquote(
      "Funzione di Distribuzione Empirica - Analisi dell'Esposizione Elevata a PM2.5 nella Popolazione"
    )
  ) +
  labs(x = expression("Percentuale di Popolazione Esposta a più di 10 microgrammi/m"^3),
       y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )


# Salva la funzione di distribuzione empirica in un file PDF
ggsave(
  "ecdf_exposed.pdf",
  ecdf_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Funzione di densità di probabilità
densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
  geom_density(fill = "#A8CF97", alpha = 0.7) +
  labs(title = "Funzione di Densità di Probabilità",
       x = "Valore",
       y = "Densità") +   theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )

# Salva il grafico della funzione di densità di probabilità in un file PDF
ggsave(
  "densita_exposed.pdf",
  densita_probabilita,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)
# Calcolo delle statistiche descrittive
statistiche_descrittive <- dataset_r %>%
  summarise(
    Media = mean(Value, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Deviazione_Standard = sd(Value, na.rm = TRUE),
    Minimo = min(Value, na.rm = TRUE),
    Massimo = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Calcolo di altri indici
frequenza_assoluta <- sum(!is.na(dataset_r$Value))
frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
simmetria <- skewness(dataset_r$Value)
curtosi <- kurtosis(dataset_r$Value)
deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
varianza <- var(dataset_r$Value, na.rm = TRUE)

# Stampa delle statistiche
print("Percentage of population exposed to more than 10 micrograms/m3")
print(statistiche_descrittive)
cat("Frequenza assoluta:", frequenza_assoluta, "\n")
cat("Frequenza relativa:", frequenza_relativa, "\n")
cat("Simmetria:", simmetria, "\n")
cat("Curtosi:", curtosi, "\n")
cat("Deviazione standard:", deviazione_standard, "\n")
cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
cat("Varianza:", varianza, "\n")

# Crea un box plot per la variabile "Percentage of population exposed to more than 10 micrograms/m3"
grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
  ggtitle(bquote(
    "Boxplot per la variabile: Percentuale di Popolazione Esposta a più di 10 microgrammi/m³"
  )) +
  geom_boxplot(
    show.legend = FALSE,
    color = "black",
    fill = "#A8CF97",
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.size=3,
    outlier.color = "#00aedb"
  ) +
  scale_y_continuous() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Variabile", y = "Percentuale di popolazione esposta") +
  theme_linedraw(base_size = 14) + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    )
  )

# Visualizza il grafico
ggsave(
  "boxplot_percentage_population_exposed_to_pm10.pdf",
  grafico,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

for (cont in names(mappatura_continenti)) {
  dati_continente <- dataset_r %>%
    filter(Country %in% mappatura_continenti[[cont]]) %>%
    arrange(YEA)
  # Se ci sono dati, crea un grafico per il continente
  if (nrow(dati_continente) > 0) {
    nome_file <- paste0("grafico_exposed_", cont, ".pdf")
    # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
    X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
    X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Calcola il minimo e il massimo valore per l'asse y
    Y_MIN <- 0
    Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
    
    # Crea un grafico per "Production-based CO2 emissions" con ggplot
    grafico_mortalita <-
      ggplot(dati_continente,
             aes(
               x = YEA,
               y = Value,
               group = Country,
               color = Country
             )) +
      geom_line(alpha = 0.8,
                size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
      geom_point(
        aes(group = Country),
        size = 2,
        shape = 16,
        fill = "white",
        show.legend = FALSE
      ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
      scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
      ggtitle(bquote("Analisi dell'Esposizione Elevata a PM2.5 nella Popolazione  in" ~ .(cont))) +
      theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
      scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x
      labs(
        color = "Paesi",
        x = "Anno",
        # Etichetta dell'asse x
        y = expression("Percentuale di Popolazione Esposta a più di 10 microgrammi/m"^3)
      ) +  # Etichetta dell'asse y con notazione CO2
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        # Imposta la posizione della legenda e le dimensioni della chiave
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Imposta l'angolo e l'allineamento del testo sull'asse x
        axis.title.x = element_text(margin = margin(t = 30)),
        # Imposta il margine per l'etichetta dell'asse x
        axis.title.y = element_text(margin = margin(r = 30)),
        # Imposta il margine per l'etichetta dell'asse y
        plot.title = element_text(
          face = 'bold',
          hjust = 0.5,
          margin = margin(b = 25, t = 15)
        )  # Imposta l'allineamento e il margine per il titolo del grafico
      )
    
    # Salva il grafico
    ggsave(
      nome_file,
      grafico_mortalita,
      path = percorso_sotto_cartella,
      device = "pdf",
      width = 29.7,
      height = 21,
      units = "cm"
    )
  } else {
    print(paste("Nessun dato disponibile per il continente:", cont))
  }
}

# Specifica il nome della sotto-cartella
sotto_cartella <- "WelfareCost"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)
if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

# Filtra il dataset per la variabile "Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent"
dataset_r <- filter(dataset, Variable == "Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent")

# Crea l'istogramma per "Production-based CO2 emissions"
histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  geom_histogram(

    fill = "#A8CF97",
    color = "black",
    alpha = 0.7
  ) +
  ggtitle(
    bquote(
      "Analisi dei Costi sociali legati alle morti premature per l'Esposizione a PM2.5 nell'Ambiente"
    )
  ) +
  labs(x = expression("Costi del Benessere (in unità di PIL)"),
       y = "Frequenza",
  ) + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva l'istogramma in un file PDF
ggsave(
  "istogramma_welfare.pdf",
  histogram_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  stat_ecdf(geom = "step",
            color = "#A8CF97",
            size = 1) +
  ggtitle(
    bquote(
      "Funzione di Distribuzione Empirica"
    )
  ) +
  labs(x = expression("Costi del Benessere (in unità di PIL)"),
       y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta la posizione della legenda e le dimensioni della chiave
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )


# Salva la funzione di distribuzione empirica in un file PDF
ggsave(
  "ecdf_welfare.pdf",
  ecdf_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Funzione di densità di probabilità
densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
  geom_density(fill = "#A8CF97", alpha = 0.7) +
  labs(title = "Funzione di Densità di Probabilità",
       x = "Valore",
       y = "Densità") +   theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )

# Salva il grafico della funzione di densità di probabilità in un file PDF
ggsave(
  "densita_welfare.pdf",
  densita_probabilita,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)
# Calcolo delle statistiche descrittive
statistiche_descrittive <- dataset_r %>%
  summarise(
    Media = mean(Value, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Deviazione_Standard = sd(Value, na.rm = TRUE),
    Minimo = min(Value, na.rm = TRUE),
    Massimo = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Calcolo di altri indici
frequenza_assoluta <- sum(!is.na(dataset_r$Value))
frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
simmetria <- skewness(dataset_r$Value)
curtosi <- kurtosis(dataset_r$Value)
deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
varianza <- var(dataset_r$Value, na.rm = TRUE)

# Stampa delle statistiche
print("Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent")
print(statistiche_descrittive)
cat("Frequenza assoluta:", frequenza_assoluta, "\n")
cat("Frequenza relativa:", frequenza_relativa, "\n")
cat("Simmetria:", simmetria, "\n")
cat("Curtosi:", curtosi, "\n")
cat("Deviazione standard:", deviazione_standard, "\n")
cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
cat("Varianza:", varianza, "\n")

# Crea un box plot per la variabile "Percentage of population exposed to more than 10 micrograms/m3"
grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
  ggtitle(bquote(
    "Boxplot per la variabile: Costi sociali delle morti premature dovute all’esposizione a PM2.5 ambientale"
  )) +
  geom_boxplot(
    show.legend = FALSE,
    color = "black",
    fill = "#A8CF97",
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.size=3,
    outlier.color = "#00aedb"
  ) +
  scale_y_continuous() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Variabile", y = "Costi sociali (in unità di PIL)") +
  theme_linedraw(base_size = 14) + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    )
  )

# Visualizza il grafico
ggsave(
  "boxplot_welfare.pdf",
  grafico,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

for (cont in names(mappatura_continenti)) {
  dati_continente <- dataset_r %>%
    filter(Country %in% mappatura_continenti[[cont]]) %>%
    arrange(YEA)
  # Se ci sono dati, crea un grafico per il continente
  if (nrow(dati_continente) > 0) {
    nome_file <- paste0("grafico_welfare_", cont, ".pdf")
    # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
    X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
    X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Calcola il minimo e il massimo valore per l'asse y
    Y_MIN <- 0
    Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
    
    # Crea un grafico per "Production-based CO2 emissions" con ggplot
    grafico_mortalita <-
      ggplot(dati_continente,
             aes(
               x = YEA,
               y = Value,
               group = Country,
               color = Country
             )) +
      geom_line(alpha = 0.8,
                size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
      geom_point(
        aes(group = Country),
        size = 2,
        shape = 16,
        fill = "white",
        show.legend = FALSE
      ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
      scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
      ggtitle(bquote("Costi sociali delle morti premature dovute all’esposizione a PM2.5 ambientale  in" ~ .(cont))) +
      theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
      scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x
      labs(
        color = "Paesi",
        x = "Anno",
        # Etichetta dell'asse x
        y = expression("Costi del Benessere (in unità di PIL)")
      ) +  # Etichetta dell'asse y con notazione CO2
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        # Imposta la posizione della legenda e le dimensioni della chiave
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Imposta l'angolo e l'allineamento del testo sull'asse x
        axis.title.x = element_text(margin = margin(t = 30)),
        # Imposta il margine per l'etichetta dell'asse x
        axis.title.y = element_text(margin = margin(r = 30)),
        # Imposta il margine per l'etichetta dell'asse y
        plot.title = element_text(
          face = 'bold',
          hjust = 0.5,
          margin = margin(b = 25, t = 15)
        )  # Imposta l'allineamento e il margine per il titolo del grafico
      )
    
    # Salva il grafico
    ggsave(
      nome_file,
      grafico_mortalita,
      path = percorso_sotto_cartella,
      device = "pdf",
      width = 29.7,
      height = 21,
      units = "cm"
    )
  } else {
    print(paste("Nessun dato disponibile per il continente:", cont))
  }
}

# Imposta la directory di destinazione
directory_destinazione <- "RisorseNaturali"

# Assicurati che la sottodirectory esista, altrimenti creala
if (!file.exists(directory_destinazione)) {
  dir.create(directory_destinazione)
}

# "Phosphorus balance per hectare",
# "Nitrogen balance per hectare",
# "Terrestrial protected area, % land area",
# "Marine protected area, % total exclusive economic zone"

# Specifica il nome della sotto-cartella
sotto_cartella <- "MarineProtectedArea"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)
if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

# Filtra il dataset per la variabile "Nitrogen balance per hectare"
dataset_r <- filter(dataset, Variable == "Marine protected area, % total exclusive economic zone")

# Crea l'istogramma per "Production-based CO2 emissions"
histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  geom_histogram(
    binwidth = 10,
    fill = "#A8CF97",
    color = "black",
    alpha = 0.7
  ) +
  ggtitle(
    bquote(
      "Area Marina Protetta come Percentuale della Zona Economica Esclusiva Totale"
    )
  ) +
  labs(x = expression("Percentuale dell'Area Marina Protetta"),
       y = "Frequenza",
  ) + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta la posizione della legenda e le dimensioni della chiave
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva l'istogramma in un file PDF
ggsave(
  "istogramma_marine.pdf",
  histogram_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  stat_ecdf(geom = "step",
            color = "#A8CF97",
            size = 1) +
  ggtitle(
    bquote(
      "Funzione di Distribuzione Empirica"
    )
  ) +
  labs(x = expression("Percentuale dell'Area Marina Protetta"),
       y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta la posizione della legenda e le dimensioni della chiave
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )


# Salva la funzione di distribuzione empirica in un file PDF
ggsave(
  "ecdf_marine.pdf",
  ecdf_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Funzione di densità di probabilità
densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
  geom_density(fill = "#A8CF97", alpha = 0.7) +
  labs(title = "Funzione di Densità di Probabilità",
       x = "Valore",
       y = "Densità") +   theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )

# Salva il grafico della funzione di densità di probabilità in un file PDF
ggsave(
  "densita_marine.pdf",
  densita_probabilita,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)
# Calcolo delle statistiche descrittive
statistiche_descrittive <- dataset_r %>%
  summarise(
    Media = mean(Value, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Deviazione_Standard = sd(Value, na.rm = TRUE),
    Minimo = min(Value, na.rm = TRUE),
    Massimo = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Calcolo di altri indici
frequenza_assoluta <- sum(!is.na(dataset_r$Value))
frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
simmetria <- skewness(dataset_r$Value)
curtosi <- kurtosis(dataset_r$Value)
deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
varianza <- var(dataset_r$Value, na.rm = TRUE)

# Stampa delle statistiche
print("Marine protected area, % land area")
print(statistiche_descrittive)
cat("Frequenza assoluta:", frequenza_assoluta, "\n")
cat("Frequenza relativa:", frequenza_relativa, "\n")
cat("Simmetria:", simmetria, "\n")
cat("Curtosi:", curtosi, "\n")
cat("Deviazione standard:", deviazione_standard, "\n")
cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
cat("Varianza:", varianza, "\n")

# Crea un box plot per la variabile "Percentage of population exposed to more than 10 micrograms/m3"
grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
  ggtitle(bquote(
    "Boxplot per la variabile: Area Marina Protetta"
  )) +
  geom_boxplot(
    show.legend = FALSE,
    color = "black",
    fill = "#A8CF97",
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.size=3,
    outlier.color = "#00aedb"
  ) +
  scale_y_continuous() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Variabile", y = "Percentuale dell'Area Marina Protetta") +
  theme_linedraw(base_size = 14) + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    )
  )

# Visualizza il grafico
ggsave(
  "boxplot_marine.pdf",
  grafico,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

for (cont in names(mappatura_continenti)) {
  dati_continente <- dataset_r %>%
    filter(Country %in% mappatura_continenti[[cont]]) %>%
    arrange(YEA)
  # Se ci sono dati, crea un grafico per il continente
  if (nrow(dati_continente) > 0) {
    nome_file <- paste0("grafico_marine_", cont, ".pdf")
    # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
    X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
    X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Calcola il minimo e il massimo valore per l'asse y
    Y_MIN <- 0
    Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
    
    # Crea un grafico per "Production-based CO2 emissions" con ggplot
    grafico_mortalita <-
      ggplot(dati_continente,
             aes(
               x = YEA,
               y = Value,
               group = Country,
               color = Country
             )) +
      geom_line(alpha = 0.8,
                size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
      geom_point(
        aes(group = Country),
        size = 2,
        shape = 16,
        fill = "white",
        show.legend = FALSE
      ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
      scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
      ggtitle(bquote("Area Marina Protetta come Percentuale della Zona Economica Esclusiva Totale in " ~ .(cont))) +
      theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
      scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x
      labs(
        color = "Paesi",
        x = "Anno",
        # Etichetta dell'asse x
        y = expression("Percentuale dell'Area Marina Protetta")
      ) +  # Etichetta dell'asse y con notazione CO2
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        # Imposta la posizione della legenda e le dimensioni della chiave
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Imposta l'angolo e l'allineamento del testo sull'asse x
        axis.title.x = element_text(margin = margin(t = 30)),
        # Imposta il margine per l'etichetta dell'asse x
        axis.title.y = element_text(margin = margin(r = 30)),
        # Imposta il margine per l'etichetta dell'asse y
        plot.title = element_text(
          face = 'bold',
          hjust = 0.5,
          margin = margin(b = 25, t = 15)
        )  # Imposta l'allineamento e il margine per il titolo del grafico
      )
    
    # Salva il grafico
    ggsave(
      nome_file,
      grafico_mortalita,
      path = percorso_sotto_cartella,
      device = "pdf",
      width = 29.7,
      height = 21,
      units = "cm"
    )
  } else {
    print(paste("Nessun dato disponibile per il continente:", cont))
  }
}

# Specifica il nome della sotto-cartella
sotto_cartella <- "TerrestrialProtectedArea"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)
if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

# Filtra il dataset per la variabile "Nitrogen balance per hectare"
dataset_r <- filter(dataset, Variable == "Terrestrial protected area, % land area")

# Crea l'istogramma per "Production-based CO2 emissions"
histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  geom_histogram(
    binwidth = 10,
    fill = "#A8CF97",
    color = "black",
    alpha = 0.7
  ) +
  ggtitle(
    bquote(
      "Area Terrestre Protetta come Percentuale della Superficie Terrestre"
    )
  ) +
  labs(x = expression("Percentuale dell'Area Terrestre Protetta"),
       y = "Frequenza",
  ) + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva l'istogramma in un file PDF
ggsave(
  "istogramma_terrest.pdf",
  histogram_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  stat_ecdf(geom = "step",
            color = "#A8CF97",
            size = 1) +
  ggtitle(
    bquote(
      "Funzione di Distribuzione Empirica"
    )
  ) +
  labs(x = expression("Percentuale dell'Area Terrestre Protetta"),
       y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )


# Salva la funzione di distribuzione empirica in un file PDF
ggsave(
  "ecdf_terrest.pdf",
  ecdf_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Funzione di densità di probabilità
densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
  geom_density(fill = "#A8CF97", alpha = 0.7) +
  labs(title = "Funzione di Densità di Probabilità",
       x = "Valore",
       y = "Densità") +   theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )

# Salva il grafico della funzione di densità di probabilità in un file PDF
ggsave(
  "densita_terrest.pdf",
  densita_probabilita,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)
# Calcolo delle statistiche descrittive
statistiche_descrittive <- dataset_r %>%
  summarise(
    Media = mean(Value, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Deviazione_Standard = sd(Value, na.rm = TRUE),
    Minimo = min(Value, na.rm = TRUE),
    Massimo = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Calcolo di altri indici
frequenza_assoluta <- sum(!is.na(dataset_r$Value))
frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
simmetria <- skewness(dataset_r$Value)
curtosi <- kurtosis(dataset_r$Value)
deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
varianza <- var(dataset_r$Value, na.rm = TRUE)

# Stampa delle statistiche
print("Terrestrial protected area, % land area")
print(statistiche_descrittive)
cat("Frequenza assoluta:", frequenza_assoluta, "\n")
cat("Frequenza relativa:", frequenza_relativa, "\n")
cat("Simmetria:", simmetria, "\n")
cat("Curtosi:", curtosi, "\n")
cat("Deviazione standard:", deviazione_standard, "\n")
cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
cat("Varianza:", varianza, "\n")

# Crea un box plot per la variabile "Percentage of population exposed to more than 10 micrograms/m3"
grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
  ggtitle(bquote(
    "Boxplot per la variabile: Area Terrestre Protetta"
  )) +
  geom_boxplot(
    show.legend = FALSE,
    color = "black",
    fill = "#A8CF97",
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.size=3,
    outlier.color = "#00aedb"
  ) +
  scale_y_continuous() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Variabile", y = "Percentuale dell'Area Terrestre Protetta") +
  theme_linedraw(base_size = 14) + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    )
  )

# Visualizza il grafico
ggsave(
  "boxplot_terrest.pdf",
  grafico,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

for (cont in names(mappatura_continenti)) {
  dati_continente <- dataset_r %>%
    filter(Country %in% mappatura_continenti[[cont]]) %>%
    arrange(YEA)
  # Se ci sono dati, crea un grafico per il continente
  if (nrow(dati_continente) > 0) {
    nome_file <- paste0("grafico_balance_", cont, ".pdf")
    # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
    X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
    X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Calcola il minimo e il massimo valore per l'asse y
    Y_MIN <- 0
    Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
    
    # Crea un grafico per "Production-based CO2 emissions" con ggplot
    grafico_mortalita <-
      ggplot(dati_continente,
             aes(
               x = YEA,
               y = Value,
               group = Country,
               color = Country
             )) +
      geom_line(alpha = 0.8,
                size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
      geom_point(
        aes(group = Country),
        size = 2,
        shape = 16,
        fill = "white",
        show.legend = FALSE
      ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
      scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
      ggtitle(bquote("Area Terrestre Protetta come Percentuale della Superficie Terrestre in " ~ .(cont))) +
      theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
      scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x
      labs(
        color = "Paesi",
        x = "Anno",
        # Etichetta dell'asse x
        y = expression("Percentuale dell'Area Terrestre Protetta")
      ) +  # Etichetta dell'asse y con notazione CO2
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        # Imposta la posizione della legenda e le dimensioni della chiave
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Imposta l'angolo e l'allineamento del testo sull'asse x
        axis.title.x = element_text(margin = margin(t = 30)),
        # Imposta il margine per l'etichetta dell'asse x
        axis.title.y = element_text(margin = margin(r = 30)),
        # Imposta il margine per l'etichetta dell'asse y
        plot.title = element_text(
          face = 'bold',
          hjust = 0.5,
          margin = margin(b = 25, t = 15)
        )  # Imposta l'allineamento e il margine per il titolo del grafico
      )
    
    # Salva il grafico
    ggsave(
      nome_file,
      grafico_mortalita,
      path = percorso_sotto_cartella,
      device = "pdf",
      width = 29.7,
      height = 21,
      units = "cm"
    )
  } else {
    print(paste("Nessun dato disponibile per il continente:", cont))
  }
}

# # Specifica il nome della sotto-cartella
# sotto_cartella <- "NitrogenBalancePerHectare"
# 
# # Crea la sotto-cartella
# percorso_sotto_cartella <-
#   file.path(directory_destinazione, sotto_cartella)
# if (!file.exists(percorso_sotto_cartella)) {
#   dir.create(percorso_sotto_cartella)
# }
# 
# # Filtra il dataset per la variabile "Nitrogen balance per hectare"
# dataset_r <- filter(dataset, Variable == "Nitrogen balance per hectare")
# 
# # Crea l'istogramma per "Production-based CO2 emissions"
# histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
#   geom_histogram(
#     binwidth = 10,
#     fill = "#A8CF97",
#     color = "black",
#     alpha = 0.7
#   ) +
#   ggtitle(
#     bquote(
#       "Analisi del Bilancio dell'Azoto per Ettaro"
#     )
#   ) +
#   labs(x = expression("Bilancio dell' Azoto per Ettaro"),
#        y = "Frequenza",
#   ) + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     # Imposta l'angolo e l'allineamento del testo sull'asse x
#     axis.title.x = element_text(margin = margin(t = 30)),
#     # Imposta il margine per l'etichetta dell'asse x
#     axis.title.y = element_text(margin = margin(r = 30)),
#     # Imposta il margine per l'etichetta dell'asse y
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
#   )
# 
# # Salva l'istogramma in un file PDF
# ggsave(
#   "istogramma_balance.pdf",
#   histogram_co2_prod,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
# ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
#   stat_ecdf(geom = "step",
#             color = "#A8CF97",
#             size = 1) +
#   ggtitle(
#     bquote(
#       "Funzione di Distribuzione Empirica"
#     )
#   ) +
#   labs(x = expression("Bilancio di azoto per ettaro"),
#        y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     # Imposta l'angolo e l'allineamento del testo sull'asse x
#     axis.title.x = element_text(margin = margin(t = 30)),
#     # Imposta il margine per l'etichetta dell'asse x
#     axis.title.y = element_text(margin = margin(r = 30)),
#     # Imposta il margine per l'etichetta dell'asse y
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
#   )
# 
# 
# # Salva la funzione di distribuzione empirica in un file PDF
# ggsave(
#   "ecdf_balance.pdf",
#   ecdf_co2_prod,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Funzione di densità di probabilità
# densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
#   geom_density(fill = "#A8CF97", alpha = 0.7) +
#   labs(title = "Funzione di Densità di Probabilità",
#        x = "Valore",
#        y = "Densità") +   theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     axis.title.x = element_text(margin = margin(t = 30)),
#     axis.title.y = element_text(margin = margin(r = 30)),
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))
#   )
# 
# # Salva il grafico della funzione di densità di probabilità in un file PDF
# ggsave(
#   "densita_balance.pdf",
#   densita_probabilita,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# # Calcolo delle statistiche descrittive
# statistiche_descrittive <- dataset_r %>%
#   summarise(
#     Media = mean(Value, na.rm = TRUE),
#     Mediana = median(Value, na.rm = TRUE),
#     Deviazione_Standard = sd(Value, na.rm = TRUE),
#     Minimo = min(Value, na.rm = TRUE),
#     Massimo = max(Value, na.rm = TRUE),
#     Q1 = quantile(Value, 0.25, na.rm = TRUE),
#     Q3 = quantile(Value, 0.75, na.rm = TRUE)
#   )
# 
# # Calcolo di altri indici
# frequenza_assoluta <- sum(!is.na(dataset_r$Value))
# frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
# simmetria <- skewness(dataset_r$Value)
# curtosi <- kurtosis(dataset_r$Value)
# deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
# coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
# varianza <- var(dataset_r$Value, na.rm = TRUE)
# 
# # Stampa delle statistiche
# print("Bilancio di azoto per ettaro")
# print(statistiche_descrittive)
# cat("Frequenza assoluta:", frequenza_assoluta, "\n")
# cat("Frequenza relativa:", frequenza_relativa, "\n")
# cat("Simmetria:", simmetria, "\n")
# cat("Curtosi:", curtosi, "\n")
# cat("Deviazione standard:", deviazione_standard, "\n")
# cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
# cat("Varianza:", varianza, "\n")
# 
# # Crea un box plot per la variabile "Percentage of population exposed to more than 10 micrograms/m3"
# grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
#   ggtitle(bquote(
#     "Boxplot per la variabile: Bilancio di azoto per ettaro"
#   )) +
#   geom_boxplot(
#     show.legend = FALSE,
#     color = "black",
#     fill = "#A8CF97",
#     # Notch?
#     notch=TRUE,
#     notchwidth = 0.8,
#     # custom outliers
#     outlier.size=3,
#     outlier.color = "#00aedb"
#   ) +
#   scale_y_continuous() +
#   scale_fill_brewer(palette = "Set2") +
#   labs(x = "Variabile", y = "Bilancio del Azoto per Ettaro (in unità)") +
#   theme_linedraw(base_size = 14) + theme(
#     axis.text.x = element_blank(),
#     axis.title.x = element_text(margin = margin(t = 30)),
#     axis.title.y = element_text(margin = margin(r = 30)),
#     plot.title = element_text(
#       hjust = 0.5,
#       margin = margin(b = 25, t = 15)
#     )
#   )
# 
# # Visualizza il grafico
# ggsave(
#   "boxplot_balance.pdf",
#   grafico,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# for (cont in names(mappatura_continenti)) {
#   dati_continente <- dataset_r %>%
#     filter(Country %in% mappatura_continenti[[cont]]) %>%
#     arrange(YEA)
#   # Se ci sono dati, crea un grafico per il continente
#   if (nrow(dati_continente) > 0) {
#     nome_file <- paste0("grafico_balance_", cont, ".pdf")
#     # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
#     X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
#     X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
#     cont <- gsub("_", " ", cont)
#     # Calcola il minimo e il massimo valore per l'asse y
#     Y_MIN <- 0
#     Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
#     
#     # Crea un grafico per "Production-based CO2 emissions" con ggplot
#     grafico_mortalita <-
#       ggplot(dati_continente,
#              aes(
#                x = YEA,
#                y = Value,
#                group = Country,
#                color = Country
#              )) +
#       geom_line(alpha = 0.8,
#                 size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
#       geom_point(
#         aes(group = Country),
#         size = 2,
#         shape = 16,
#         fill = "white",
#         show.legend = FALSE
#       ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
#       scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
#       ggtitle(bquote("Bilancio del Azoto per Ettaro in " ~ .(cont))) +
#       theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
#       scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x
#       labs(
#         color = "Paesi",
#         x = "Anno",
#         # Etichetta dell'asse x
#         y = expression("Bilancio del Azoto per Ettaro (in unità)")
#       ) +  # Etichetta dell'asse y con notazione CO2
#       theme(
#         legend.position = "right",
#         legend.key.size = unit(0.5, "cm"),
#         # Imposta la posizione della legenda e le dimensioni della chiave
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         # Imposta l'angolo e l'allineamento del testo sull'asse x
#         axis.title.x = element_text(margin = margin(t = 30)),
#         # Imposta il margine per l'etichetta dell'asse x
#         axis.title.y = element_text(margin = margin(r = 30)),
#         # Imposta il margine per l'etichetta dell'asse y
#         plot.title = element_text(
#           face = 'bold',
#           hjust = 0.5,
#           margin = margin(b = 25, t = 15)
#         )  # Imposta l'allineamento e il margine per il titolo del grafico
#       )
#     
#     # Salva il grafico
#     ggsave(
#       nome_file,
#       grafico_mortalita,
#       path = percorso_sotto_cartella,
#       device = "pdf",
#       width = 29.7,
#       height = 21,
#       units = "cm"
#     )
#   } else {
#     print(paste("Nessun dato disponibile per il continente:", cont))
#   }
# }

# # Specifica il nome della sotto-cartella
# sotto_cartella <- "PhosphorusNalancePerHectare"
# 
# # Crea la sotto-cartella
# percorso_sotto_cartella <-
#   file.path(directory_destinazione, sotto_cartella)
# if (!file.exists(percorso_sotto_cartella)) {
#   dir.create(percorso_sotto_cartella)
# }
# 
# # Filtra il dataset per la variabile "Percentage of population exposure to icing days"
# dataset_r <- filter(dataset, Variable == "Phosphorus balance per hectare")
# 
# # Crea l'istogramma per "Production-based CO2 emissions"
# histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
#   geom_histogram(
#     binwidth = 10,
#     fill = "#A8CF97",
#     color = "black",
#     alpha = 0.7
#   ) +
#   ggtitle(
#     bquote(
#       "Analisi del Bilancio del Fosforo per Ettaro"
#     )
#   ) +
#   labs(x = expression("Bilancio del Fosforo per Ettaro (in unità)"),
#        y = "Frequenza",
#   ) + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     # Imposta l'angolo e l'allineamento del testo sull'asse x
#     axis.title.x = element_text(margin = margin(t = 30)),
#     # Imposta il margine per l'etichetta dell'asse x
#     axis.title.y = element_text(margin = margin(r = 30)),
#     # Imposta il margine per l'etichetta dell'asse y
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
#   )
# 
# # Salva l'istogramma in un file PDF
# ggsave(
#   "istogramma_balance.pdf",
#   histogram_co2_prod,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
# ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
#   stat_ecdf(geom = "step",
#             color = "#A8CF97",
#             size = 1) +
#   ggtitle(
#     bquote(
#       "Funzione di Distribuzione Empirica"
#     )
#   ) +
#   labs(x = expression("Bilancio del Fosforo per Ettaro (in unità)"),
#        y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     # Imposta l'angolo e l'allineamento del testo sull'asse x
#     axis.title.x = element_text(margin = margin(t = 30)),
#     # Imposta il margine per l'etichetta dell'asse x
#     axis.title.y = element_text(margin = margin(r = 30)),
#     # Imposta il margine per l'etichetta dell'asse y
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
#   )
# 
# 
# # Salva la funzione di distribuzione empirica in un file PDF
# ggsave(
#   "ecdf_balance.pdf",
#   ecdf_co2_prod,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Funzione di densità di probabilità
# densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
#   geom_density(fill = "#A8CF97", alpha = 0.7) +
#   labs(title = "Funzione di Densità di Probabilità",
#        x = "Valore",
#        y = "Densità") +   theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     axis.title.x = element_text(margin = margin(t = 30)),
#     axis.title.y = element_text(margin = margin(r = 30)),
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))
#   )
# 
# # Salva il grafico della funzione di densità di probabilità in un file PDF
# ggsave(
#   "densita_balance.pdf",
#   densita_probabilita,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# # Calcolo delle statistiche descrittive
# statistiche_descrittive <- dataset_r %>%
#   summarise(
#     Media = mean(Value, na.rm = TRUE),
#     Mediana = median(Value, na.rm = TRUE),
#     Deviazione_Standard = sd(Value, na.rm = TRUE),
#     Minimo = min(Value, na.rm = TRUE),
#     Massimo = max(Value, na.rm = TRUE),
#     Q1 = quantile(Value, 0.25, na.rm = TRUE),
#     Q3 = quantile(Value, 0.75, na.rm = TRUE)
#   )
# 
# # Calcolo di altri indici
# frequenza_assoluta <- sum(!is.na(dataset_r$Value))
# frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
# simmetria <- skewness(dataset_r$Value)
# curtosi <- kurtosis(dataset_r$Value)
# deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
# coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
# varianza <- var(dataset_r$Value, na.rm = TRUE)
# 
# # Stampa delle statistiche
# print("Phosphorus balance per hectare")
# print(statistiche_descrittive)
# cat("Frequenza assoluta:", frequenza_assoluta, "\n")
# cat("Frequenza relativa:", frequenza_relativa, "\n")
# cat("Simmetria:", simmetria, "\n")
# cat("Curtosi:", curtosi, "\n")
# cat("Deviazione standard:", deviazione_standard, "\n")
# cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
# cat("Varianza:", varianza, "\n")
# 
# # Crea un box plot per la variabile "Percentage of population exposed to more than 10 micrograms/m3"
# grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
#   ggtitle(bquote(
#     "Boxplot per la variabile: Bilancio di fosforo per ettaro"
#   )) +
#   geom_boxplot(
#     show.legend = FALSE,
#     color = "black",
#     fill = "#A8CF97",
#     # Notch?
#     notch=TRUE,
#     notchwidth = 0.8,
#     # custom outliers
#     outlier.size=3,
#     outlier.color = "#00aedb"
#   ) +
#   scale_y_continuous() +
#   scale_fill_brewer(palette = "Set2") +
#   labs(x = "Variabile", y = "Bilancio del Fosforo per Ettaro (in unità)") +
#   theme_linedraw(base_size = 14) + theme(
#     axis.text.x = element_blank(),
#     axis.title.x = element_text(margin = margin(t = 30)),
#     axis.title.y = element_text(margin = margin(r = 30)),
#     plot.title = element_text(
#       hjust = 0.5,
#       margin = margin(b = 25, t = 15)
#     )
#   )
# 
# # Visualizza il grafico
# ggsave(
#   "boxplot_balance.pdf",
#   grafico,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# for (cont in names(mappatura_continenti)) {
#   dati_continente <- dataset_r %>%
#     filter(Country %in% mappatura_continenti[[cont]]) %>%
#     arrange(YEA)
#   # Se ci sono dati, crea un grafico per il continente
#   if (nrow(dati_continente) > 0) {
#     nome_file <- paste0("grafico_balance_", cont, ".pdf")
#     # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
#     X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
#     X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
#     cont <- gsub("_", " ", cont)
#     # Calcola il minimo e il massimo valore per l'asse y
#     Y_MIN <- 0
#     Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
#     
#     # Crea un grafico per "Production-based CO2 emissions" con ggplot
#     grafico_mortalita <-
#       ggplot(dati_continente,
#              aes(
#                x = YEA,
#                y = Value,
#                group = Country,
#                color = Country
#              )) +
#       geom_line(alpha = 0.8,
#                 size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
#       geom_point(
#         aes(group = Country),
#         size = 2,
#         shape = 16,
#         fill = "white",
#         show.legend = FALSE
#       ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
#       scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
#       ggtitle(bquote("Bilancio del Fosforo per Ettaro in " ~ .(cont))) +
#       theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
#       scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x
#       labs(
#         color = "Paesi",
#         x = "Anno",
#         # Etichetta dell'asse x
#         y = expression("Bilancio del Fosforo per Ettaro (in unità)")
#       ) +  # Etichetta dell'asse y con notazione CO2
#       theme(
#         legend.position = "right",
#         legend.key.size = unit(0.5, "cm"),
#         # Imposta la posizione della legenda e le dimensioni della chiave
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         # Imposta l'angolo e l'allineamento del testo sull'asse x
#         axis.title.x = element_text(margin = margin(t = 30)),
#         # Imposta il margine per l'etichetta dell'asse x
#         axis.title.y = element_text(margin = margin(r = 30)),
#         # Imposta il margine per l'etichetta dell'asse y
#         plot.title = element_text(
#           face = 'bold',
#           hjust = 0.5,
#           margin = margin(b = 25, t = 15)
#         )  # Imposta l'allineamento e il margine per il titolo del grafico
#       )
#     
#     # Salva il grafico
#     ggsave(
#       nome_file,
#       grafico_mortalita,
#       path = percorso_sotto_cartella,
#       device = "pdf",
#       width = 29.7,
#       height = 21,
#       units = "cm"
#     )
#   } else {
#     print(paste("Nessun dato disponibile per il continente:", cont))
#   }
# }
# 

# Economia
# "Environmentally related government R&D budget, % total government R&D"
# "Environmentally related taxes, % GDP"
# "Environmentally adjusted multifactor productivity growth"
# "Renewable energy public RD&D budget, % total energy public RD&D"
# "Municipal waste recycled or composted, % treated waste"
# "Municipal waste disposed to landfills, % treated waste"
# "Adjustment for pollution abatement"
# "Net migration"
# "Real GDP per capita"
# "Mean feed-in tariff for wind electricity generation"
# "Development of environment-related technologies, % all technologies"
# "Environmentally related taxes, % total tax revenue"

# Imposta la directory di destinazione
directory_destinazione <- "Economia"

# Assicurati che la sottodirectory esista, altrimenti creala
if (!file.exists(directory_destinazione)) {
  dir.create(directory_destinazione)
}

# Specifica il nome della sotto-cartella
sotto_cartella <- "EnvironmentallyRelatedTaxes"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)
if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

# Filtra il dataset per la variabile "Percentage of population exposure to icing days"
dataset_r <- filter(dataset, Variable == "Environmentally related taxes, % GDP")

# Crea l'istogramma per "Production-based CO2 emissions"
histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  geom_histogram(
    fill = "#A8CF97",
    color = "black",
    alpha = 0.7
  ) +
  ggtitle(
    bquote(
      "Analisi delle Tasse Legate all'Ambiente come Percentuale del Prodotto Interno Lordo"
    )
  ) +
  labs(x = expression("Percentuale delle Tasse Legate all'Ambiente rispetto al PIL"),
       y = "Frequenza",
  ) + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva l'istogramma in un file PDF
ggsave(
  "istogramma_balance.pdf",
  histogram_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  stat_ecdf(geom = "step",
            color = "#A8CF97",
            size = 1) +
  ggtitle(
    bquote(
      "Funzione di Distribuzione Empirica"
    )
  ) +
  labs(x = expression("Percentuale delle Tasse Legate all'Ambiente rispetto al PIL"),
       y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
    margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva la funzione di distribuzione empirica in un file PDF
ggsave(
  "ecdf_envorimetally.pdf",
  ecdf_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Funzione di densità di probabilità
densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
  geom_density(fill = "#A8CF97", alpha = 0.7) +
  labs(title = "Funzione di Densità di Probabilità",
       x = "Valore",
       y = "Densità") +   theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
    margin = margin(b = 25, t = 15))
  )

# Salva il grafico della funzione di densità di probabilità in un file PDF
ggsave(
  "densita_envoromentally.pdf",
  densita_probabilita,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Calcolo delle statistiche descrittive
statistiche_descrittive <- dataset_r %>%
  summarise(
    Media = mean(Value, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Deviazione_Standard = sd(Value, na.rm = TRUE),
    Minimo = min(Value, na.rm = TRUE),
    Massimo = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Calcolo di altri indici
frequenza_assoluta <- sum(!is.na(dataset_r$Value))
frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
simmetria <- skewness(dataset_r$Value)
curtosi <- kurtosis(dataset_r$Value)
deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
varianza <- var(dataset_r$Value, na.rm = TRUE)

# Stampa delle statistiche
print("Environmentally related taxes, % GDP")
print(statistiche_descrittive)
cat("Frequenza assoluta:", frequenza_assoluta, "\n")
cat("Frequenza relativa:", frequenza_relativa, "\n")
cat("Simmetria:", simmetria, "\n")
cat("Curtosi:", curtosi, "\n")
cat("Deviazione standard:", deviazione_standard, "\n")
cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
cat("Varianza:", varianza, "\n")

# Crea un box plot per la variabile "Percentage of population exposed to more than 10 micrograms/m3"
grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
  ggtitle(bquote(
    "Boxplot per la variabile: Tasse Legate all'Ambiente rispetto al PIL"
  )) +
  geom_boxplot(
    show.legend = FALSE,
    color = "black",
    fill = "#A8CF97",
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.size=3,
    outlier.color = "#00aedb"
  ) +
  scale_y_continuous() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Variabile", y = "Percentuale delle Tasse Legate all'Ambiente rispetto al PIL") +
  theme_linedraw(base_size = 14) + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    )
  )

# Visualizza il grafico
ggsave(
  "boxplot_enviromentally.pdf",
  grafico,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

for (cont in names(mappatura_continenti)) {
  dati_continente <- dataset_r %>%
    filter(Country %in% mappatura_continenti[[cont]]) %>%
    arrange(YEA)
  # Se ci sono dati, crea un grafico per il continente
  if (nrow(dati_continente) > 0) {
    nome_file <- paste0("grafico_balance_", cont, ".pdf")
    # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
    X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
    X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Calcola il minimo e il massimo valore per l'asse y
    Y_MIN <- 0
    Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
    
    # Crea un grafico per "Production-based CO2 emissions" con ggplot
    grafico_mortalita <-
      ggplot(dati_continente,
             aes(
               x = YEA,
               y = Value,
               group = Country,
               color = Country
             )) +
      geom_line(alpha = 0.8,
                size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
      geom_point(
        aes(group = Country),
        size = 2,
        shape = 16,
        fill = "white",
        show.legend = FALSE
      ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
      scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
      ggtitle(bquote("Tasse Legate all'Ambiente come Percentuale del Prodotto Interno Lordo  in " ~ .(cont))) +
      theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
      scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x
      labs(
        color = "Paesi",
        x = "Anno",
        # Etichetta dell'asse x
        y = expression("Percentuale delle Tasse Legate all'Ambiente rispetto al PIL")
      ) +  # Etichetta dell'asse y con notazione CO2
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        # Imposta la posizione della legenda e le dimensioni della chiave
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Imposta l'angolo e l'allineamento del testo sull'asse x
        axis.title.x = element_text(margin = margin(t = 30)),
        # Imposta il margine per l'etichetta dell'asse x
        axis.title.y = element_text(margin = margin(r = 30)),
        # Imposta il margine per l'etichetta dell'asse y
        plot.title = element_text(
          face = 'bold',
          hjust = 0.5,
          margin = margin(b = 25, t = 15)
        )  # Imposta l'allineamento e il margine per il titolo del grafico
      )
    
    # Salva il grafico
    ggsave(
      nome_file,
      grafico_mortalita,
      path = percorso_sotto_cartella,
      device = "pdf",
      width = 29.7,
      height = 21,
      units = "cm"
    )
  } else {
    print(paste("Nessun dato disponibile per il continente:", cont))
  }
}


# Specifica il nome della sotto-cartella
sotto_cartella <- "RealGDP"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)
if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

# Filtra il dataset per la variabile "Percentage of population exposure to icing days"
dataset_r <- filter(dataset, Variable == "Real GDP per capita")

# Crea l'istogramma per "Production-based CO2 emissions"
histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  geom_histogram(
    fill = "#A8CF97",
    color = "black",
    alpha = 0.7
  ) +
  ggtitle(
    bquote(
      "Prodotto Interno Lordo Reale (PIL) Pro Capite"
    )
  ) +
  labs(x = expression("Valore del PIL Reale per Capita"),
       y = "Frequenza",
  ) + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),

    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva l'istogramma in un file PDF
ggsave(
  "istogramma_balance.pdf",
  histogram_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
  stat_ecdf(geom = "step",
            color = "#A8CF97",
            size = 1) +
  ggtitle(
    bquote(
      "Funzione di Distribuzione Empirica"
    )
  ) +
  labs(x = expression("Prodotto Interno Lordo Reale (PIL) Pro Capite"),
       y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
  
    # Imposta l'angolo e l'allineamento del testo sull'asse x
    axis.title.x = element_text(margin = margin(t = 30)),
    # Imposta il margine per l'etichetta dell'asse x
    axis.title.y = element_text(margin = margin(r = 30)),
    # Imposta il margine per l'etichetta dell'asse y
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
  )

# Salva la funzione di distribuzione empirica in un file PDF
ggsave(
  "ecdf_envorimetally.pdf",
  ecdf_co2_prod,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Funzione di densità di probabilità
densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
  geom_density(fill = "#A8CF97", alpha = 0.7) +
  labs(title = "Funzione di Densità di Probabilità",
       x = "Valore",
       y = "Densità") +   theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),

    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )

# Salva il grafico della funzione di densità di probabilità in un file PDF
ggsave(
  "densita_envoromentally.pdf",
  densita_probabilita,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Calcolo delle statistiche descrittive
statistiche_descrittive <- dataset_r %>%
  summarise(
    Media = mean(Value, na.rm = TRUE),
    Mediana = median(Value, na.rm = TRUE),
    Deviazione_Standard = sd(Value, na.rm = TRUE),
    Minimo = min(Value, na.rm = TRUE),
    Massimo = max(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

# Calcolo di altri indici
frequenza_assoluta <- sum(!is.na(dataset_r$Value))
frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
simmetria <- skewness(dataset_r$Value)
curtosi <- kurtosis(dataset_r$Value)
deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
varianza <- var(dataset_r$Value, na.rm = TRUE)

# Stampa delle statistiche
print("Real GDP per capita")
print(statistiche_descrittive)
cat("Frequenza assoluta:", frequenza_assoluta, "\n")
cat("Frequenza relativa:", frequenza_relativa, "\n")
cat("Simmetria:", simmetria, "\n")
cat("Curtosi:", curtosi, "\n")
cat("Deviazione standard:", deviazione_standard, "\n")
cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
cat("Varianza:", varianza, "\n")

# Crea un box plot per la variabile "Percentage of population exposed to more than 10 micrograms/m3"
grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
  ggtitle(bquote(
    "Boxplot per la variabile: Prodotto Interno Lordo Reale (PIL) Pro Capite"
  )) +
  geom_boxplot(
    show.legend = FALSE,
    color = "black",
    fill = "#A8CF97",
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    # custom outliers
    outlier.size=3,
    outlier.color = "#00aedb"
  ) +
  scale_y_continuous() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Variabile", y = "Valore del PIL Reale per Capita") +
  theme_linedraw(base_size = 14) + theme(
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = 25, t = 15)
    )
  )

# Visualizza il grafico
ggsave(
  "boxplot_enviromentally.pdf",
  grafico,
  path = percorso_sotto_cartella,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

for (cont in names(mappatura_continenti)) {
  dati_continente <- dataset_r %>%
    filter(Country %in% mappatura_continenti[[cont]]) %>%
    arrange(YEA)
  # Se ci sono dati, crea un grafico per il continente
  if (nrow(dati_continente) > 0) {
    nome_file <- paste0("grafico_balance_", cont, ".pdf")
    # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
    X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
    X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
    cont <- gsub("_", " ", cont)
    # Calcola il minimo e il massimo valore per l'asse y
    Y_MIN <- 0
    Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
    
    # Crea un grafico per "Production-based CO2 emissions" con ggplot
    grafico_mortalita <-
      ggplot(dati_continente,
             aes(
               x = YEA,
               y = Value,
               group = Country,
               color = Country
             )) +
      geom_line(alpha = 0.8,
                size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
      geom_point(
        aes(group = Country),
        size = 2,
        shape = 16,
        fill = "white",
        show.legend = FALSE
      ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
      scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
      ggtitle(bquote("Prodotto Interno Lordo Reale (PIL) Pro Capite in " ~ .(cont))) +
      theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
      scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x
      labs(
        color = "Paesi",
        x = "Anno",
        # Etichetta dell'asse x
        y = expression("Valore del PIL Reale per Capita")
      ) +  # Etichetta dell'asse y con notazione CO2
      theme(
        legend.position = "right",
        legend.key.size = unit(0.5, "cm"),
        # Imposta la posizione della legenda e le dimensioni della chiave
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Imposta l'angolo e l'allineamento del testo sull'asse x
        axis.title.x = element_text(margin = margin(t = 30)),
        # Imposta il margine per l'etichetta dell'asse x
        axis.title.y = element_text(margin = margin(r = 30)),
        # Imposta il margine per l'etichetta dell'asse y
        plot.title = element_text(
          face = 'bold',
          hjust = 0.5,
          margin = margin(b = 25, t = 15)
        )  # Imposta l'allineamento e il margine per il titolo del grafico
      )
    
    # Salva il grafico
    ggsave(
      nome_file,
      grafico_mortalita,
      path = percorso_sotto_cartella,
      device = "pdf",
      width = 29.7,
      height = 21,
      units = "cm"
    )
  } else {
    print(paste("Nessun dato disponibile per il continente:", cont))
  }
}

# # Specifica il nome della sotto-cartella
# sotto_cartella <- "Development"
# 
# # Crea la sotto-cartella
# percorso_sotto_cartella <-
#   file.path(directory_destinazione, sotto_cartella)
# if (!file.exists(percorso_sotto_cartella)) {
#   dir.create(percorso_sotto_cartella)
# }

# # Filtra il dataset per la variabile "Percentage of population exposure to icing days"
# dataset_r <- filter(dataset, Variable == "Development of environment-related technologies, % all technologies")
# 
# # Crea l'istogramma per "Production-based CO2 emissions"
# histogram_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
#   geom_histogram(
#     fill = "#A8CF97",
#     color = "black",
#     alpha = 0.7
#   ) +
#   ggtitle(
#     bquote(
#       "Sviluppo delle Tecnologie Legate all'Ambiente come Percentuale di Tutte le Tecnologie"
#     )
#   ) +
#   labs(x = expression("Percentuale di Sviluppo delle Tecnologie Legate all'Ambiente"),
#        y = "Frequenza",
#   ) + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     # Imposta il margine per l'etichetta dell'asse x
#     axis.title.y = element_text(margin = margin(r = 30)),
#     # Imposta il margine per l'etichetta dell'asse y
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
#   )
# 
# # Salva l'istogramma in un file PDF
# ggsave(
#   "istogramma_balance.pdf",
#   histogram_co2_prod,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Crea la funzione di distribuzione empirica per "Production-based CO2 emissions"
# ecdf_co2_prod <- ggplot(dataset_r, aes(x = Value)) +
#   stat_ecdf(geom = "step",
#             color = "#A8CF97",
#             size = 1) +
#   ggtitle(
#     bquote(
#       "Funzione di Distribuzione Empirica"
#     )
#   ) +
#   labs(x = expression("Percentuale di Sviluppo delle Tecnologie Legate all'Ambiente"),
#        y = "Probabilità Cumulativa") + theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     # Imposta il margine per l'etichetta dell'asse x
#     axis.title.y = element_text(margin = margin(r = 30)),
#     # Imposta il margine per l'etichetta dell'asse y
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))  # Imposta l'allineamento e il margine per il titolo del grafico
#   )
# 
# # Salva la funzione di distribuzione empirica in un file PDF
# ggsave(
#   "ecdf_envorimetally.pdf",
#   ecdf_co2_prod,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Funzione di densità di probabilità
# densita_probabilita <- ggplot(dataset_r, aes(x = Value)) +
#   geom_density(fill = "#A8CF97", alpha = 0.7) +
#   labs(title = "Funzione di Densità di Probabilità",
#        x = "Valore",
#        y = "Densità") +   theme_linedraw(base_size = 14) +
#   theme(
#     legend.position = "right",
#     legend.key.size = unit(0.4, "cm"),
#     axis.title.x = element_text(margin = margin(t = 30)),
#     axis.title.y = element_text(margin = margin(r = 30)),
#     plot.title = element_text(hjust = 0.5,
#                               margin = margin(b = 25, t = 15))
#   )
# 
# # Salva il grafico della funzione di densità di probabilità in un file PDF
# ggsave(
#   "densita_envoromentally.pdf",
#   densita_probabilita,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# # Calcolo delle statistiche descrittive
# statistiche_descrittive <- dataset_r %>%
#   summarise(
#     Media = mean(Value, na.rm = TRUE),
#     Mediana = median(Value, na.rm = TRUE),
#     Deviazione_Standard = sd(Value, na.rm = TRUE),
#     Minimo = min(Value, na.rm = TRUE),
#     Massimo = max(Value, na.rm = TRUE),
#     Q1 = quantile(Value, 0.25, na.rm = TRUE),
#     Q3 = quantile(Value, 0.75, na.rm = TRUE)
#   )
# 
# # Calcolo di altri indici
# frequenza_assoluta <- sum(!is.na(dataset_r$Value))
# frequenza_relativa <- frequenza_assoluta / nrow(dataset_r)
# simmetria <- skewness(dataset_r$Value)
# curtosi <- kurtosis(dataset_r$Value)
# deviazione_standard <- sd(dataset_r$Value, na.rm = TRUE)
# coefficiente_di_variazione <- deviazione_standard / mean(dataset_r$Value, na.rm = TRUE)
# varianza <- var(dataset_r$Value, na.rm = TRUE)
# 
# # Stampa delle statistiche
# print("Development of environment-related technologies, % all technologies")
# print(statistiche_descrittive)
# cat("Frequenza assoluta:", frequenza_assoluta, "\n")
# cat("Frequenza relativa:", frequenza_relativa, "\n")
# cat("Simmetria:", simmetria, "\n")
# cat("Curtosi:", curtosi, "\n")
# cat("Deviazione standard:", deviazione_standard, "\n")
# cat("Coefficienti di variazione:", coefficiente_di_variazione, "\n")
# cat("Varianza:", varianza, "\n")
# 
# # Crea un box plot per la variabile "Percentage of population exposed to more than 10 micrograms/m3"
# grafico <- ggplot(dataset_r, aes(x = Variable, y = Value, fill = Variable)) +
#   ggtitle(bquote(
#     "Boxplot per la variabile: Sviluppo delle Tecnologie Legate all'Ambiente"
#   )) +
#   geom_boxplot(
#     show.legend = FALSE,
#     color = "black",
#     fill = "#A8CF97",
#     # Notch?
#     notch=TRUE,
#     notchwidth = 0.8,
#     # custom outliers
#     outlier.size=3,
#     outlier.color = "#00aedb"
#   ) +
#   scale_y_continuous() +
#   scale_fill_brewer(palette = "Set2") +
#   labs(x = "Variabile", y = "Percentuale di Sviluppo delle Tecnologie Legate all'Ambiente") +
#   theme_linedraw(base_size = 14) + theme(
#     axis.text.x = element_blank(),
#     axis.title.x = element_text(margin = margin(t = 30)),
#     axis.title.y = element_text(margin = margin(r = 30)),
#     plot.title = element_text(
#       hjust = 0.5,
#       margin = margin(b = 25, t = 15)
#     )
#   )
# 
# # Visualizza il grafico
# ggsave(
#   "boxplot_enviromentally.pdf",
#   grafico,
#   path = percorso_sotto_cartella,
#   device = "pdf",
#   width = 29.7,
#   height = 21,
#   units = "cm"
# )
# 
# for (cont in names(mappatura_continenti)) {
#   dati_continente <- dataset_r %>%
#     filter(Country %in% mappatura_continenti[[cont]]) %>%
#     arrange(YEA)
#   # Se ci sono dati, crea un grafico per il continente
#   if (nrow(dati_continente) > 0) {
#     nome_file <- paste0("grafico_balance_", cont, ".pdf")
#     # Calcola il minimo e il massimo valore per l'asse x in base ai dati effettivi
#     X_MIN <- min(dati_continente$YEA, na.rm = TRUE)
#     X_MAX <- max(dati_continente$YEA, na.rm = TRUE)
#     cont <- gsub("_", " ", cont)
#     # Calcola il minimo e il massimo valore per l'asse y
#     Y_MIN <- 0
#     Y_MAX <- max(dati_continente$Value, na.rm = TRUE)
#     
#     # Crea un grafico per "Production-based CO2 emissions" con ggplot
#     grafico_mortalita <-
#       ggplot(dati_continente,
#              aes(
#                x = YEA,
#                y = Value,
#                group = Country,
#                color = Country
#              )) +
#       geom_line(alpha = 0.8,
#                 size = 1) +  # Aggiunge linee al grafico con un'opacità dell'80% e una dimensione del tratto di 1
#       geom_point(
#         aes(group = Country),
#         size = 2,
#         shape = 16,
#         fill = "white",
#         show.legend = FALSE
#       ) +  # Aggiunge punti al grafico con dimensioni e forma specifiche
#       scale_fill_discrete(type = getOption("ggplot2.discrete.fill")) +  # Imposta la scala dei colori per le legende
#       ggtitle(bquote("Sviluppo delle Tecnologie Legate all'Ambiente in " ~ .(cont))) +
#       theme_linedraw(base_size = 14) +  # Imposta il tema per il grafico
#       scale_x_continuous(breaks = scales::pretty_breaks(n = 14)) +  # Imposta i punti di interruzione per l'asse x
#       labs(
#         color = "Paesi",
#         x = "Anno",
#         # Etichetta dell'asse x
#         y = expression("Percentuale di Sviluppo delle Tecnologie Legate all'Ambiente")
#       ) +  # Etichetta dell'asse y con notazione CO2
#       theme(
#         legend.position = "right",
#         legend.key.size = unit(0.5, "cm"),
#         # Imposta la posizione della legenda e le dimensioni della chiave
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         # Imposta l'angolo e l'allineamento del testo sull'asse x
#         axis.title.x = element_text(margin = margin(t = 30)),
#         # Imposta il margine per l'etichetta dell'asse x
#         axis.title.y = element_text(margin = margin(r = 30)),
#         # Imposta il margine per l'etichetta dell'asse y
#         plot.title = element_text(
#           face = 'bold',
#           hjust = 0.5,
#           margin = margin(b = 25, t = 15)
#         )  # Imposta l'allineamento e il margine per il titolo del grafico
#       )
#     
#     # Salva il grafico
#     ggsave(
#       nome_file,
#       grafico_mortalita,
#       path = percorso_sotto_cartella,
#       device = "pdf",
#       width = 29.7,
#       height = 21,
#       units = "cm"
#     )
#   } else {
#     print(paste("Nessun dato disponibile per il continente:", cont))
#   }
# }
