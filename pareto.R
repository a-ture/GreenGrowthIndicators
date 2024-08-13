library(ggplot2)
library(dplyr)


# Imposta la directory di destinazione
directory_destinazione <- "Pareto"

# Assicurati che la sottodirectory esista, altrimenti creala
if (!file.exists(directory_destinazione)) {
  dir.create(directory_destinazione)
}

# Preparazione dei dati per il Diagramma di Pareto
dataset_pareto <- dataset %>%
  filter(Variable == "Production-based CO2 emissions") %>%
  group_by(Country) %>%
  summarise(TotalEmissions = sum(Value)) %>%
  arrange(TotalEmissions) %>%
  mutate(CumPercent = rev(cumsum(rev(TotalEmissions)) / sum(TotalEmissions) * 100))

# Calcola il valore massimo delle emissioni totali per l'asse Y secondario
max_emissions <- max(dataset_pareto$TotalEmissions)

# Creazione del Diagramma di Pareto
pareto <- ggplot(dataset_pareto, aes(x = reorder(Country, -TotalEmissions), y = TotalEmissions)) +
  geom_bar(stat = "identity", fill = "#A8CF97") +
  geom_line(aes(y = CumPercent * max_emissions / 100, group = 1), color = "black") +
  geom_point(aes(y = CumPercent * max_emissions / 100), color = "black", size = 1) +
  geom_text(aes(y = CumPercent * max_emissions / 100, label = paste0(round(CumPercent, 2), "%")), hjust = -0.2, vjust = -0.5, color = "red", size = 2) +
  scale_y_continuous() +theme_linedraw(base_size = 14) +
  labs(title = "Diagrammma di Pareto delle Emissioni basate sulla produzione", x = "Paesi", y = "Emissioni Totali")+
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


# Salva il diagramma di Pareto in un file PDF
ggsave(
  "pareto_production_based_co2.pdf",
  pareto,
  path = directory_destinazione,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)


# Preparazione dei dati per il Diagramma di Pareto
dataset_pareto <- dataset %>%
  filter(Variable == "Demand-based CO2 emissions") %>%
  group_by(Country) %>%
  summarise(TotalEmissions = sum(Value)) %>%
  arrange(TotalEmissions) %>%
  mutate(CumPercent = rev(cumsum(rev(TotalEmissions)) / sum(TotalEmissions) * 100))

# Calcola il valore massimo delle emissioni totali per l'asse Y secondario
max_emissions <- max(dataset_pareto$TotalEmissions)

# Creazione del Diagramma di Pareto
pareto <- ggplot(dataset_pareto, aes(x = reorder(Country, -TotalEmissions), y = TotalEmissions)) +
  geom_bar(stat = "identity", fill = "#A8CF97") +
  geom_line(aes(y = CumPercent * max_emissions / 100, group = 1), color = "black") +
  geom_point(aes(y = CumPercent * max_emissions / 100), color = "black", size = 1) +
  geom_text(aes(y = CumPercent * max_emissions / 100, label = paste0(round(CumPercent, 2), "%")), hjust = -0.2, vjust = -0.5, color = "red", size = 2) +
  scale_y_continuous() +theme_linedraw(base_size = 14) +
  labs(title = "Diagrammma di Pareto delle Emissioni basate sulla domanda", x = "Paesi", y = "Emissioni Totali")+
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


# Salva il diagramma di Pareto in un file PDF
ggsave(
  "pareto_demand_based_co2.pdf",
  pareto,
  path = directory_destinazione,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Preparazione dei dati per il Diagramma di Pareto
dataset_pareto <- dataset %>%
  filter(Variable == "Renewable energy supply, % total energy supply") %>%
  group_by(Country) %>%
  summarise(TotalEmissions = sum(Value)) %>%
  arrange(TotalEmissions) %>%
  mutate(CumPercent = rev(cumsum(rev(TotalEmissions)) / sum(TotalEmissions) * 100))

# Calcola il valore massimo delle emissioni totali per l'asse Y secondario
max_emissions <- max(dataset_pareto$TotalEmissions)

# Creazione del Diagramma di Pareto
pareto <- ggplot(dataset_pareto, aes(x = reorder(Country, -TotalEmissions), y = TotalEmissions)) +
  geom_bar(stat = "identity", fill = "#A8CF97") +
  geom_line(aes(y = CumPercent * max_emissions / 100, group = 1), color = "black") +
  geom_point(aes(y = CumPercent * max_emissions / 100), color = "black", size = 1) +
  geom_text(aes(y = CumPercent * max_emissions / 100, label = paste0(round(CumPercent, 2), "%")), hjust = -0.2, vjust = -0.5, color = "red", size = 2) +
  scale_y_continuous() +theme_linedraw(base_size = 14) +
  labs(title = "Diagrammma di Pareto della Fornitura di energia rinnovabile", x = "Paesi", y = "Fornitura Totale")+
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


# Salva il diagramma di Pareto in un file PDF
ggsave(
  "pareto_energy.pdf",
  pareto,
  path = directory_destinazione,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Preparazione dei dati per il Diagramma di Pareto
dataset_pareto <- dataset %>%
  filter(Variable == "Renewable electricity, % total electricity generation") %>%
  group_by(Country) %>%
  summarise(TotalEmissions = sum(Value)) %>%
  arrange(TotalEmissions) %>%
  mutate(CumPercent = rev(cumsum(rev(TotalEmissions)) / sum(TotalEmissions) * 100))

# Calcola il valore massimo delle emissioni totali per l'asse Y secondario
max_emissions <- max(dataset_pareto$TotalEmissions)

# Creazione del Diagramma di Pareto
pareto <- ggplot(dataset_pareto, aes(x = reorder(Country, -TotalEmissions), y = TotalEmissions)) +
  geom_bar(stat = "identity", fill = "#A8CF97") +
  geom_line(aes(y = CumPercent * max_emissions / 100, group = 1), color = "black") +
  geom_point(aes(y = CumPercent * max_emissions / 100), color = "black", size = 1) +
  geom_text(aes(y = CumPercent * max_emissions / 100, label = paste0(round(CumPercent, 2), "%")), hjust = -0.2, vjust = -0.5, color = "red", size = 2) +
  scale_y_continuous() +theme_linedraw(base_size = 14) +
  labs(title = "Diagrammma di Pareto dell' Energia elettrica rinnovabile", x = "Paesi", y = "Fornitura Totale")+
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


# Salva il diagramma di Pareto in un file PDF
ggsave(
  "pareto_electricity.pdf",
  pareto,
  path = directory_destinazione,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)

# Preparazione dei dati per il Diagramma di Pareto
dataset_pareto <- dataset %>%
  filter(Variable == "Water stress, total freshwater abstraction as % total available renewable resources") %>%
  group_by(Country) %>%
  summarise(TotalEmissions = sum(Value)) %>%
  arrange(TotalEmissions) %>%
  mutate(CumPercent = rev(cumsum(rev(TotalEmissions)) / sum(TotalEmissions) * 100))

# Calcola il valore massimo delle emissioni totali per l'asse Y secondario
max_emissions <- max(dataset_pareto$TotalEmissions)

# Creazione del Diagramma di Pareto
pareto <- ggplot(dataset_pareto, aes(x = reorder(Country, -TotalEmissions), y = TotalEmissions)) +
  geom_bar(stat = "identity", fill = "#A8CF97") +
  geom_line(aes(y = CumPercent * max_emissions / 100, group = 1), color = "black") +
  geom_point(aes(y = CumPercent * max_emissions / 100), color = "black", size = 1) +
  geom_text(aes(y = CumPercent * max_emissions / 100, label = paste0(round(CumPercent, 2), "%")), hjust = -0.2, vjust = -0.5, color = "red", size = 2) +
  scale_y_continuous() +theme_linedraw(base_size = 14) +
  labs(title = "Diagrammma di Pareto dello Stress Idrico", x = "Paesi", y = "Prelievo Totale")+
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


# Salva il diagramma di Pareto in un file PDF
ggsave(
  "pareto_water.pdf",
  pareto,
  path = directory_destinazione,
  device = "pdf",
  width = 29.7,
  height = 21,
  units = "cm"
)


