library(tidyr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(GGally)

# Imposta la directory di destinazione per il heatmap
directory_destinazione <- "Correlazione"

# Assicurati che la sottodirectory esista, altrimenti creala
if (!file.exists(directory_destinazione)) {
  dir.create(directory_destinazione)
}

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
correlation_matrix <-
  cor(transposed_data[,-c(1:2)], use = "pairwise.complete.obs")

# Salva la matrice di correlazione in un file CSV
write.csv(correlation_matrix, file = "matrice_di_correlazione.csv")

# Trasforma la matrice di correlazione in un formato adatto a ggplot
cor_df <- reshape2::melt(correlation_matrix)

# Creare un grafico a matrice di correlazione
heatmap <-
  ggplot(data = cor_df, aes(x = Var1, y = Var2, fill = value)) +
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
  theme_linedraw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      margin = margin(b = 25, t = 15)
    )
  )

# Salva il grafico in un file PDF con orientamento orizzontale e dimensioni A4
ggsave(
  file.path(directory_destinazione, "heatmap.pdf"),
  heatmap,
  width = 42,
  height = 29.7,
  units = "cm"
)

# Imposta la directory di destinazione per gli scatter plots
sotto_cartella <- "Scatterplots"

# Crea la sotto-cartella
percorso_sotto_cartella_s <-
  file.path(directory_destinazione, sotto_cartella)

if (!file.exists(percorso_sotto_cartella_s)) {
  dir.create(percorso_sotto_cartella_s)
}

scatter_plot <-
  ggplot(
    transposed_data,
    aes(x = `Production-based CO2 emissions` , y = `Demand-based CO2 emissions`, color=Country)
  ) +
  geom_point() +
  labs(
    title = "Emissioni basate sulla produzione vs. Emissioni basate sulla domanda",
    x = expression("Emissioni di CO"[2] * " basate sulla produzione"),
    y = expression("Emissioni di CO"[2] * " basate sulla domanda",
                color = "Paesi",)
  ) +
  geom_smooth(
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  ) +
  theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5,
                              margin = margin(b = 25, t = 15))
  )


# Salva il grafico in un file PDF
ggsave(
  file.path(percorso_sotto_cartella_s, "scatter_production_vs_demand.pdf"),
  plot = scatter_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)

# Imposta la directory di destinazione per gli scatter plots
sotto_cartella <- "Regressione_Production_Demand"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)

if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}

modello_produzione_domanda <- lm(`Demand-based CO2 emissions` ~ `Production-based CO2 emissions`, data = transposed_data)
summary(modello_produzione_domanda)
# Creazione del modello di regressione lineare
modello_produzione_domanda <- lm(`Demand-based CO2 emissions` ~ `Production-based CO2 emissions`, data = transposed_data)

# Stampa il riassunto del modello nella console
print(summary(modello_produzione_domanda))

# Cattura l'output del riassunto del modello e lo salva in un file di testo
output_riassunto <- capture.output(summary(modello_produzione_domanda))
write(output_riassunto, file = file.path(percorso_sotto_cartella, "Riassunto_Modello_Produzione_Domanda.txt"))

# Calcolo dei residui standardizzati
residui_standardizzati <- rstandard(modello_produzione_domanda)

g <- ggplot(data = transposed_data, aes(x = fitted(modello_produzione_domanda), y = residui_standardizzati, color = Country)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Valori predetti", y = "Residui standardizzati", title = "Grafico dei Residui Standardizzati vs Valori Predetti") +
  theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(percorso_sotto_cartella, "standard_residui_production_vs_demand.pdf"),
  plot = g,
  width = 29.7,
  height = 21,
  units = "cm"
)

# Scatter Plot: Mortalità PM2.5 vs. Percentuale della Popolazione Esposta
scatter_plot <-
  ggplot(
    transposed_data,
    aes(x = `Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent`, y = `Percentage of population exposed to more than 10 micrograms/m3`, color = Country)
  ) +
  geom_point() +
  geom_smooth(
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  ) +
  labs(title = "Costi del Benessere Mortalità PM2.5 vs. Percentuale Popolazione Esposta",
       x = "Costi del Benessere per Mortalità PM2.5 (Equivalente PIL)",
       y = "Percentuale della Popolazione Esposta a oltre 10 microgrammi/m3",
           color = "Paesi") +
  theme_bw(base_size = 14) + theme(
    legend.position = "right",

    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(
    percorso_sotto_cartella_s,
    "scatter_costi_benessere_vs_percentuale_esposizione.pdf"
  ),
  plot = scatter_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)


# Creazione dello scatter plot 1
scatter_plot <-
  ggplot(
    transposed_data,
    aes(x = `Demand-based CO2 emissions`, y = `Environmentally related taxes, % GDP`, color = Country)
  ) +
  geom_point() +
  geom_smooth(
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  ) +
  labs(title = "Emissioni di CO2 Basate sulla Domanda vs. Tasse Correlate all’Ambiente ",
       x = expression("Emissioni di CO"[2] * " basate sulla domanda"),
       y = "Tasse Correlate all’Ambiente (% del PIL)",
       color = "Paesi") +
  theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(
    percorso_sotto_cartella_s,
    "scatter_demand_vs_taxes.pdf"
  ),
  plot = scatter_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)


# Creazione dello scatter plot 1
scatter_plot <-
  ggplot(
    transposed_data,
    aes(x = `Demand-based CO2 emissions`, y = `Environmentally related taxes, % GDP`, color = Country)
  ) +
  geom_point() +
  geom_smooth(
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  ) +
  labs(title = "Emissioni basate sulla Domanda vs. Tasse Correlate all’Ambiente ",
       x = "Emissioni di CO2 Basate sulla Domanda",
       y = "Tasse Correlate all’Ambiente (% del PIL)",
       color = "Paesi") +
  theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(
    percorso_sotto_cartella_s,
    "scatter_demand_vs_taxes.pdf"
  ),
  plot = scatter_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)


# Creazione dello scatter plot 1
scatter_plot <-
  ggplot(
    transposed_data,
    aes(x = `Mortality from exposure to ambient PM2.5`, y = `Real GDP per capita`, color = Country)
  ) +
  geom_point() +
  geom_smooth(
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  ) +
  labs(title = " Mortalità da Esposizione a PM2.5 Ambientali vs. PIL reale pro capite",
       x = " Mortalità da Esposizione a PM2.5 Ambientali",
       y = "PIL reale pro capite",
       color = "Paesi") +
  theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(
    percorso_sotto_cartella_s,
    "scatter_mortality_vs_real.pdf"
  ),
  plot = scatter_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)

# Creazione dello scatter plot 1
scatter_plot <-
  ggplot(
    transposed_data,
    aes(x = `Population with access to improved drinking water sources, % total population`, y = `Percentage of population exposed to more than 10 micrograms/m3`, color = Country)
  ) +
  geom_point() +
  geom_smooth(
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  ) +
  labs(title = "Popolazione con accesso a acqua potabile vs. Percentuale di Popolazione Esposta",
       x = "Popolazione con accesso a acqua potabile migliorata",
       y = " Percentuale di Popolazione Esposta a Più di 10 µg/m³",
       color = "Paesi") +
  theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(
    percorso_sotto_cartella_s,
    "scatter_population_vs_percent.pdf"
  ),
  plot = scatter_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)

# Creazione dello scatter plot 1
scatter_plot <-
  ggplot(
    transposed_data,
    aes(x = `Renewable electricity, % total electricity generation`, y = `Percentage of population exposed to more than 10 micrograms/m3`, color = Country)
  ) +
  geom_point() +
  geom_smooth(
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  ) +
  labs(title = "Energia Elettrica Rinnovabile vs. Percentuale di Popolazione Esposta a Più di 10 µg/m³",
       x = "Energia Elettrica Rinnovabile (Percentuale della Generazione Totale di Elettricità)",
       y = " Percentuale di Popolazione Esposta a Più di 10 µg/m³",
       color = "Paesi") +
  theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(
    percorso_sotto_cartella_s,
    "scatter_eletrcity_vs_percent.pdf"
  ),
  plot = scatter_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)


# Creazione dello scatter plot 1
scatter_plot <-
  ggplot(
    transposed_data,
    aes(x = `Population with access to improved drinking water sources, % total population`, y = `Real GDP per capita`, color = Country)
  ) +
  geom_point() +
  geom_smooth(
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  ) +
  labs(title = "Popolazione con accesso a acqua potabile migliorata vs. PIL reale pro capite",
       x = "Popolazione con accesso a acqua potabile migliorata",
       y = "PIL reale pro capite",
       color = "Paesi") +
    theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(
    percorso_sotto_cartella_s,
    "scatter_population_vs_real.pdf"
  ),
  plot = scatter_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)

# Creazione dello scatter plot 3
scatter_plot <-
  ggplot(
    transposed_data,
    aes(x = `Renewable energy supply, % total energy supply`, y = `Renewable electricity, % total electricity generation`, color = Country)
  ) +
  geom_point() +
  geom_smooth(
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  ) +
  labs(title = "Elettricità rinnovabile vs. Fornitura di energia rinnovabile",
       x = "Fornitura di energia rinnovabile, % del totale dell'energia fornita",
       y = "Elettricità rinnovabile, % del totale dell'elettricità generata",
       color = "Paesi") +
  theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(
    percorso_sotto_cartella_s,
    "scatter_energia_vs_elettricita.pdf"
  ),
  plot = scatter_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)


# Creazione dello scatter plot 3
scatter_plot <-
  ggplot(
    transposed_data,
    aes(x = `Renewable energy supply, % total energy supply`, y = `Renewable electricity, % total electricity generation`, color = Country)
  ) +
  geom_point() +
  geom_smooth(
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  ) +
  labs(title = "Elettricità rinnovabile vs. Fornitura di energia rinnovabile",
       x = "Fornitura di energia rinnovabile, % del totale dell'energia fornita",
       y = "Elettricità rinnovabile, % del totale dell'elettricità generata",
       color = "Paesi") +
  theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(
    percorso_sotto_cartella_s,
    "scatter_energia_vs_elettricita.pdf"
  ),
  plot = scatter_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)

# Creazione dello scatter plot 5
scatter_plot <-
  ggplot(
    transposed_data,
    aes(x = `Percentage of population exposed to more than 10 micrograms/m3`, y = `Mortality from exposure to ambient PM2.5`, color = Country)
  ) +
  geom_point() +
  geom_smooth(
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  ) +
  labs(title = "Percentuale della popolazione esposta vs. Mortalità dovuta all'esposizione a PM2.5",
       x = "Percentuale della popolazione esposta a più di 10 µg/m³",
       y = "Mortalità dovuta all'esposizione a PM2.5 ambientali",
       color = "Paesi") +
  theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(
    percorso_sotto_cartella_s,
    "scatter_esposizione_vs_mortalità.pdf"
  ),
  plot = scatter_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)

# Creazione dello scatter plot 6 
scatter_plot <-
  ggplot(
    transposed_data,
    aes(x = `Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent`, y = `Mortality from exposure to ambient PM2.5`, color = Country)
  ) +
  geom_point() +   geom_smooth(
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  ) +
  labs(title = "Mortalità dovuta all'esposizione a PM2.5 vs. Costi del benessere per le mortalità premature",
       x = "Costi del benessere per le mortalità premature dovute all'esposizione a PM2.5 ambientale",
       y = "Mortalità dovuta all'esposizione a PM2.5 ambientale",) +
  theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(percorso_sotto_cartella_s, "scatter_mortalità_vs_costi.pdf"),
  plot = scatter_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)
# Imposta la directory di destinazione per gli scatter plots
sotto_cartella <- "Regressione_Multipla"

# Crea la sotto-cartella
percorso_sotto_cartella <-
  file.path(directory_destinazione, sotto_cartella)

if (!file.exists(percorso_sotto_cartella)) {
  dir.create(percorso_sotto_cartella)
}
# Seleziona solo le colonne di interesse
data_selected <- transposed_data %>%
  select(`Percentage of population exposed to more than 10 micrograms/m3`, 
         `Mortality from exposure to ambient PM2.5`, 
         `Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent`)

# Inizia la registrazione dei grafici su un file PDF
pdf(file.path(percorso_sotto_cartella, "scatterplot_coppie.pdf"), width = 11, height = 8.5)

# Crea lo scatterplot a coppie
pairs(data_selected)

# Fermare la registrazione dei grafici e chiudere il file PDF
dev.off()

# Creazione del Modello di Regressione Multipla
modello_regressione_multipla <- lm(`Mortality from exposure to ambient PM2.5` ~ `Percentage of population exposed to more than 10 micrograms/m3` + `Welfare costs of premature mortalities from exposure to ambient PM2.5, GDP equivalent`, data = transposed_data)

# Stampa il riassunto del modello
stime <-summary(modello_regressione_multipla)
print(stime)

intervalli_confidenza <- confint(modello_regressione_multipla, level = 0.95)
print(intervalli_confidenza)

# Puoi anche salvare il riassunto in un file di testo, se necessario
output_riassunto <- capture.output(summary(modello_regressione_multipla))
write(output_riassunto, file = "Riassunto_Modello_Regressione_Multipla.txt")

g<-ggplot(data = transposed_data, aes(x = fitted(modello_regressione_multipla), y = residuals(modello_regressione_multipla), color = Country)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Valori Predetti", y = "Residui", title = "Grafico dei Residui vs Valori Predetti")+
  theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(percorso_sotto_cartella, "residui.pdf"),
  plot = g,
  width = 29.7,
  height = 21,
  units = "cm"
)

g<-ggplot(data = transposed_data, aes(sample = residuals(modello_regressione_multipla))) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot dei Residui")+theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(percorso_sotto_cartella, "qqplot.pdf"),
  plot = g,
  width = 29.7,
  height = 21,
  units = "cm"
)

g<-ggplot(data = transposed_data, aes(x = fitted(modello_regressione_multipla), y = sqrt(abs(residuals(modello_regressione_multipla))))) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(x = "Valori Predetti", y = "Radice Quadrata dei Residui Assoluti", title = "Grafico Scale-Location")+theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(percorso_sotto_cartella, "Scale-Location.pdf"),
  plot = g,
  width = 29.7,
  height = 21,
  units = "cm"
)

residui_standardizzati <- rstandard(modello_regressione_multipla)
g<-ggplot(data = transposed_data, aes(x = fitted(modello_regressione_multipla), y = residui_standardizzati, color = Country)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Valori Predetti", y = "Residui Standardizzati", title = "Grafico dei Residui Standardizzati")+theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )


# Salva il grafico in un file PDF
ggsave(
  file.path(percorso_sotto_cartella, "residui_standar.pdf"),
  plot = g,
  width = 29.7,
  height = 21,
  units = "cm"
)

plot(modello_regressione_multipla, which = 5)


# Creazione dello scatter plot 7
scatter_plot <-
  ggplot(
    transposed_data,
    aes(x = `Renewable energy supply, % total energy supply`, y = `Water stress, total freshwater abstraction as % total available renewable resources`, color = Country)
  ) +
  geom_point() +   geom_smooth(
    color = "red",
    fill = "#69b3a2",
    se = TRUE
  ) +
  labs(title = "Energia Rinnovabile vs. Stress Idrico",
       x = "Stress idrico",
       y = "Energia Rinnovabileper ettaro",) +
  theme_bw(base_size = 14) + theme(
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    axis.title.x = element_text(margin = margin(t = 30)),
    axis.title.y = element_text(margin = margin(r = 30)),
    plot.title = element_text(hjust = 0.5, margin = margin(b = 25, t = 15))
  )

# Salva il grafico in un file PDF
ggsave(
  file.path(percorso_sotto_cartella_s, "scatter_energia_vs_acqua.pdf"),
  plot = scatter_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)


# # Per la validazione incrociata e la valutazione del modello, assicurati di avere 'transposed_data' pronto
# set.seed(123) # Per riproducibilità
# indices <- sample(1:nrow(transposed_data), size = 0.7 * nrow(transposed_data))
# training_data <- transposed_data[indices, ]
# test_data <- transposed_data[-indices, ]
# 
# # Adattare il modello ai dati di training
# modello_cv <- lm(`Demand-based CO2 emissions` ~ `Production-based CO2 emissions`, data = training_data)
# 
# # Valutare il modello sui dati di test
# predizioni <- predict(modello_cv, newdata = test_data)
# reale <- test_data$`Demand-based CO2 emissions`
# 
# # Calcolare l'errore quadratico medio (MSE)
# mse <- mean((reale - predizioni)^2)
# print(paste("L'errore quadratico medio (MSE) sul set di test è:", mse))


