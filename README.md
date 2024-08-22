# GreenGrowthIndicators

## Descrizione del Progetto

Questo progetto, realizzato nell'ambito del corso di Statistica e Analisi dei Dati presso l'Università degli Studi di Salerno per l'anno accademico 2023/2024, include diverse analisi statistiche applicate a variabili ambientali ed economiche. Gli script R inclusi coprono una vasta gamma di tecniche statistiche, dalla preparazione e pulizia dei dati alla regressione, correlazione, analisi univariata e clustering.

Presenta un’analisi approfondita dei dati relativi alle interazioni tra politiche ambientali, sostenibilità e sviluppo economico, utilizzando il dataset “OECD Green Growth”. L’analisi mira a identificare tendenze, correlazioni e insight riguardanti l’impatto delle politiche ambientali sulle società e le economie a livello globale.

## Contenuto del Progetto

### Script R Inclusi

- **`preparazione_dati.R`**: Script per la preparazione e la pulizia dei dati. Questo script esegue operazioni come la gestione dei valori mancanti, la normalizzazione e la trasformazione delle variabili, preparando il dataset per le successive analisi statistiche.

- **`analisi_univariata.R`**: Contiene il codice per eseguire un'analisi univariata delle variabili. Questa analisi si concentra su singole variabili, esplorando la loro distribuzione, calcolando parametri statistici come media, mediana, varianza e generando grafici di distribuzione.

- **`correlazione.R`**: Script dedicato all'analisi delle correlazioni tra variabili. Include il calcolo delle matrici di correlazione e la creazione di grafici che visualizzano le relazioni tra le variabili.

- **`regressione.R`**: Include il codice per eseguire analisi di regressione. Sono esplorati modelli di regressione lineare e non lineare, con la possibilità di valutare l'adeguatezza dei modelli tramite criteri statistici e grafici diagnostici.

- **`pareto.R`**: Script che esegue l'analisi di Pareto, utilizzata per identificare i fattori più influenti in un set di dati, basandosi sul principio dell'80/20.

- **`aleatoria.R`**: Questo script si concentra sull'analisi di variabili aleatorie, esplorando la loro distribuzione e testando ipotesi riguardanti la casualità dei dati.

- **`cluster.R`**: Contiene il codice per eseguire tecniche di clustering, come il k-means e l'analisi gerarchica, per identificare gruppi omogenei all'interno dei dati.

## Requisiti

Per eseguire questo progetto, è necessario avere installato:

- R (versione 4.0 o successiva)
- I seguenti pacchetti R:
  - `dplyr`
  - `ggplot2`
  - `cluster`
  - `factoextra`
  - Altri pacchetti potrebbero essere richiesti specificamente da ciascun script.

Puoi installare i pacchetti mancanti eseguendo:

```r
install.packages(c("dplyr", "ggplot2", "cluster", "factoextra"))
```

## Come Utilizzare il Progetto

1. **Clona il repository:**
   ```bash
   git clone https://github.com/a-ture/GreenGrowthIndicators/
   cd GreenGrowthIndicators
   ```

2. **Preparazione del Dataset:**
   - Esegui lo script `preparazione_dati.R` per pulire e preparare il dataset. Assicurati che il file del dataset sia caricato correttamente nello script.
   - Questo passaggio è fondamentale prima di procedere con le analisi successive.

3. **Esecuzione delle Analisi:**
   - Dopo aver preparato i dati, puoi eseguire ciascuno degli altri script (`analisi_univariata.R`, `correlazione.R`, ecc.) a seconda dell'analisi che desideri eseguire.

4. **Esamina i Risultati:**
   - Gli output statistici e i grafici generati dagli script saranno disponibili nella console R e nelle directory di output specificate all'interno degli script.

## Esempi di Applicazione

Di seguito è riportato un esempio di come eseguire l'intero flusso di lavoro, dalla preparazione dei dati all'analisi univariata:

```r
# Step 1: Preparazione dei dati
source("preparazione_dati.R")

# Step 2: Esegui l'analisi univariata
source("analisi_univariata.R")
```

