if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("caret")) install.packages("caret")
if (!require("e1071")) install.packages("e1071")

library(readr)
library(dplyr)
library(caret)
library(e1071)

#tengo una copia dell'originale
test_data <- read_csv("/Users/davidecanfora/Desktop/Apprendimento automatico/Progetto/home-data-for-ml-course/train.csv")
#dataset che non modificherò
train <- test_data

test_data <- test_data[,-1]

#Vediamo ogni riga quanti NaN ha
NaN_per_row <- rowSums(is.na(test_data))
test_data$na_count <- NaN_per_row #aggiungo una colonna che conta i NaN di ogni riga
max(test_data$na_count, na.rm = TRUE) #sembra che ogni osservazione abbia al massimo 10-15 NA (su 80)
# quindi non eliminiamo nessuna riga
# anche perchè alcuni Na non corrispondono a dati mancanti, ma sono collegati a risposte precedenti
test_data <- test_data %>% select(-na_count) #tolgo dal dataset la colonna che mi conta i NaN
rm(NaN_per_row)

#Ora vediamo quali colonne hanno troppi NaN:
NaN_per_column <- colSums(is.na(test_data))
threshold <- 0.40 * nrow(test_data) #ho messo la soglia del 40%
colonne_con_troppi_NaN <- names(NaN_per_column[NaN_per_column > threshold])
colonne_con_troppi_NaN
#quattro colonne hanno piu del 40% di NaN

#i NaN della colonna Alley significano che non vogliono l'alley, NON che manca il dato
test_data$Alley[is.na(test_data$Alley)] <- "UNWANTED" #sostituisco i NaN con "Non voluto"
#idem per fireplace quality
test_data$FireplaceQu[is.na(test_data$FireplaceQu)] <- "UNWANTED"

#controllare anche le altre tre dal file "descrizione"
test_data$PoolQC[is.na(test_data$PoolQC)] <- "PoolUnwanted"
test_data$Fence[is.na(test_data$Fence)] <- "FenceUnwanted"
test_data$MiscFeature[is.na(test_data$MiscFeature)] <- "FeaturesUnwanted"
#faccio lo stesso anche con le altre variabili dove "NaN" NON significa "dato mancante"
test_data$BsmtQual[is.na(test_data$BsmtQual)] <- "BasementUnwanted"
test_data$BsmtCond[is.na(test_data$BsmtCond)] <- "BasementUnwanted"
test_data$BsmtExposure[is.na(test_data$BsmtExposure)] <- "BasementUnwanted"
test_data$BsmtFinType1[is.na(test_data$BsmtFinType1)] <- "BasementUnwanted"
test_data$BsmtFinType2[is.na(test_data$BsmtFinType2)] <- "BasementUnwanted"
test_data$GarageType[is.na(test_data$GarageType)] <- "GarageUnwanted"
test_data$GarageFinish[is.na(test_data$GarageFinish)] <- "GarageUnwanted"
test_data$GarageQual[is.na(test_data$GarageQual)] <- "GarageUnwanted"
test_data$GarageCond[is.na(test_data$GarageCond)] <- "GarageUnwanted"

# Lista delle variabili categoriche
# Converte le colonne character in factor
test_data[] <- lapply(test_data, function(x) if(is.character(x)) as.factor(x) else x)

# Ora trova le colonne di tipo factor
categorical_vars <- names(test_data)[sapply(test_data, is.factor)]

print(categorical_vars)

# Inizializza una lista per memorizzare i risultati
anova_results <- list()

# Ciclo for per eseguire ANOVA su ciascuna variabile categorica
# Crea un vettore per memorizzare le variabili categoriche significative
significant_categoricals <- c()

# Esegui ANOVA per tutte le variabili categoriche
for (var in categorical_vars) {
  # Esegui il test ANOVA
  anova_result <- aov(SalePrice ~ get(var), data = test_data)
  
  # Estrai il p-value
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Se il p-value è significativo (ad esempio < 0.05), aggiungi la variabile alla lista
  if (p_value < 0.05) {
    significant_categoricals <- c(significant_categoricals, var)
  } else {
    # Stampa le variabili non significative
    print(paste("Non significativa:", var, "con p-value:", p_value))
  }
  
}
# Ora "significant_categoricals" contiene le variabili significative
print(significant_categoricals)

# Crea una matrice di queste variabili per uso successivo (ad esempio, per One-Hot Encoding)
significant_categorical_data <- test_data[, significant_categoricals]

cat_vars <- significant_categorical_data

#------------------------ANALISI ESPLORATIVA DEI DATI per var categoriche

#1) Tabelle di frequenza e percentuali

library(ggplot2)
categorical_freq <- data.frame(
  Variable = rep(names(cat_vars), times = sapply(cat_vars, nlevels)),
  Level = unlist(lapply(cat_vars, levels)),
  Count = unlist(lapply(cat_vars, function(x) table(x)))
)
library(gridExtra)
library(grid)
# Creiamo una cartella per salvare le tabelle
dir.create("categorical_tables", showWarnings = FALSE)
# Ciclo su tutte le variabili categoriche per creare e salvare le tabelle
for (var in names(cat_vars)) {
  df <- as.data.frame(table(cat_vars[[var]], useNA = "no"))  # Escludiamo gli NA
  df$Percentage <- round(df$Freq / sum(df$Freq) * 100, 2)  # Calcoliamo le percentuali
  colnames(df) <- c(var, "Count", "Percentage")  # Rinominiamo le colonne
  # Creiamo la tabella grafica
  table_plot <- tableGrob(df, rows = NULL)  # Nessun numero accanto ai termini
  # Salviamo la tabella come immagine PNG
  png(filename = paste0("categorical_tables/", var, "_table.png"), width = 800, height = 400)
  grid.newpage()
  grid.draw(table_plot)
  dev.off()  # Chiudiamo il dispositivo grafico
}

#Alcune categorie potrebbero essere sbilanciate, il che potrebbe influenzare la previsione del modello.
#Variabili con molte categorie potrebbero essere raggruppate per evitare alta dimensionalità.

#2) Bar Plot

#Visualizza la distribuzione con bar plot
library(ggplot2)
# Creiamo una cartella per salvare i grafici
dir.create("categorical_barplots", showWarnings = FALSE)
# Ciclo su tutte le variabili categoriche per creare e salvare i bar plot
for (var in names(cat_vars)) {
  df <- as.data.frame(table(cat_vars[[var]], useNA = "no"))  # Escludiamo gli NA
  colnames(df) <- c("Level", "Count")  # Rinominiamo le colonne
  # Creiamo il bar plot
  p <- ggplot(df, aes(x = Level, y = Count, fill = Level)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Bar plot of", var), x = var, y = "Count") +
    coord_flip()  # Ruotiamo l'asse per maggiore leggibilità
  # Salviamo il bar plot come immagine PNG
  ggsave(filename = paste0("categorical_barplots/", var, "_barplot.png"), 
         plot = p + theme(panel.background = element_rect(fill = "white", color = NA),  # Sfondo del pannello bianco
                          plot.background = element_rect(fill = "white", color = NA),   # Sfondo dell'immagine bianco
         ), width = 8, height = 6, dpi = 300)
}

#Alcune categorie potrebbero essere sbilanciate, il che potrebbe influenzare la previsione del modello.
#Variabili con molte categorie potrebbero essere raggruppate per evitare alta dimensionalità.

#3)Controllo di cardinalità

#Verifica il numero di livelli per ogni variabile
library(ggplot2)
# Creiamo la cartella per salvare il grafico
dir.create("categorical_cardinality", showWarnings = FALSE)
# Creiamo il grafico della cardinalità
cardinality <- data.frame(Variable = names(cat_vars), Levels = sapply(cat_vars, nlevels))
p <- ggplot(cardinality, aes(x = reorder(Variable, -Levels), y = Levels)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Number of levels for every categorical variable") +
  coord_flip()
# Salviamo il grafico come immagine PNG
ggsave(filename = "categorical_cardinality/cardinality_plot.png", 
       plot = p + theme_minimal() + 
         theme(panel.background = element_rect(fill = "white", color = NA),  # Sfondo del pannello bianco
               plot.background = element_rect(fill = "white", color = NA),   # Sfondo dell'immagine bianco
               ),width = 10, height = 6, dpi = 300)

#Se una variabile ha troppe categorie, potrebbe essere necessario ridurle con tecniche di clustering o encoding specifici.

#4) Grafici a torta

#Anche se spesso i bar plot sono preferibili, in alcuni casi puoi usare grafici a torta per visualizzare le percentuali

# Creiamo una cartella per salvare i grafici a torta
dir.create("categorical_pie_charts", showWarnings = FALSE)
# Ciclo su tutte le variabili categoriche per creare e salvare i grafici a torta
for (var in names(cat_vars)) {
  # Impostiamo lo sfondo bianco per ogni grafico
  png(filename = paste0("categorical_pie_charts/", var, "_pie_chart.png"), width = 800, height = 600)
  # Impostiamo lo sfondo bianco del grafico
  par(bg = "white")
  # Creiamo il grafico a torta
  pie(table(cat_vars[[var]]), main = paste("Distribution of", var))
  # Salviamo il grafico
  dev.off()
}

#Aiuta a visualizzare la distribuzione percentuale delle categorie.

#5) Cross tabulation
#Per vedere la relazione tra due variabili categoriche

#OMESSO
#avendo 40 categoriche, dovremmo generare 40 x 39 / 2 = 780 grafici

#Le barre colorate aiutano a individuare pattern tra variabili categoriche, ad esempio, se alcuni tipi di tetto sono più comuni in certi quartieri.
#Se una variabile non mostra variazioni significative rispetto a un'altra, potrebbe essere poco informativa e quindi eliminabile.
#Se una categoria è dominata da un'unica classe, potremmo raggruppare le altre categorie meno frequenti per evitare problemi di sparsità.


#----------------------FINE ANALISI ESPLORATIVA DEI DATI per var categoriche

#encoding 
significant_categorical_data$ExterQual <- as.numeric(factor(significant_categorical_data$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), labels = c(1, 2, 3, 4, 5)))
significant_categorical_data$ExterCond <- as.numeric(factor(significant_categorical_data$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), labels = c(1, 2, 3, 4, 5)))
significant_categorical_data$BsmtQual <- as.numeric(factor(significant_categorical_data$BsmtQual, levels = c("BasementUnwanted", "Po", "Fa", "TA", "Gd", "Ex"), labels = c(0, 1, 2, 3, 4, 5)))
significant_categorical_data$BsmtCond <- as.numeric(factor(significant_categorical_data$BsmtCond, levels = c("BasementUnwanted", "Po", "Fa", "TA", "Gd", "Ex"), labels = c(0, 1, 2, 3, 4, 5)))
significant_categorical_data$HeatingQC <- as.numeric(factor(significant_categorical_data$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"), labels = c(1, 2, 3, 4, 5)))
significant_categorical_data$KitchenQual <- as.numeric(factor(significant_categorical_data$KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), labels = c(1, 2, 3, 4, 5)))
significant_categorical_data$FireplaceQu <- as.numeric(factor(significant_categorical_data$FireplaceQu, levels = c("FireplaceUnwanted", "Po", "Fa", "TA", "Gd", "Ex"), labels = c(0, 1, 2, 3, 4, 5)))
significant_categorical_data$GarageQual <- as.numeric(factor(significant_categorical_data$GarageQual, levels = c("GarageUnwanted", "Po", "Fa", "TA", "Gd", "Ex"), labels = c(0, 1, 2, 3, 4, 5)))
significant_categorical_data$GarageCond <- as.numeric(factor(significant_categorical_data$GarageCond, levels = c("GarageUnwanted", "Po", "Fa", "TA", "Gd", "Ex"), labels = c(0, 1, 2, 3, 4, 5)))
significant_categorical_data$PoolQC <- as.numeric(factor(significant_categorical_data$PoolQC, levels = c("PoolUnwanted", "Fa", "TA", "Gd", "Ex"), labels = c(0, 1, 2, 3, 4)))
#ora metto in dataset le categoriche che ho trasformato "a scala"
cat_to_num_data <- significant_categorical_data[, sapply(significant_categorical_data, is.numeric)]

#estraggo le categoriche da significant
cat_one_hot <- significant_categorical_data %>% select(where(~ !is.numeric(.)))

# Creiamo il modello per One-Hot Encoding
library(caret)
dummies <- dummyVars("~ .", data = cat_one_hot)
# Applichiamo il modello di One-Hot Encoding per ottenere il dataframe con le variabili dummy
categorical_data_encoded <- as.data.frame(predict(dummies, newdata = cat_one_hot))

#ora mi occupo delle variabili che sono numeriche 
numeric_data <- test_data[, sapply(test_data, is.numeric)]
head(numeric_data)

#test_data: dataset iniziale
#numeric_data : dataset delle SOLE variabili che erano numeriche fin dall'inizio
#cat_to_num_data : dataset delle variabili che ho "reso numeriche" a scala (ex: bad=0,good=1, excellent=2)
#categorical_data_encoded : dataset delle variabili che ho reso numeriche con l'one hot encoding

# ora plottiamo la matrice delle sole variabili numeriche "naturali"

#ora vediamo quali variabili sono collegate col target (utili) e quali no
cor_target <- cor(numeric_data, use = "complete.obs")["SalePrice", ]
cor_target_sorted <- sort(cor_target, decreasing = TRUE)
print(cor_target_sorted)
threshold <- 0.2  # Soglia di correlazione minima
selected_vars <- names(cor_target_sorted[abs(cor_target_sorted) >= threshold])
low_corr_vars <- names(cor_target[abs(cor_target) < 0.2])
numeric_data <- numeric_data[, selected_vars]
cat("Numero di variabili con bassa correlazione con SalePrice:", length(low_corr_vars), "\n")
print(low_corr_vars)  # Stampa i nomi delle variabili con bassa correlazione

library(ggplot2)
cor_target <- data.frame(Variable = names(cor_target_sorted), Correlation = cor_target_sorted)
ggplot(cor_target, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Correlation between variables and target")

#costruiamo la matrice di correlazione
cor_matrix <- cor(numeric_data[, -which(names(numeric_data) == "SalePrice")], use = "complete.obs")
if (!require("ggcorrplot")) {
  install.packages("ggcorrplot")
}

library(ggcorrplot)
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE)

library(caret)  # Assicurati che il pacchetto sia installato
high_corr <- findCorrelation(cor_matrix, cutoff = 0.9)  # Identifica le colonne da rimuovere
if(length(high_corr)<0){
  numeric_data <- numeric_data[, -high_corr]  # Rimuove le variabili con alta correlazione (>0.9)
}

# È la matrice di correlazione tra le variabili numeriche del dataset
# type = "lower": Mostra solo la parte inferiore della matrice di correlazione (dato che la matrice è simmetrica, evita di ripetere le informazioni)
# Identificare e rimuovere le variabili altamente correlate

# Controlliamo quante variabili sono state rimosse
length(high_corr)  # Mostra quante variabili sono state eliminate

library(car)  # Assicurati che il pacchetto sia installato
# Calcolo del Variance Inflation Factor (VIF) per le variabili numeriche
vif_values <- vif(lm(SalePrice ~ ., data = numeric_data))
# Stampa dei valori VIF
print(vif_values)
#Variabili con VIF elevato: GrLivArea: VIF = 114.60, TotRmsAbvGrd: VIF = 65.16
#Questi due valori di VIF sono molto alti quindi li rimuovo dal dataset per ridurre la multicollinearità
# Identifica le variabili con VIF >= 10
high_vif_vars <- names(vif_values[vif_values >= 10])
# Rimuovi le variabili con VIF >= 10
numeric_data <- numeric_data[, !names(numeric_data) %in% high_vif_vars]
# Ricalcola il VIF dopo la rimozione
vif_values <- vif(lm(SalePrice ~ ., data = numeric_data))
print(vif_values)
#ora tutti i vif sono inferiori a 10

#------------------------ANALISI ESPLORATIVA DEI DATI per var numeriche

#1) # Calcola statistiche descrittive per ogni variabile numerica

library(gridExtra)
library(grid)
#Calcola statistiche descrittive per ogni variabile numerica
stats_table <- data.frame(
  Variable = names(numeric_data),  # Aggiungi i nomi delle variabili
  Mean    = sapply(numeric_data, function(x) mean(x, na.rm = TRUE)),
  Median  = sapply(numeric_data, function(x) median(x, na.rm = TRUE)),
  Variance= sapply(numeric_data, function(x) var(x, na.rm = TRUE)),
  SD      = sapply(numeric_data, function(x) sd(x, na.rm = TRUE)),
  Min     = sapply(numeric_data, function(x) min(x, na.rm = TRUE)),
  Max     = sapply(numeric_data, function(x) max(x, na.rm = TRUE))
)
#Creare una cartella per salvare la tabella grafica
dir.create("numeric_stats_tables", showWarnings = FALSE)
#Creiamo la tabella grafica
table_plot <- tableGrob(stats_table, rows = NULL)  # Nessun numero accanto ai termini
#Salviamo la tabella come immagine PNG con sfondo bianco
png(filename = "numeric_stats_tables/numeric_stats_table.png", width = 1000, height = 500)
grid.newpage()
grid.draw(table_plot)
dev.off()  # Chiudiamo il dispositivo grafico

#Possiamo osservare le differenze di scala tra le variabili:
#Alcune variabili hanno una varianza molto alta, indicando grande dispersione dei dati.
#La presenza di un'elevata differenza tra media e mediana suggerisce una possibile asimmetria della distribuzione.

#È importante controllare se le variabili numeriche hanno scale molto diverse,
#perché modelli come SVM e regressione sono sensibili a scale diverse

#Se noti scale molto differenti, conviene standardizzare (centrare e scalare) i dati.
#Se le variabili hanno scale molto diverse (ad es. Area in mq vs Numero di bagni), potrebbe essere necessario standardizzarle.
#Se una variabile ha una deviazione standard molto più alta delle altre, potrebbe dominare il modello e influenzarne le prestazioni.


#2)Visualizzazione grafica: istogrammi, boxplot e density plots

#Istogrammi e density plots ti mostrano la forma della distribuzione (simmetrica, asimmetrica, a code lunghe, multimodale, ecc.)
#Boxplot evidenzia outlier e fornisce informazioni su mediana e quartili
#Nel contesto di un modello di pricing (ad es. per prezzare una casa), se una persona richiede una casa a 10 piani (un valore molto distante dalla media),
#questo outlier potrebbe influenzare la media ma, a seconda del modello, potrebbe essere gestito diversamente (ad esempio, modelli robusti o trasformazioni dei dati).
#Alcuni modelli (come gli alberi decisionali) sono meno sensibili agli outlier rispetto a una regressione lineare. Pertanto, l'EDA ti aiuta a decidere se rimuovere, trasformare o mantenere gli outlier

library(gridExtra)
library(ggplot2)
# Crea le cartelle per salvare i grafici
dir.create("numeric_histograms", showWarnings = FALSE)
dir.create("numeric_boxplots", showWarnings = FALSE)
dir.create("numeric_density_plots", showWarnings = FALSE)
# Crea i grafici per ogni variabile numerica
plots <- lapply(names(numeric_data), function(var) {
  # Verifica se la variabile esiste nel dataset
  if (var %in% colnames(numeric_data)) {
    
#A) Istogramma
    p1 <- ggplot(numeric_data, aes(x = .data[[var]])) + 
      geom_histogram(bins = 30, fill = "blue", alpha = 0.7) + 
      theme_minimal() + 
      ggtitle(paste("Histogram of", var)) +
      theme(panel.background = element_rect(fill = "white", color = NA),  # Sfondo del pannello bianco
            plot.background = element_rect(fill = "white", color = NA))   # Sfondo dell'immagine bianco
    # Salva l'istogramma
    ggsave(filename = paste0("numeric_histograms/", var, "_histogram.png"), plot = p1, width = 8, height = 6, dpi = 300)
    
#B) Boxplot
    p2 <- ggplot(numeric_data, aes(y = .data[[var]])) + 
      geom_boxplot(fill = "red", alpha = 0.7) + 
      theme_minimal() + 
      ggtitle(paste("Boxplot of", var)) +
      theme(panel.background = element_rect(fill = "white", color = NA),  # Sfondo del pannello bianco
            plot.background = element_rect(fill = "white", color = NA))   # Sfondo dell'immagine bianco
    # Salva il boxplot
    ggsave(filename = paste0("numeric_boxplots/", var, "_boxplot.png"), plot = p2, width = 8, height = 6, dpi = 300)
    
#C) Density plot
    p3 <- ggplot(numeric_data, aes(x = .data[[var]])) + 
      geom_density(fill = "green", alpha = 0.7) + 
      theme_minimal() + 
      ggtitle(paste("Density Plot of", var)) +
      theme(panel.background = element_rect(fill = "white", color = NA),  # Sfondo del pannello bianco
            plot.background = element_rect(fill = "white", color = NA))   # Sfondo dell'immagine bianco
    # Salva il density plot
    ggsave(filename = paste0("numeric_density_plots/", var, "_density_plot.png"), plot = p3, width = 8, height = 6, dpi = 300)
  } else {
    cat("La variabile", var, "non è presente nel dataset.\n")
  }
  # Restituisci i grafici (utile se vuoi visualizzarli immediatamente)
  return(list(p1, p2, p3))
})

#Alcune variabili mostrano asimmetria (skewness) e code lunghe, indicando la presenza di outlier.
#Il boxplot aiuta a identificare valori estremi che potrebbero influenzare la qualità del modello.
#Il density plot mostra che molte variabili potrebbero non essere distribuite normalmente, confermando il risultato del test di Shapiro-Wilk.

#D) Distribuzioni per tutte le variabili numeriche (faceted)
library(reshape2)
library(ggplot2)
# Creiamo una cartella per salvare i grafici
dir.create("numeric_distributions", showWarnings = FALSE)
# "Melt" del dataset numerico per ottenere un formato long
numeric_data_melt <- melt(numeric_data)
# Creiamo il grafico delle distribuzioni per tutte le variabili numeriche
p <- ggplot(numeric_data_melt, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions of Numerical Variables") +
  theme(panel.background = element_rect(fill = "white", color = NA),  # Sfondo del pannello bianco
        plot.background = element_rect(fill = "white", color = NA))   # Sfondo dell'immagine bianco
# Salviamo il grafico come immagine PNG
ggsave(filename = "numeric_distributions/numeric_distributions_plot.png", plot = p, width = 10, height = 6, dpi = 300)

#3) Verifica della normalità

#È importante verificare la normalità delle variabili soprattutto se il modello che intendi
#utilizzare assume una distribuzione normale degli errori (ad esempio, nella regressione lineare).

#A) Shapiro-Wilk test
normality_tests <- sapply(numeric_data, function(x) {
  if(length(na.omit(x)) >= 3) {  # il test richiede almeno 3 osservazioni
    round(shapiro.test(x)$p.value, 4)
  } else {
    NA
  }
})
print(normality_tests)

#Il test di Shapiro-Wilk indica che tutte le variabili hanno p-value = 0, il che suggerisce che nessuna di esse segue una distribuzione normale.

#B) Q-Q plot per una valutazione visiva
# Creiamo la cartella per salvare i Q-Q plot
dir.create("qq_plots", showWarnings = FALSE)
# Generiamo e salviamo un Q-Q plot per ogni variabile numerica
for (var in names(numeric_data)) {
  png(filename = paste0("qq_plots/", var, "_qq_plot.png"), width = 800, height = 600)
  # Impostiamo lo sfondo bianco
  par(bg = "white")
  # Creiamo il Q-Q plot
  qqnorm(numeric_data[[var]], main = paste("Q-Q Plot of", var))
  qqline(numeric_data[[var]], col = "red")
  # Salviamo il file e chiudiamo il dispositivo grafico
  dev.off()
}

#Le variabili che deviano fortemente dalla linea rossa non seguono una distribuzione normale.
#I valori che si discostano dalla diagonale suggeriscono la presenza di outlier.

#4) Identifica e confronta le distribuzioni

#Confrontare le distribuzioni ti permette di capire se alcune variabili sono molto asimmetriche o presentano outlier estremi.

library(GGally)
num_vars <- names(numeric_data)
vars_per_plot <- 5
num_plots <- ceiling(length(num_vars) / vars_per_plot)
for (i in 1:num_plots) {
  start_idx <- (i - 1) * vars_per_plot + 1
  end_idx <- min(i * vars_per_plot, length(num_vars))
  subset_vars <- num_vars[start_idx:end_idx]
  p <- ggpairs(numeric_data[, subset_vars])
  # Salviamo o visualizziamo
  print(p)  # o ggsave(...) per salvare
}

#La diagonale mostra la distribuzione di ogni variabile: se simmetrica, la variabile è ben distribuita; se asimmetrica, potrebbe essere necessario applicare una trasformazione (es. logaritmica).
#Le relazioni tra le variabili fuori dalla diagonale aiutano a identificare correlazioni forti (linee ben definite) o assenza di relazione (distribuzione casuale).
#Se alcune variabili sono fortemente correlate, potremmo considerare di rimuoverne una per evitare collinearità.

#5) EXTRA

#A) Scree Plot

numeric_data_without_NaN <- na.omit(numeric_data)

# Esegui la PCA sulle variabili numeriche (scalando i dati)
pca_res <- prcomp(numeric_data_without_NaN, scale. = TRUE)

# Calcola la varianza spiegata da ciascuna componente
scree_data <- data.frame(Component = 1:length(pca_res$sdev), 
                         Variance = pca_res$sdev^2)
scree_data$Proportion <- scree_data$Variance / sum(scree_data$Variance)

# Visualizza il Scree Plot
library(ggplot2)
ggplot(scree_data, aes(x = Component, y = Proportion)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Scree Plot", x = "Principal Component", y = "Proportion of variance explained") +
  theme_minimal()

#B)

# Usa ggfortify per generare un biplot
library(ggfortify)
autoplot(pca_res, data = numeric_data_without_NaN, 
         loadings = TRUE,          # Mostra i vettori di carico (contributo delle variabili)
         loadings.label = TRUE,    # Etichetta i vettori di carico
         loadings.label.size = 3, 
         main = "Biplot PCA")

#---------FINE ANALISI ESPLORATIVA DEI DATI per var numeriche

#ora unisco i 3 dataset
complete_clean <- cbind(numeric_data,cat_one_hot,categorical_data_encoded)
rm(list = setdiff(ls(), "complete_clean"))
summary(complete_clean)