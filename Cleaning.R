#---------------------
#STEP 1: Cleaning
#---------------------

if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("caret")) install.packages("caret")
if (!require("e1071")) install.packages("e1071")

library(readr)
library(dplyr)
library(caret)
library(e1071)

#dataset che non modificherò
train_data <- read_csv("/Users/davidecanfora/Desktop/Apprendimento automatico/Progetto/home-data-for-ml-course/train.csv")

train_data <- train_data[,-1]

#Vediamo ogni riga quanti NaN ha
NaN_per_row <- rowSums(is.na(train_data))
train_data$na_count <- NaN_per_row #aggiungo una colonna che conta i NaN di ogni riga
max(train_data$na_count, na.rm = TRUE) #sembra che ogni osservazione abbia al massimo 10-15 NA (su 80)
# quindi non eliminiamo nessuna riga
# anche perchè alcuni Na non corrispondono a dati mancanti, ma sono collegati a risposte precedenti
train_data <- train_data %>% select(-na_count) #tolgo dal dataset la colonna che mi conta i NaN
rm(NaN_per_row)

#Ora vediamo quali colonne hanno troppi NaN:
NaN_per_column <- colSums(is.na(train_data))
threshold <- 0.40 * nrow(train_data) #ho messo la soglia del 40%
colonne_con_troppi_NaN <- names(NaN_per_column[NaN_per_column > threshold])
colonne_con_troppi_NaN
#quattro colonne hanno piu del 40% di NaN

#i NaN della colonna Alley significano che non vogliono l'alley, NON che manca il dato
train_data$Alley[is.na(train_data$Alley)] <- "AlleyUnwanted" #sostituisco i NaN con "Non voluto"
#idem per fireplace quality
train_data$FireplaceQu[is.na(train_data$FireplaceQu)] <- "FireplaceUnwanted"
#controllare anche le altre tre dal file "descrizione"
train_data$PoolQC[is.na(train_data$PoolQC)] <- "PoolUnwanted"
train_data$Fence[is.na(train_data$Fence)] <- "FenceUnwanted"
train_data$MiscFeature[is.na(train_data$MiscFeature)] <- "FeaturesUnwanted"
#faccio lo stesso anche con le altre variabili dove "NaN" NON significa "dato mancante"
train_data$BsmtQual[is.na(train_data$BsmtQual)] <- "BasementUnwanted"
train_data$BsmtCond[is.na(train_data$BsmtCond)] <- "BasementUnwanted"
train_data$BsmtExposure[is.na(train_data$BsmtExposure)] <- "BasementUnwanted"
train_data$BsmtFinType1[is.na(train_data$BsmtFinType1)] <- "BasementUnwanted"
train_data$BsmtFinType2[is.na(train_data$BsmtFinType2)] <- "BasementUnwanted"
train_data$GarageType[is.na(train_data$GarageType)] <- "GarageUnwanted"
train_data$GarageFinish[is.na(train_data$GarageFinish)] <- "GarageUnwanted"
train_data$GarageQual[is.na(train_data$GarageQual)] <- "GarageUnwanted"
train_data$GarageCond[is.na(train_data$GarageCond)] <- "GarageUnwanted"

# Lista delle variabili categoriche
# Converte le colonne character in factor
train_data[] <- lapply(train_data, function(x) if(is.character(x)) as.factor(x) else x)

# Ora trova le colonne di tipo factor
categorical_vars <- names(train_data)[sapply(train_data, is.factor)]

print(categorical_vars)

# Inizializza una lista per memorizzare i risultati
anova_results <- list()

# Ciclo for per eseguire ANOVA su ciascuna variabile categorica
# Crea un vettore per memorizzare le variabili categoriche significative
significant_categoricals <- c()

# Esegui ANOVA per tutte le variabili categoriche
for (var in categorical_vars) {
  # Esegui il test ANOVA
  anova_result <- aov(SalePrice ~ get(var), data = train_data)
  
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
significant_categorical_data <- train_data[, significant_categoricals]

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

#ora metto in dataset le categoriche che ho trasformato in numeriche
cat_to_num_data <- significant_categorical_data[, sapply(significant_categorical_data, is.numeric)]

#estraggo le categoriche da significant
categorical_train_data <- significant_categorical_data %>% select(where(is.factor))

# Creiamo il modello per One-Hot Encoding
library(caret)
dummies <- dummyVars("~ .", data = categorical_train_data)
# Applichiamo il modello di One-Hot Encoding per ottenere il dataframe con le variabili dummy
cat_one_hot <- as.data.frame(predict(dummies, newdata = categorical_train_data))

#ora mi occupo delle variabili che sono numeriche 
numeric_data <- train_data[, sapply(train_data, is.numeric)]
head(numeric_data)

#train_data: dataset iniziale
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
length(high_corr)  # Mostra quante variabili sono state eliminate (nessuna)

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

#ora unisco i 3 dataset
complete_train <- cbind(numeric_data,cat_to_num_data,cat_one_hot)

# Sostituisci i NA con la media della colonna
complete_train <- complete_train %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

rm(list = setdiff(ls(), c("complete_train", "significant_categorical_data","categorical_train_data","selected_vars","high_vif_vars","cat_one_hot")))
graphics.off()
summary(complete_train)
save_train <- complete_train

#---------------------
#STEP 2: SVM-Model
#---------------------

# Va bene, ma richiede tuning e non è il modello più performante per regressione.
# E' pensato per la classificazione (es: "Sì/No") ma, volendo, si può adattare ad un problema di regressione
# Dunque, se vuoi usare SVM, assicurati di:
#1) Standardizzare le feature (SVM è sensibile alla scala delle variabili).
#2) Usare una grid search per ottimizzare i parametri.

#Sappiamo che l'SVM è sensibile alla scala delle variabili:
#Le variabili numeriche devono essere standardizzate affinché l'SVM possa trattarle correttamente

complete_train <- save_train

#Normalizzazione delle variabili numeriche
preproc <- preProcess(complete_train, method = c("center", "scale") )
complete_train <- predict(preproc, complete_train)

# Identifica le variabili costanti
constant_vars <- names(Filter(function(x) length(unique(x)) == 1, complete_train))
# Stampa le variabili costanti trovate
cat("Variabili costanti trovate:", constant_vars, "\n")
# Rimuove le variabili costanti dal dataset
complete_train <- complete_train[, !colnames(complete_train) %in% constant_vars]

#Quando si prepara un dataset per l'addestramento di un modello SVM, è necessario
#separare le variabili indipendenti dalla variabile dipendente (target)
X <- complete_train %>% select(-SalePrice)  # Le variabili indipendenti (senza la variabile target)
y <- complete_train$SalePrice  # La variabile target

#Una volta preparato il dataset, posso costruire il tuo modello SVM utilizzando la funzione svm()
library(e1071)
# Creazione del modello SVM (regressione o classificazione, dipende dal tipo di target)
svm_model <- svm(SalePrice ~ ., data = complete_train, kernel = "radial", scale = FALSE)

#check linear/polynomial kernels (fatti a lezione)

# Visualizzazione dei risultati del modello
summary(svm_model)

#---------------------
#STEP 3: Risultati del train
#---------------------

# Previsioni sul training set
y_pred <- predict(svm_model, X)

# Calcolo dell'errore quadratico medio (RMSE)
rmse <- sqrt(mean((y_pred - y)^2))

# Calcolo del coefficiente di determinazione (R²)
rsquared <- cor(y_pred, y)^2

# Stampa delle metriche
cat(paste("RMSE:", round(rmse, 2), "\n"))
cat(paste("R²:", round(rsquared, 2), "\n"))

# Valutazione delle performance del modello
rmse_threshold <- 50000  # Soglia indicativa per un buon RMSE
rsquared_threshold <- 0.7  # Un buon modello ha R² >= 0.7

if (rmse < rmse_threshold & rsquared > rsquared_threshold) {
  cat("Il modello ha buone prestazioni!\n")
} else {
  cat("Il modello potrebbe non essere ottimale, considera di migliorarlo.\n")
}

#Regola generale: RMSE < 0.5 → Eccellente ✅ RMSE tra 0.5 e 1 → Buono 👍 RMSE > 1 → Scarso ⚠️
#Regola generale: R² > 0.8 → Modello eccellente ✅ R² tra 0.6 e 0.8 → Buon modello 👍 R² < 0.5 → Modello debole ⚠️

#---------------------
#STEP 4: Cleaning del test
#---------------------

test_data <- read_csv("/Users/davidecanfora/Desktop/Apprendimento automatico/Progetto/home-data-for-ml-course/test.csv")

test_data <- test_data[,-1]

test_data$Alley[is.na(test_data$Alley)] <- "AlleyUnwanted" #sostituisco i NaN con "Non voluto"
test_data$FireplaceQu[is.na(test_data$FireplaceQu)] <- "FireplaceUnwanted"
test_data$PoolQC[is.na(test_data$PoolQC)] <- "PoolUnwanted"
test_data$Fence[is.na(test_data$Fence)] <- "FenceUnwanted"
test_data$MiscFeature[is.na(test_data$MiscFeature)] <- "FeaturesUnwanted"
test_data$BsmtQual[is.na(test_data$BsmtQual)] <- "BasementUnwanted"
test_data$BsmtCond[is.na(test_data$BsmtCond)] <- "BasementUnwanted"
test_data$BsmtExposure[is.na(test_data$BsmtExposure)] <- "BasementUnwanted"
test_data$BsmtFinType1[is.na(test_data$BsmtFinType1)] <- "BasementUnwanted"
test_data$BsmtFinType2[is.na(test_data$BsmtFinType2)] <- "BasementUnwanted"
test_data$GarageType[is.na(test_data$GarageType)] <- "GarageUnwanted"
test_data$GarageFinish[is.na(test_data$GarageFinish)] <- "GarageUnwanted"
test_data$GarageQual[is.na(test_data$GarageQual)] <- "GarageUnwanted"
test_data$GarageCond[is.na(test_data$GarageCond)] <- "GarageUnwanted"

# Converte le colonne character in factor
test_data[] <- lapply(test_data, function(x) if(is.character(x)) as.factor(x) else x)

# Estrai solo le colonne categoriche da test_data
categorical_test_data <- test_data %>% select(where(is.factor))
# Trova le colonne comuni tra categorical_test_data e significant_categorical_data
common_columns <- intersect(names(categorical_test_data), names(significant_categorical_data))
# Mantieni solo le colonne che sono presenti in significant_categorical_data
categorical_test_data <- categorical_test_data[, common_columns]
#ora categorical_test_data ha 40 variabili come significant_categorical_var dopo l'anova

# Encoding
categorical_test_data$ExterQual <- as.numeric(factor(categorical_test_data$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), labels = c(1, 2, 3, 4, 5)))
categorical_test_data$ExterCond <- as.numeric(factor(categorical_test_data$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), labels = c(1, 2, 3, 4, 5)))
categorical_test_data$BsmtQual <- as.numeric(factor(categorical_test_data$BsmtQual, levels = c("BasementUnwanted", "Po", "Fa", "TA", "Gd", "Ex"), labels = c(0, 1, 2, 3, 4, 5)))
categorical_test_data$BsmtCond <- as.numeric(factor(categorical_test_data$BsmtCond, levels = c("BasementUnwanted", "Po", "Fa", "TA", "Gd", "Ex"), labels = c(0, 1, 2, 3, 4, 5)))
categorical_test_data$HeatingQC <- as.numeric(factor(categorical_test_data$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"), labels = c(1, 2, 3, 4, 5)))
categorical_test_data$KitchenQual <- as.numeric(factor(categorical_test_data$KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), labels = c(1, 2, 3, 4, 5)))
categorical_test_data$FireplaceQu <- as.numeric(factor(categorical_test_data$FireplaceQu, levels = c("FireplaceUnwanted", "Po", "Fa", "TA", "Gd", "Ex"), labels = c(0, 1, 2, 3, 4, 5)))
categorical_test_data$GarageQual <- as.numeric(factor(categorical_test_data$GarageQual, levels = c("GarageUnwanted", "Po", "Fa", "TA", "Gd", "Ex"), labels = c(0, 1, 2, 3, 4, 5)))
categorical_test_data$GarageCond <- as.numeric(factor(categorical_test_data$GarageCond, levels = c("GarageUnwanted", "Po", "Fa", "TA", "Gd", "Ex"), labels = c(0, 1, 2, 3, 4, 5)))
categorical_test_data$PoolQC <- as.numeric(factor(categorical_test_data$PoolQC, levels = c("PoolUnwanted", "Fa", "TA", "Gd", "Ex"), labels = c(0, 1, 2, 3, 4)))
# Ora metto in dataset le categoriche che ho trasformato in numeriche
cat_to_num_data <- categorical_test_data[, sapply(categorical_test_data, is.numeric)]
# Ora metto in dataset le categoriche
categorical_test_data <- categorical_test_data[, sapply(categorical_test_data, is.factor)]

library(caret)
library(dplyr)

# 1️⃣ Crea il modello di One-Hot Encoding basato su categorical_train_data
dummies <- dummyVars(" ~ .", data = categorical_train_data)
# 2️⃣ Applica il One-Hot Encoding a categorical_test_data (ignora livelli nuovi)
categorical_test_encoded <- predict(dummies, newdata = categorical_test_data) %>% as.data.frame()
# 3️⃣ Ottieni le colonne di categorical_train_encoded (quelle originali di train)
train_columns <- colnames(predict(dummies, newdata = categorical_train_data))
# 4️⃣ Trova le colonne mancanti in categorical_test_encoded
missing_cols <- setdiff(train_columns, colnames(categorical_test_encoded))
# 5️⃣ Aggiungi colonne mancanti con valore 0 nel test
for (col in missing_cols) {
  categorical_test_encoded[[col]] <- 0
}
# 6️⃣ Riordina le colonne in categorical_test_encoded per avere lo stesso ordine di train
categorical_test_encoded <- categorical_test_encoded[, train_columns]

#ora mi occupo delle numeriche
numeric_test_data <- test_data[, sapply(test_data, is.numeric)]
numeric_test_data <- numeric_test_data[, selected_vars[selected_vars != "SalePrice"]]
numeric_test_data <- numeric_test_data[, !names(numeric_test_data) %in% high_vif_vars]
#ha 19 variabili e non 20 perchè non c'è SalePrice

complete_test <- cbind(numeric_test_data,cat_to_num_data,categorical_test_encoded)
#ha 238 variabili anzichè 239

# Sostituisci i NA con la media della colonna
complete_test <- complete_test %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

save_test <- complete_test

#---------------------
#STEP 5: Risultati del test
#---------------------

complete_test <- save_test

#Normalizzazione delle variabili numeriche
preproc <- preProcess(complete_test, method = c("center", "scale") )
#warning: alcune variabili avranno varianza zero per il one hot encoding
complete_test <- predict(preproc, complete_test)

#tolgo colonne che avevo tolto in train poichè costanti
complete_test <- complete_test[, !colnames(complete_test) %in% constant_vars]

predictions <- predict(svm_model, complete_test)

#---------------------
# Step 6: Creazione del file di sottomissione
#---------------------

# Estrai i parametri di normalizzazione dal modello preproc
saleprice_mean <- preproc$mean["SalePrice"]
saleprice_sd <- preproc$std["SalePrice"]

# Denormalizza le predizioni
predictions_original <- (predictions * saleprice_sd) + saleprice_mean

# Carica il file sample_submission per ottenere gli indici originali
sample_submission <- read_csv("/Users/davidecanfora/Desktop/Apprendimento automatico/Progetto/home-data-for-ml-course/sample_submission.csv")

# Creazione del file di sottomissione con gli stessi ID di sample_submission
submission <- data.frame(Id = sample_submission$Id, SalePrice = predictions_original)

# Salvataggio del file CSV senza l'indice di riga
write_csv(submission, "submission.csv")

cat("File di sottomissione 'submission.csv' creato con successo!\n")

