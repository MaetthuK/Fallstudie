
# 1. Setup ----


## 1.1 Packages and Libraries ----
# install.packages(c("tidyverse", "ggplot2", "dplyr", "corrplot", "caret", "pROC"))

library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(pROC)


## 1.2 CSV-Dateien Anbindung und Umbenennung Spalten ----
library(tidyverse)
library(dplyr)

# Read your train/test CSVs
train_data <- read.csv("bloodtrain.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
test_data  <- read.csv("bloodtest.csv",  header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Rename columns (to have consistent readable names with spaces)
colnames(train_data) <- c(
  "ID",
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende",
  "Spende Maerz 2007"
)

colnames(test_data) <- c(
  "ID",
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende"
)

# Mark 'Dataset' so we know which rows are Train vs. Test
train_data$Dataset <- "Train"
test_data$Dataset  <- "Test"

# If your test data lacks the target column:
#   test_data$`Spende Maerz 2007` <- NA
# but for the boxplots, we only need the numeric columns + "Dataset", so it's okay.

##############################################################################
# 2) COMBINE INTO ONE DATA FRAME (“combined_data”) FOR YOUR BOXPLOTS
##############################################################################
# We’ll just keep the columns we want for the 4 boxplots + "Dataset"
combined_data <- bind_rows(
  select(
    train_data,
    `Monate Letzte Spende`,
    `Anzahl Spenden`,
    Gesamtvolumen,
    `Monate Erste Spende`,
    Dataset
  ),
  select(
    test_data,
    `Monate Letzte Spende`,
    `Anzahl Spenden`,
    Gesamtvolumen,
    `Monate Erste Spende`,
    Dataset
  )
)


# 1.3.3 Erste Zeilen anzeigen ----
head(train_data)
head(test_data)


# 2. Datenverständnis ----

## 2.1 Struktur & fehlende Werte ----
str(train_data)
str(test_data)

# Fehlende Werte pro Spalte
colSums(is.na(train_data))
colSums(is.na(test_data))


## 2.2 Duplikate prüfen ----
duplicated_rows <- duplicated(train_data)
if(any(duplicated_rows)) {
  print("Gefundene Duplikate:")
  print(train_data[duplicated_rows, ])
} else {
  print("Keine Duplikate gefunden.")
}


# 3. Explorative Datenanalyse ----

## 3.1 Boxplots: Vergleich Train vs. Test ----
par(mfrow = c(1, 4))

boxplot(`Monate Letzte Spende` ~ Dataset, data = combined_data,
        notch = TRUE, col = c("red", "blue"),
        main = "Monate Letzte Spende",
        xlab = "Datensatz", ylab = "Wert"
)

boxplot(`Anzahl Spenden` ~ Dataset, data = combined_data,
        notch = TRUE, col = c("red", "blue"),
        main = "Anzahl Spenden",
        xlab = "Datensatz", ylab = "Wert"
)

boxplot(Gesamtvolumen ~ Dataset, data = combined_data,
        notch = TRUE, col = c("red", "blue"),
        main = "Gesamtvolumen",
        xlab = "Datensatz", ylab = "Wert"
)

boxplot(`Monate Erste Spende` ~ Dataset, data = combined_data,
        notch = TRUE, col = c("red", "blue"),
        main = "Monate Erste Spende",
        xlab = "Datensatz", ylab = "Wert"
)




## 3.2 Histogramme: Anzahl Spenden ----
# Dieser Codeabschnitt erstellt mehrere Histogramme und ein Balkendiagramm

# 1) Datensätze kennzeichnen: "Train" vs. "Test"
train <- train_data
test  <- test_data
train$Dataset <- "Train"
test$Dataset  <- "Test"

# 2) Zielvariable im Testdatensatz ergänzen (falls dort nicht vorhanden)
if (!"Spende Maerz 2007" %in% colnames(test)) {
  test[["Spende Maerz 2007"]] <- NA
}

# 3) Zusammenführen in einem Dataframe
#    Wir verwenden nur die Spalten, die wir für die Plots benötigen.
library(dplyr)  # Falls noch nicht geladen
combined_data <- bind_rows(
  select(
    train,
    `Monate Letzte Spende`,
    `Anzahl Spenden`,
    Gesamtvolumen,
    `Monate Erste Spende`,
    Dataset,
    `Spende Maerz 2007`
  ),
  select(
    test,
    `Monate Letzte Spende`,
    `Anzahl Spenden`,
    Gesamtvolumen,
    `Monate Erste Spende`,
    Dataset,
    `Spende Maerz 2007`
  )
)

# 4) Mehrere Histogramme erstellen: Monate Letzte Spende, Anzahl Spenden, Gesamtvolumen, Monate Erste Spende
library(ggplot2)
p1 <- ggplot(combined_data, aes(x = `Monate Letzte Spende`, fill = Dataset)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  labs(title = "Monate Letzte Spende", x = "Monate", y = "Frequenz") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  theme_minimal()

p2 <- ggplot(combined_data, aes(x = `Anzahl Spenden`, fill = Dataset)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  labs(title = "Anzahl Spenden", x = "Anzahl", y = "Frequenz") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  theme_minimal()

p3 <- ggplot(combined_data, aes(x = Gesamtvolumen, fill = Dataset)) +
  geom_histogram(binwidth = 500, position = "dodge", color = "black") +
  labs(title = "Gesamtvolumen", x = "Volumen", y = "Frequenz") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  theme_minimal()

p4 <- ggplot(combined_data, aes(x = `Monate Erste Spende`, fill = Dataset)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  labs(title = "Monate Erste Spende", x = "Monate", y = "Frequenz") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  theme_minimal()

# 5) Balkendiagramm für die Zielvariable (nur Trainingsdaten)
p5 <- ggplot(train, aes(x = factor(`Spende Maerz 2007`), fill = "Train")) +
  geom_bar(color = "black") +
  labs(
    title = "Klassenverteilung (Spende im März 2007, Trainingsdaten)",
    x = "Klasse",
    y = "Anzahl"
  ) +
  scale_x_discrete(labels = c("0" = "nein", "1" = "ja")) +
  scale_fill_manual(values = c("Train" = "blue")) +
  theme_minimal() +
  theme(legend.position = "none")

# 6) Alle Plots zusammen in einem Grid anzeigen (2 Spalten, 3 Zeilen)
library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, ncol = 2)



## 3.3 Pairs-Plot & Korrelationsmatrix ----

# Ziel:
#   - Genau wie in den vorigen Plots wollen wir weiterhin Leerzeichen in den 
#     Spaltennamen (z.B. "Monate Letzte Spende", "Anzahl Spenden").
#   - Wir bilden "Spendetakt" = (Anzahl Spenden) / (Monate Letzte Spende).
#   - Division durch 0 vermeiden => Zeilen, wo "Monate Letzte Spende" == 0, => NA.
#   - Danach: Pairs-Plot (links) & Korrelationsmatrix (rechts) in einem gemeinsamen Layout.
# 1) Datensatz neu einlesen
#    Wir gehen davon aus, dass 'bloodtrain.csv' die folgenden Spalten hat:
#      "ID", 
#      "Monate Letzte Spende", 
#      "Anzahl Spenden", 
#      "Gesamtvolumen", 
#      "Monate Erste Spende",
#      "Spende Maerz 2007"
train_data <- read.csv("bloodtrain.csv", stringsAsFactors = FALSE)

# Zur Sicherheit: Gleiche Namensgebung wie in den vorherigen Plots
colnames(train_data) <- c(
  "ID",
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende",
  "Spende Maerz 2007"
)

# 2) 0 -> NA in "Monate Letzte Spende"
#    Prüfen, ob es überhaupt Werte == 0 gibt. Nur dann ersetzen wir.
has_zero <- any(train_data$`Monate Letzte Spende` == 0, na.rm = TRUE)
if (has_zero) {
  train_data$`Monate Letzte Spende`[train_data$`Monate Letzte Spende` == 0] <- NA
}

# 3) Neuer Prädiktor "Spendetakt" = (Anzahl Spenden) / (Monate Letzte Spende)
train_data$Spendetakt <- train_data$`Anzahl Spenden` / train_data$`Monate Letzte Spende`

# 4) 2-spaltiges Grafiklayout
par(
  mfrow = c(1, 2),
  oma   = c(0, 0, 2, 0),
  mar   = c(4, 4, 4, 1)
)

# 5) LINKS: Pairs-Plot
#    Wir wählen die Spalten, alle haben Leerzeichen wie in den früheren Abschnitten:
numeric_vars <- c(
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende",
  "Spendetakt"
)

# Farbgebung nach "Spende Maerz 2007": Wir interpretieren 1 => grün, 0 => rot.
point_colors <- ifelse(train_data$`Spende Maerz 2007` == 1, "green", "red")

pairs(
  train_data[, numeric_vars],
  col       = point_colors,
  pch       = 16,       # geschlossene Kreise
  cex       = 1.2,      # Punktgröße
  font.main = 2,        # Fettschrift für den Titel
  font.lab  = 2,
  main      = "Pairs-Plot der Trainingsdaten",
  cex.main  = 1.4,
  cex.labels= 1.2
)

# 6) RECHTS: Korrelationsmatrix (inkl. Spendetakt)
library(corrplot)  # Falls nicht installiert: install.packages("corrplot")

# Zuerst nur numerische Spalten filtern
num_data <- train_data[sapply(train_data, is.numeric)]

# Falls "ID" numerisch und unnötig, entfernen wir sie
if ("ID" %in% names(num_data)) {
  num_data <- num_data[, !names(num_data) %in% "ID"]
}

# Korrelation (pairwise.complete.obs => Zeilen mit NA in mind. einer Variable ignorieren)
cor_matrix <- cor(num_data, use = "pairwise.complete.obs")

# Plot der Korrelationsmatrix
corrplot(
  cor_matrix,
  method      = "color",   # Farbkacheln
  type        = "upper",   # Nur obere Hälfte
  tl.col      = "black",
  tl.srt      = 45,
  addCoef.col = "black",   # Zahlen in Schwarz
  number.cex  = 0.8,
  title       = "Korrelationsmatrix (mit Spendetakt)",
  mar         = c(0, 0, 2, 0),
  cex.main    = 1.4,
  font.main   = 2
)

# FERTIG: Links Pairs-Plot (mit Leerzeichen in Spaltennamen), 
#         rechts Korrelationsmatrix. 






## 5.1 Logistisches Regressionsmodell ----

# Wir bauen ein Modell, das u.a. die Spalten
#   "Anzahl Spenden",
#   "Monate Letzte Spende",
#   "Monate Erste Spende"
# sowie den neuen Prädiktor "Spenden Rate" verwendet.
# 
# Zielvariable: "Spende Maerz 2007" (0/1-codiert)
#  - Wenn 0 => "Nein", 1 => "Ja"
# 
# ACHTUNG:
#   - Die Spaltennamen haben LEERZEICHEN wie "Monate Letzte Spende".
#     Daher verwenden wir Backticks `...`.
#   - 0-Werte in "Monate Letzte Spende" => NA, damit Division nicht crasht.


# 1) Falls train_data noch nicht existiert, CSV einlesen und 
#    mit LEERZEICHEN in den Spaltennamen benennen:
if (!exists("train_data")) {
  train_data <- read.csv("bloodtrain.csv", header = TRUE, stringsAsFactors = FALSE)
  colnames(train_data) <- c(
    "ID",
    "Monate Letzte Spende",
    "Anzahl Spenden",
    "Gesamtvolumen",
    "Monate Erste Spende",
    "Spende Maerz 2007"
  )
}

# 2) 0 -> NA in "Monate Letzte Spende" (falls 0-Werte vorhanden)
if (any(train_data$`Monate Letzte Spende` == 0, na.rm = TRUE)) {
  train_data$`Monate Letzte Spende`[train_data$`Monate Letzte Spende` == 0] <- NA
}

# 3) Falls "Spenden Rate" noch nicht existiert, erzeugen wir sie:
#    Spenden Rate = (Anzahl Spenden) / (Monate Letzte Spende)
if (!"Spenden Rate" %in% colnames(train_data)) {
  train_data$`Spenden Rate` <- with(train_data, 
                                    ifelse(is.na(`Monate Letzte Spende`), NA, `Anzahl Spenden` / `Monate Letzte Spende`)
  )
}

# 4) Zielvariable in Factor umwandeln (0/1)
train_data$`Spende Maerz 2007` <- as.factor(train_data$`Spende Maerz 2007`)

# 5) NA-Zeilen entfernen => wir behalten nur vollständige Datensätze
train_data_clean <- subset(
  train_data,
  !is.na(`Spende Maerz 2007`) & !is.na(`Spenden Rate`)
)

# 6) Einfaches logistisches Modell
glm_model <- glm(
  `Spende Maerz 2007` ~ `Anzahl Spenden` + `Monate Letzte Spende` + `Spenden Rate` + `Monate Erste Spende`,
  data   = train_data_clean,
  family = binomial(link = "logit")
)

# 7) Modell-Zusammenfassung
summary(glm_model)


# Summary Model als Plot
plot(glm_model)




## 5.2 Variable Importance Plot ----

### 5.2.1) Variable Importance mit caret::varImp
var_imp <- caret::varImp(glm_model, scale = FALSE)

### 5.2.2) Umwandeln in Dataframe für ggplot
var_imp_df <- data.frame(
  Variable   = rownames(var_imp),
  Importance = var_imp$Overall
)

### 5.2.3) Balkendiagramm mit ggplot2
library(ggplot2)
ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Nach links/rechts kippen
  labs(
    title = "Variable Importance (Logistisches Modell)",
    x     = "Prädiktor",
    y     = "Wichtigkeit"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title   = element_text(face = "bold", size = 36),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32),
    axis.text    = element_text(size = 24)
  ) 



# 5.3) KONFUSIONSMATRIX & METRIKEN ----
# 1) Falls train_data_clean noch nicht existiert, laden wir die Daten
if (!exists("train_data_clean")) {
  train_data_clean <- read.csv("bloodtrain.csv", header = TRUE, stringsAsFactors = FALSE)
  colnames(train_data_clean) <- c(
    "ID",
    "Monate Letzte Spende",
    "Anzahl Spenden",
    "Gesamtvolumen",
    "Monate Erste Spende",
    "Spende Maerz 2007"
  )
}

# 2) 0 -> NA in "Monate Letzte Spende" (falls 0-Werte vorhanden)
if (any(train_data_clean$`Monate Letzte Spende` == 0, na.rm = TRUE)) {
  train_data_clean$`Monate Letzte Spende`[train_data_clean$`Monate Letzte Spende` == 0] <- NA
}

# 3) Falls "Spenden Rate" noch nicht existiert, erzeugen wir sie:
#    Spenden Rate = (Anzahl Spenden) / (Monate Letzte Spende)
if (!"Spenden Rate" %in% colnames(train_data_clean)) {
  train_data_clean$`Spenden Rate` <- with(train_data_clean, 
                                          ifelse(is.na(`Monate Letzte Spende`), NA, `Anzahl Spenden` / `Monate Letzte Spende`)
  )
}

# 4) Zielvariable in Factor umwandeln (0/1)
train_data_clean$`Spende Maerz 2007` <- as.factor(train_data_clean$`Spende Maerz 2007`)
train_data_clean$`Spende Maerz 2007` <- factor(train_data_clean$`Spende Maerz 2007`, levels = c("0", "1"))

# 5) Vorhersagen des Modells
train_data_clean$Predictions <- predict(glm_model, newdata = train_data_clean, type = "response")
train_data_clean$Predictions <- as.factor(ifelse(train_data_clean$Predictions >= 0.5, "1", "0"))

# 6) Konfusionsmatrix
conf_matrix <- confusionMatrix(
  train_data_clean$Predictions,
  train_data_clean$`Spende Maerz 2007`
)

# 7) Metriken (Recall = Sensitivity)
accuracy    <- conf_matrix$overall["Accuracy"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
precision   <- conf_matrix$byClass["Pos Pred Value"]  # "Precision" im caret-Output
recall      <- conf_matrix$byClass["Sensitivity"]     # "Recall" als "Sensitivity"

print(conf_matrix)

# 8) Metriken ausgeben
print(paste("Genauigkeit:", round(accuracy, 2)))
print(paste("Sensitivität (Recall):", round(sensitivity, 2)))
print(paste("Präzision:", round(precision, 2)))
print(paste("Recall:", round(recall, 2)))



## 5.3.1 Visualisierung der Metriken ----
# 1) Metriken als Balkendiagramm
metrics_df <- data.frame(
  Metric = c("Genauigkeit", "Sensitivität (Recall)", "Präzision", "Recall"),
  Wert   = c(accuracy, sensitivity, precision, recall)
)




##############################################################################
# 5.4) ROC-KURVE & AUC
# ----------------------------------------------------------------------------
# pROC::roc() erwartet als 'response' eine Faktor-Variable mit Levels (z.B. "0","1")
# und als 'predictor' einen NUMERISCHEN Vektor, der die vorhergesagte Wahrscheinlichkeit 
# oder den Score repräsentiert. 
#
# Daher verwenden wir hier NICHT das factor-Objekt "Predictions" (0/1),
# sondern die echte numerische Vorhersagewahrscheinlichkeit (type="response").
##############################################################################

# 1) Falls train_data_clean bzw. glm_model nicht existieren, laden/anlegen:
if (!exists("train_data_clean") || !exists("glm_model")) {
  stop("Bitte stellen Sie sicher, dass 'train_data_clean' & 'glm_model' vorhanden sind!")
}

# 2) Numerische Wahrscheinlichkeiten berechnen (predict(..., type='response'))
#    => "Prob" = Schätzwerte aus dem Modell
train_data_clean$Prob <- predict(glm_model, newdata = train_data_clean, type = "response")

# 3) ROC-Kurve berechnen
#    - 'response' = Zielvariable, am besten Faktor mit Levels c("0","1")
#    - 'predictor' = numeric, hier unser 'Prob'
#    - levels=c("0","1") => 0 als negatives, 1 als positives Klasse
roc_curve <- pROC::roc(
  response  = train_data_clean$`Spende Maerz 2007`, 
  predictor = train_data_clean$Prob,
  levels    = c("0","1")
)



# 5) AUC ausgeben
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")


##############################################################################
# 5.3 + 5.4) Metriken-Plot (links) & ROC-Kurve (rechts)
# ----------------------------------------------------------------------------
# Wir zeigen "Übersicht der Metriken" (Barplot) auf der linken Seite,
# und direkt daneben die ROC-Kurve inklusive AUC auf der rechten Seite.
#
# Voraussetzung:
#   - Wir haben bereits `train_data_clean` (mit Spalte 'Prob' als Wahrscheinlichkeiten)
#     und ein trainiertes Modell 'glm_model'.
#   - "Spende Maerz 2007" = Faktor mit Levels c("0","1").
##############################################################################

# 1) Prüfen, ob wir train_data_clean und glm_model haben
if (!exists("train_data_clean") || !"Prob" %in% colnames(train_data_clean) || !exists("glm_model")) {
  stop("Bitte stellen Sie sicher, dass 'train_data_clean$Prob' und 'glm_model' existieren!")
}

# 2) PAR-Layout: 1 Zeile, 2 Spalten
par(mfrow = c(1, 2),      # links: Barplot, rechts: ROC
    oma  = c(0, 0, 2, 0), # äußerer Rand
    mar  = c(4, 4, 4, 1)) # innerer Rand pro Plot

# 3) LINKS: Barplot der Metriken
bar_x <- seq_along(metrics_df$Metric)
bar_heights <- metrics_df$Wert
bar_names <- metrics_df$Metric

barplot(
  height     = bar_heights,
  names.arg  = bar_names,
  col        = "steelblue",
  ylim       = c(0, 1),
  main       = "Übersicht der Metriken",
  xlab       = "Kennzahl",
  ylab       = "Wert"
)

# Werte draufschreiben
text(
  x      = bar_x,
  y      = bar_heights,
  labels = round(bar_heights, 2),
  pos    = 3,     # oberhalb
  cex    = 1.2
)

# 4) RECHTS: ROC-Kurve
plot(
  roc_curve,
  col  = "blue",
  main = paste("ROC-Kurve - AUC:", round(auc_value, 3))
)
