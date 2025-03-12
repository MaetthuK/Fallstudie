################################################################################
# 1. SETUP
################################################################################
# 1.1 Pakete und Libraries laden
# (Hier ggf. weitere benötigte Pakete einfügen, z.B. für Datenanalyse, Plots etc.)
library(dplyr)       # für Datenmanipulation
library(ggplot2)     # für Visualisierungen
library(caret)       # für Modelltraining und Confusion-Matrix (optional)

# 1.2 Arbeitsverzeichnis und Datendateien
# Hinweis: In RStudio ggf. Session > Set Working Directory ... anpassen
# Da die CSV-Dateien laut Aufgabe "bloodtrain.csv" und "bloodtest.csv" im Projekt 
# oder in der Shiny-App liegen, wird hier ein exemplarischer Pfad angegeben.
# Bei Bedarf ändern oder an das eigene Projekt anpassen.
# setwd("Pfad/zum/Projektverzeichnis")

################################################################################
# 2. DATENVERSTÄNDNIS
################################################################################

# 2.1 Einlesen der Trainings- und Testdaten
# Annahme: Die Dateien haben jeweils einen Header (Spaltennamen) und sind durch Kommas 
# oder Semikolons getrennt. Entsprechend 'sep' ggf. anpassen.
blood_train <- read.csv("bloodtrain.csv", 
                        sep = ";",          # ggf. "," wenn Kommaseparierung
                        header = TRUE)

blood_test <- read.csv("bloodtest.csv", 
                       sep = ";", 
                       header = TRUE)

# Kurzer Blick auf die Daten und plot machen
head(blood_train)  # Erste Zeilen des Trainingsdatensatzes
head(blood_test)   # Erste Zeilen des Testdatensatzes

# 2.2 Überblick über die Zielvariable
# Laut Aufgabenstellung heißt die Zielvariable im Trainingsdatensatz "target".
# Die Testdaten haben diese Spalte nicht.
table(blood_train$target)  # Verteilung der Zielvariable im Trainingsdatensatz

################################################################################
# 3. EXPLORATIVE DATENANALYSE (EDA)
################################################################################

# 3.1 Übersicht: Fehlende Werte und Ausreißer checken
# (Je nach Datensatz anpassen)
sum(is.na(blood_train))  # Anzahl fehlender Werte im Training
sum(is.na(blood_test))   # Anzahl fehlender Werte im Test

# Beispiel: Boxplots für numerische Variablen
# (Annehmen wir haben Spalten "MonthsLastDonation", "NumDonations", etc.)
# Bitte die tatsächlichen Spaltennamen aus dem Datensatz verwenden
numeric_vars <- c("MonthsLastDonation", "NumDonations")  # Beispiel-Variablen
boxplot(blood_train[, numeric_vars], 
        main = "Boxplot der Trainingsdaten für ausgewählte numerische Variablen",
        col = "lightblue")

# 3.2 Korrelationen (falls gewünscht)
# Beispielhafte Korrelation (Pearson) zwischen numerischen Variablen im Training
# Achtung: Funktioniert nur, wenn numeric_vars existieren.
cor_matrix <- cor(blood_train[, numeric_vars])
print(cor_matrix)

# 3.3 Verteilungen / Histogramme
# Hier beispielhaft ein Histogramm einer numerischen Variable
hist(blood_train$MonthsLastDonation, 
     main = "Histogramm: Monate seit letzter Spende (Train)",
     xlab = "Monate seit letzter Spende",
     col = "lightgreen")

################################################################################
# 4. DATENBEREINIGUNG
################################################################################
# In diesem Abschnitt würden wir z.B. 
#   - Duplikate entfernen
#   - Datenformate anpassen (z.B. Faktoren, Datumsformate)
#   - Outlier behandeln
#   - etc.
# Beispielhaft:
blood_train_clean <- blood_train %>%
  distinct()   # Duplikate entfernen, falls vorhanden

# Falls weitere Bereinigungsschritte nötig sind, hier ergänzen.

################################################################################
# 5. FEATURE ENGINEERING
################################################################################
# Hier könnten neue Variablen abgeleitet werden. Z.B. "HeuerPraediktor" o.Ä.
# Beispiel: Erstelle eine fiktive neue Variable
# (Anhand der PDF-Hinweise könnte man z.B. "AnzahlSpenden / MonateSeitLetzterSpende" definieren)
# Dies ist nur ein Beispiel – bitte an die tatsächliche Logik anpassen.
blood_train_clean <- blood_train_clean %>%
  mutate(HeuerPraediktor = NumDonations / (MonthsLastDonation + 1))  # +1 um Division durch 0 zu vermeiden

# Gleiches Feature ggf. auch in Testdaten erzeugen (falls vorhanden):
if("MonthsLastDonation" %in% colnames(blood_test) & "NumDonations" %in% colnames(blood_test)){
  blood_test <- blood_test %>%
    mutate(HeuerPraediktor = NumDonations / (MonthsLastDonation + 1))
}

################################################################################
# 6. MODELL (LOGISTISCHE REGRESSION)
################################################################################

# 6.1 Training des Modells
# Wir nehmen an, die Zielvariable heißt "target" (0 = kein Spender, 1 = Spender).
# Hier ein vereinfachtes Beispiel für eine logistische Regression mit family = binomial.
# Die Formel kann nach Bedarf erweitert werden, z.B. target ~ MonthsLastDonation + NumDonations + HeuerPraediktor
# Wichtig: Stellen Sie sicher, dass 'target' als Faktor oder 0/1 vorliegt.
blood_train_clean$target <- as.factor(blood_train_clean$target)

model_glm <- glm(target ~ MonthsLastDonation + NumDonations + HeuerPraediktor,
                 data = blood_train_clean,
                 family = binomial(link = "logit"))

# 6.2 Modellzusammenfassung
summary(model_glm)

# 6.3 Modellgüte im Trainingsdatensatz (Confusion Matrix)
# Zunächst Vorhersagen (p-Werte)
pred_prob_train <- predict(model_glm, type = "response") 
# Klassische binäre Entscheidung (hier Schwellenwert = 0.5)
pred_class_train <- ifelse(pred_prob_train >= 0.5, 1, 0)  

# Confusion Matrix erstellen
conf_mat_train <- table(Vorhergesagt = pred_class_train, 
                        Wahr = blood_train_clean$target)
conf_mat_train

# Optional: Mit caret::confusionMatrix für erweiterte Kennzahlen
# confusionMatrix(
#   data = factor(pred_class_train, levels = c(0,1)),
#   reference = factor(blood_train_clean$target, levels = c(0,1))
# )

################################################################################
# 7. EINSATZ AUF TESTDATEN & VORHERSAGE
################################################################################

# 7.1 Vorhersage auf Testdatensatz (der keine 'target'-Spalte hat)
# Wir verwenden das zuvor trainierte Modell.
# Zunächst berechnen wir die Wahrscheinlichkeit:
pred_prob_test <- predict(model_glm, 
                          newdata = blood_test, 
                          type = "response")

# 7.2 Finale binäre Klassifikation (Optional)
pred_class_test <- ifelse(pred_prob_test >= 0.5, 1, 0)

# 7.3 Ergebnisausgabe
# Oft wird gewünscht, die Vorhersagen in einer CSV-Datei zu speichern.
# Hier als Beispiel: wir schreiben die ID (falls vorhanden) und das Prediction-Flag.
# Falls "ID" im Testdatensatz existiert, adaptieren Sie dies bitte entsprechend.
if("ID" %in% colnames(blood_test)){
  submission <- data.frame(ID = blood_test$ID, 
                           PredictedTarget = pred_class_test)
} else {
  submission <- data.frame(Zeile = 1:nrow(blood_test),
                           PredictedTarget = pred_class_test)
}

# Schreiben als CSV für den Upload / die Abgabe
# (Pfad und Dateiname bitte anpassen)
write.csv(submission, 
          file = "BloodPredictions.csv", 
          row.names = FALSE)

################################################################################
# 8. FAZIT UND WEITERE SCHRITTE
################################################################################

# - Das obige Skript illustriert einen durchgängigen Workflow:
#   1) Daten einlesen und untersuchen
#   2) Explorative Datenanalyse und Bereinigung
#   3) Feature-Engineering (z.B. neue Prädiktorvariable)
#   4) Training eines logistischen Regressionsmodells
#   5) Evaluation mit Confusion Matrix
#   6) Anwendung auf den Testdatensatz (generiert Prognosen)
#
# - Zur weiteren Verbesserung könnte man:
#   * Weitere Variablen/Prädiktoren hinzufügen oder selektieren
#   * Den Schwellenwert (0.5) zur Klassifikation optimieren
#   * Kreuzvalidierung oder Bootstrapping zur robusteren Schätzung einsetzen
#   * Weitere Metriken auswerten (AUC, Sensitivität, Spezifität etc.)
#
# ENDE DES SKRIPTS
