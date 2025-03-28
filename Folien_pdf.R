###############################################################################
# GESAMTES SKRIPT: Boxplots, Histogramme, Pairs/Correlation
# Ziel: Keine Fehlermeldung "figure margins too large".
# Ausgabe in mehreren PDF-Dateien (A4-Querformat), pro "Folie" eine PDF.
###############################################################################

# 1) Pakete laden -------------------------------------------------------------
# (Installieren bei Bedarf z.B. via install.packages("corrplot") usw.)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(pROC)
library(gridExtra)  # für grid.arrange, tableGrob etc.
library(grid)       # für grid.rect, textGrob

# 2) CSV-Dateien einlesen (Train/Test) ----------------------------------------
train_data <- read.csv("bloodtrain.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
test_data  <- read.csv("bloodtest.csv",  header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Spaltennamen anpassen
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

# Kennzeichnen, aus welchem Datensatz
train_data$Dataset <- "Train"
test_data$Dataset  <- "Test"

# Kombinierte Daten (für Boxplots etc.)
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

# Kurzer Blick auf Datenstruktur -------------------------------------------
# (kein PDF, nur Konsole)
cat("Train-Daten:\n")
str(train_data)
cat("\nTest-Daten:\n")
str(test_data)


###############################################################################
# 3) FOLIE 3: STATISTISCHE KENNZAHLEN (PDF, A4 QUER)
###############################################################################
###############################################################################
# FOLIE 3 – Final optimiert: Ausgewogene Höhen & saubere Positionen
###############################################################################

library(grid)
library(here)

# === Daten vorbereiten ===
summary_train <- summary(train_data |> select(-Dataset))
summary_test  <- summary(test_data  |> select(-Dataset))

train_text <- paste(capture.output(summary_train), collapse = "\n")
test_text  <- paste(capture.output(summary_test), collapse = "\n")

train_rows <- nrow(train_data)
test_rows  <- nrow(test_data)

spende_counts <- table(train_data$`Spende Maerz 2007`)
spende_text <- paste0("Spende März 2007 ja/nein:\nja:   ", spende_counts["1"],
                      "\nnein: ", spende_counts["0"])

# === Dynamische Höhen berechnen ===
line_height <- 0.015
train_lines <- length(capture.output(summary_train))
test_lines  <- length(capture.output(summary_test))

train_box_height <- 0.07 + train_lines * line_height
test_box_height  <- 0.07 + test_lines * line_height

# === Y-Positionen mit Optimierung ===
y_train_title <- 0.84
y_train_box   <- y_train_title - 0.035

y_output_box  <- y_train_box - train_box_height - 0.025  
y_test_title  <- y_output_box - 0.18                    
y_test_box    <- y_test_title - 0.04

# === PDF erzeugen ===
presentation_dir <- here::here("Präsentation")
pdf_filename <- file.path(presentation_dir, "Folie3_Statistische_Kennzahlen.pdf")
pdf(pdf_filename, width = 11.7, height = 8.3)
grid.newpage()

# Hintergrund
grid.rect(gp = gpar(fill = "white", col = NA))

# Kopfbereich (blau)
grid.rect(
  x = 0.5, y = 1,
  width = 1, height = 0.12,
  just = c("center", "top"),
  gp = gpar(fill = "#005DAA", col = NA)
)

grid.text(
  "Statistische Kennzahlen für Trainings– und Testdaten",
  x = 0.5, y = 0.965,
  gp = gpar(fontsize = 24, fontface = "bold", col = "white")
)

# === Trainingsdaten: Titel ===
grid.text(
  "2.1.1 Trainingsdaten Zusammenfassung",
  x = 0.06, y = y_train_title,
  just = c("left", "top"),
  gp = gpar(fontsize = 14, fontface = "bold")
)

# === Trainingsdaten: Box ===
pushViewport(viewport(
  x = 0.5, y = y_train_box,
  width = 0.9, height = train_box_height,
  just = c("center", "top")
))
grid.rect(gp = gpar(fill = "#B3E5FC", col = "black"))
grid.text(
  train_text,
  x = 0.02, y = 0.93, just = c("left", "top"),
  gp = gpar(fontfamily = "Courier", fontsize = 9, lineheight = 1.1)
)
grid.text(
  paste0("Anzahl Zeilen: ", train_rows),
  x = 0.98, y = 0.93, just = c("right", "top"),
  gp = gpar(fontsize = 10, fontface = "italic")
)
popViewport()

# === Output Variable Box (zentriert) ===
pushViewport(viewport(
  x = 0.5, y = y_output_box,         # statt x = 0.11
  width = 0.33, height = 0.10,
  just = c("center", "top")          # statt "left"
))
grid.rect(gp = gpar(fill = "#0277BD", col = "black", lwd = 1.5))
grid.text("Output Variable",
          x = 0.05, y = 0.90, just = c("left", "top"),
          gp = gpar(fontsize = 11, fontface = "bold", col = "white"))
grid.text(spende_text,
          x = 0.05, y = 0.68, just = c("left", "top"),
          gp = gpar(fontsize = 10, col = "white", fontfamily = "mono"))
popViewport()


# === Testdaten: Titel ===
grid.text(
  "2.1.3 Testdaten Zusammenfassung",
  x = 0.06, y = y_test_title,
  just = c("left", "top"),
  gp = gpar(fontsize = 14, fontface = "bold")
)

# === Testdaten: Box ===
pushViewport(viewport(
  x = 0.5, y = y_test_box,
  width = 0.9, height = test_box_height,
  just = c("center", "top")
))
grid.rect(gp = gpar(fill = "#C8E6C9", col = "black"))
grid.text(
  test_text,
  x = 0.02, y = 0.93, just = c("left", "top"),
  gp = gpar(fontfamily = "Courier", fontsize = 9, lineheight = 1.1)
)
grid.text(
  paste0("Anzahl Zeilen: ", test_rows),
  x = 0.98, y = 0.93, just = c("right", "top"),
  gp = gpar(fontsize = 10, fontface = "italic")
)
popViewport()

# === Fertig ===
dev.off()





###############################################################################
# 4) "FOLIE 4": Boxplot-Vergleich Trainings- und Testdaten --------------------
###############################################################################

library(ggplot2)
library(grid)
library(gridExtra)
library(here)

# Vorausgesetzt, combined_data ist bereits im Workspace

# p1: Boxplot Monate Letzte Spende
p1 <- ggplot(combined_data, aes(x = Dataset, y = `Monate Letzte Spende`, fill = Dataset)) +
  geom_boxplot(color = "black", notch = TRUE) +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Monate Letzte Spende", x = "Datensatz", y = "Monate") +
  theme_minimal()

# p2: Boxplot Anzahl Spenden
p2 <- ggplot(combined_data, aes(x = Dataset, y = `Anzahl Spenden`, fill = Dataset)) +
  geom_boxplot(color = "black", notch = TRUE) +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Anzahl Spenden", x = "Datensatz", y = "Anzahl") +
  theme_minimal()

# p3: Boxplot Gesamtvolumen
p3 <- ggplot(combined_data, aes(x = Dataset, y = Gesamtvolumen, fill = Dataset)) +
  geom_boxplot(color = "black", notch = TRUE) +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Gesamtvolumen", x = "Datensatz", y = "Volumen") +
  theme_minimal()

# p4: Boxplot Monate Erste Spende
p4 <- ggplot(combined_data, aes(x = Dataset, y = `Monate Erste Spende`, fill = Dataset)) +
  geom_boxplot(color = "black", notch = TRUE) +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Monate Erste Spende", x = "Datensatz", y = "Monate") +
  theme_minimal()

# Titelbalken
title_bg   <- rectGrob(gp = gpar(fill = "#7F3FBF", col = NA))
title_txt  <- textGrob(
  "Boxplots Train vs. Test",
  gp = gpar(fontsize = 28, col = "white", fontface = "bold")
)
title_panel <- grobTree(title_bg, title_txt)

# Kommentarbox
comment_bg   <- rectGrob(gp = gpar(fill = rgb(1,1,1,0.2), col = NA))
comment_text <- textGrob(
  "Erkenntnisse:\n\n• Verteilungen der Merkmale im Vergleich\n• Kein starker Train/Test-Shift\n\nSchlussfolgerung:\n• Datenbasis bleibt konsistent für Modellierung",
  x = 0.05, y = 0.95, just = c("left", "top"),
  gp = gpar(col = "black", fontsize = 12)
)
comment_panel <- grobTree(comment_bg, comment_text)

# PDF erzeugen (A4 Querformat: 11.7 x 8.3 Zoll)
presentation_dir <- here::here("Präsentation")
pdf_filename <- file.path(presentation_dir, "Folie4_Boxplot_train_test.pdf")
pdf(pdf_filename, width = 11.7, height = 8.3)

# Anordnung via grid.arrange (ohne zusätzliche grün gefüllte Seite)
grid.arrange(
  title_panel, p1, p2, p3, p4, comment_panel,
  layout_matrix = rbind(
    c(1, 1, 1, 1),   # Titelzeile: über 4 Spalten
    c(2, 3, 4, 5),   # Boxplots in einer Reihe
    c(6, 6, 6, 6)    # Kommentarbox unten
  ),
  heights = c(0.8, 3, 2)
)

dev.off()

###############################################################################
# 5) "FOLIE 5": Histogramme (Train vs. Test) + Klassenverteilung --------------
###############################################################################
pdf("Präsentation/Folie5_Histogramme.pdf", width=11.7, height=8.3)
# Wir erstellen 3 Zeilen × 4 Spalten via grid.arrange -> bequemer ist es,
# ggplot2 zu verwenden und am Ende alles zusammenzufügen.

# p1: Monate Letzte Spende (binwidth=1)
p1 <- ggplot(combined_data, aes(x=`Monate Letzte Spende`, fill=Dataset)) +
  geom_histogram(binwidth=1, position="dodge", color="black") +
  scale_fill_manual(values=c("Train"="blue","Test"="green")) +
  labs(title="Monate Letzte Spende", x="Monate", y="Frequenz") +
  theme_minimal()

# p2: Anzahl Spenden (binwidth=1)
p2 <- ggplot(combined_data, aes(x=`Anzahl Spenden`, fill=Dataset)) +
  geom_histogram(binwidth=1, position="dodge", color="black") +
  scale_fill_manual(values=c("Train"="blue","Test"="green")) +
  labs(title="Anzahl Spenden", x="Anzahl", y="Frequenz") +
  theme_minimal()

# p3: Gesamtvolumen (binwidth=500)
p3 <- ggplot(combined_data, aes(x=Gesamtvolumen, fill=Dataset)) +
  geom_histogram(binwidth=500, position="dodge", color="black") +
  scale_fill_manual(values=c("Train"="blue","Test"="green")) +
  labs(title="Gesamtvolumen", x="Volumen", y="Frequenz") +
  theme_minimal()

# p4: Monate Erste Spende (binwidth=5)
p4 <- ggplot(combined_data, aes(x=`Monate Erste Spende`, fill=Dataset)) +
  geom_histogram(binwidth=5, position="dodge", color="black") +
  scale_fill_manual(values=c("Train"="blue","Test"="green")) +
  labs(title="Monate Erste Spende", x="Monate", y="Frequenz") +
  theme_minimal()

# p5: Balkendiagramm Klassenverteilung Spende Maerz 2007 (Train)
p5 <- ggplot(train_data, aes(x=factor(`Spende Maerz 2007`), fill="Train")) +
  geom_bar(color="black") +
  scale_fill_manual(values=c("Train"="blue")) +
  scale_x_discrete(labels=c("0"="nein","1"="ja")) +
  labs(title="Klassenverteilung (Train)", x="Spende im März 2007?", y="Anzahl") +
  theme_minimal() +
  theme(legend.position="none")

# Titelbalken grob
title_bg   <- rectGrob(gp=gpar(fill="#7F3FBF", col=NA))
title_txt  <- textGrob(
  "Histogramme Train vs. Test",
  gp = gpar(fontsize=28, col="white", fontface="bold")
)
title_panel<- grobTree(title_bg, title_txt)

# Kommentarbox
comment_bg   <- rectGrob(gp=gpar(fill=rgb(1,1,1,0.2), col=NA))
comment_text <- textGrob(
  "Erkenntnisse:\n\n• Verteilungen ähnlich, \n  aber Test hat weniger hohe Spenderzahlen\n• Klassenverteilung: ~60% ja, 40% nein\n\nSchlussfolgerung:\n• Kein starker Train/Test-Shift\n• Modellauswahl kann auf stabilen Daten basieren",
  x=0.05, y=0.95, just=c("left","top"),
  gp=gpar(col="black", fontsize=12)
)
comment_panel<- grobTree(comment_bg, comment_text)

# Alles anordnen
grid.arrange(
  # 7 "Elemente": title_panel, p1, p2, p3, p4, p5, comment_panel
  title_panel, p1, p2, p3, p4, p5, comment_panel,
  layout_matrix = rbind(
    c(1,1,1,1),
    c(2,3,4,5),
    c(6,6,7,7)
  ),
  heights = c(0.8,3,2)
)

dev.off()


###############################################################################
# FOLIE 6 – Pairs-Plot & Korrelationsmatrix (nur eine Seite, A4-Querformat)
###############################################################################

library(tidyverse)
library(corrplot)
library(GGally)
library(gridExtra)
library(grid)
library(ggplotify)

# --- Daten einlesen ---
train_data <- read.csv("bloodtrain.csv", stringsAsFactors = FALSE)
colnames(train_data) <- c(
  "ID",
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende",
  "Spende Maerz 2007"
)

# --- Zusätzliche Variable ---
train_data$Spendetakt <- with(
  train_data,
  ifelse(
    `Monate Letzte Spende` == 0,
    NA,
    `Anzahl Spenden` / `Monate Letzte Spende`
  )
)

# --- Korrelation ---
num_data   <- train_data |>
  select(where(is.numeric), -ID)
cor_matrix <- cor(num_data, use = "pairwise.complete.obs")

# --- Pairs-Plot via ggpairs ---
ggp <- ggpairs(
  train_data,
  columns = c("Monate Letzte Spende", "Anzahl Spenden",
              "Gesamtvolumen", "Monate Erste Spende", "Spendetakt"),
  mapping = aes(color = factor(`Spende Maerz 2007`)),
  title   = "Pairs-Plot Trainingsdaten"
) +
  theme_minimal() +
  theme(plot.title = element_text(size=14, face="bold"))

ggp_grob <- as.grob(function() print(ggp))

# --- Korrelationsmatrix als Grob ---
corr_grob <- as.grob(function() {
  corrplot(
    cor_matrix,
    method      = "color",
    type        = "upper",
    tl.col      = "black",
    tl.cex      = 0.9,
    tl.srt      = 45,
    addCoef.col = "black",
    number.cex  = 0.75,
    title       = "Korrelationsmatrix",
    cex.main    = 1.2,   # ~14 pt
    mar         = c(0, 0, 2, 0),
    font.main   = 2
  )
})

# --- Kommentare ---
comment_1 <- textGrob(
  "Erkenntnisse:\n• Hohe Korrelation: Anzahl Spenden & Gesamtvolumen\n• Spendetakt korreliert mit anderen",
  gp = gpar(col = "black", fontsize = 12),
  x = 0.05, y = 0.95, just = c("left", "top")
)
comment_2 <- textGrob(
  "Schlussfolgerung:\n• 'Gesamtvolumen' redundant\n• 'Spendetakt' als neuer Prädiktor",
  gp = gpar(col = "black", fontsize = 12),
  x = 0.05, y = 0.95, just = c("left", "top")
)

# --- Titel-Balken oben ---
title_bg <- rectGrob(
  x=0.5, y=0.5, width=1, height=1,
  gp = gpar(fill="#7F3FBF", col=NA)
)
title_txt <- textGrob(
  "Beziehung der Variablen",
  gp = gpar(fontsize=24, col="white", fontface="bold"),
  x=0.5, y=0.5
)
title_panel <- grobTree(title_bg, title_txt)

# === PDF-Erzeugung (nur eine Seite) ===
pdf("Präsentation/Folie6_Beziehung_Variablen.pdf", width=11.7, height=8.3)



grid.arrange(
  # 5 Grobs in 3 Zeilen × 2 Spalten
  title_panel,  # (Zeile 1, spannt beide Spalten)
  ggp_grob,     # (Zeile 2, Spalte 1)
  corr_grob,    # (Zeile 2, Spalte 2)
  comment_1,    # (Zeile 3, Spalte 1)
  comment_2,    # (Zeile 3, Spalte 2)
  layout_matrix = rbind(
    c(1, 1),
    c(2, 3),
    c(4, 5)
  ),
  heights = c(0.15, 0.65, 0.2)
)

dev.off()



###############################################################################
# FOLIE 7 – Modellerstellung & Variable Importance (eine Seite, A4 Querformat),
# Layout wie Folien 3–6
###############################################################################

# Pakete ----------------------------------------------------------------------
library(tidyverse)
library(caret)      # varImp()
library(grid)
library(gridExtra)
library(ggplot2)

# --- 1) Daten einlesen & Spalten anpassen ---
train_data <- read.csv("bloodtrain.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(train_data) <- c(
  "ID",
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende",
  "Spende Maerz 2007"
)

# --- 2) Neuer Prädiktor "Spenden Rate" ---
# Bei 0 in "Monate Letzte Spende" => NA
train_data$`Monate Letzte Spende`[train_data$`Monate Letzte Spende` == 0] <- NA
train_data$`Spenden Rate` <- with(
  train_data,
  ifelse(is.na(`Monate Letzte Spende`), NA, `Anzahl Spenden` / `Monate Letzte Spende`)
)

# Zielvariable in Faktor
train_data$`Spende Maerz 2007` <- factor(train_data$`Spende Maerz 2007`, levels = c("0","1"))

# NA-Zeilen entfernen
train_data_clean <- subset(train_data, !is.na(`Spende Maerz 2007`) & !is.na(`Spenden Rate`))

# --- 3) GLM-Modell erstellen ---
glm_model <- glm(
  `Spende Maerz 2007` ~ `Anzahl Spenden` + `Monate Letzte Spende` +
    `Spenden Rate` + `Monate Erste Spende`,
  data   = train_data_clean,
  family = binomial(link = "logit")
)

# Modell-Zusammenfassung abfangen
model_summary_txt <- capture.output(summary(glm_model))

# --- 4) Modell-Zusammenfassung links ---
# Hintergrund & Text in einem "Panel" (wie p1, p2 etc. bei den anderen Folien)
summary_bg   <- rectGrob(gp = gpar(fill = "grey", col = NA))  # Dunkelblau
summary_txt  <- textGrob(
  paste(model_summary_txt, collapse = "\n"),
  x = 0.03, y = 0.97,
  just = c("left", "top"),
  gp   = gpar(col = "black", fontsize = 10, fontfamily = "Courier")
)
summary_panel <- grobTree(summary_bg, summary_txt)

# --- 5) Variable Importance Plot rechts ---
var_imp    <- varImp(glm_model, scale = FALSE)
var_imp_df <- data.frame(
  Variable   = rownames(var_imp),
  Importance = var_imp$Overall
)

p_varImp <- ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#3399FF", color = "black") +
  coord_flip() +
  labs(
    title = "Variable Importance (Logistisches Modell)",
    x     = "Prädiktor",
    y     = "Wichtigkeit"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 11)
  )

# In ein grob-Objekt überführen (analog zu p1, p2 etc.)
plot_panel <- ggplotGrob(p_varImp)

# --- 6) Titelbalken (violett) wie die anderen Folien ---
title_bg  <- rectGrob(gp = gpar(fill = "#7F3FBF", col = NA))
title_txt <- textGrob(
  "Modellerstellung & Variable Importance",
  gp = gpar(fontsize = 28, fontface = "bold", col = "white"),
  x  = 0.5, y = 0.5
)
title_panel <- grobTree(title_bg, title_txt)

# --- 7) Kommentarbox unten ---
comment_bg   <- rectGrob(gp = gpar(fill = rgb(1,1,1,0.2), col = NA))
comment_text <- textGrob(
  "Erkenntnisse:\n\n• Modell fasst Spenden besser zusammen\n• Höchste Wichtigkeit: 'Monate Letzte Spende'\n\nSchlussfolgerung:\n• 'Spenden Rate' gibt guten Zusatznutzen",
  x = 0.05, y = 0.95, just = c("left", "top"),
  gp = gpar(col = "black", fontsize = 12)
)
comment_panel <- grobTree(comment_bg, comment_text)

# --- 8) PDF-Erzeugung (A4 Querformat) ---
pdf("Präsentation/Folie7_Modell_VariableImportance.pdf", width = 11.7, height = 8.3)

# Layout ähnlich wie bei Folie4/Folie5:
# - Oben: Titel (über 4 Spalten)
# - Mitte: 2 Spalten (Summary, Plot)
# - Unten: Kommentar (über 4 Spalten)
grid.arrange(
  title_panel, summary_panel, plot_panel, comment_panel,
  layout_matrix = rbind(
    c(1, 1, 1, 1),  # Titel
    c(2, 2, 3, 3),  # links: summary, rechts: VarImp-Plot
    c(4, 4, 4, 4)   # Kommentar unten
  ),
  heights = c(0.8, 3, 2)
)

dev.off()





###############################################################################
# FOLIE 8 – Metriken und Konfusionsmatrix (eine Seite, A4 Querformat),
# Layout wie Folie 4–7
###############################################################################

# Pakete ----------------------------------------------------------------------
library(tidyverse)
library(caret)      # confusionMatrix
library(grid)
library(gridExtra)
library(ggplot2)

# 1) Modell & Daten vorausgesetzt ---------------------------------------------
#    Wir gehen davon aus, dass du schon Folie 7 erstellt hast und
#    ein Objekt "glm_model" sowie "train_data_clean" existieren.  
#    Falls nicht, fügen wir kurz den Code zum erneuten Erzeugen ein:

if (!exists("glm_model") || !exists("train_data_clean")) {
  
  # CSV laden
  train_data <- read.csv("bloodtrain.csv", header=TRUE, stringsAsFactors=FALSE)
  colnames(train_data) <- c(
    "ID",
    "Monate Letzte Spende",
    "Anzahl Spenden",
    "Gesamtvolumen",
    "Monate Erste Spende",
    "Spende Maerz 2007"
  )
  
  # Neuen Praediktor "Spenden Rate"
  train_data$`Monate Letzte Spende`[train_data$`Monate Letzte Spende` == 0] <- NA
  train_data$`Spenden Rate` <- with(
    train_data,
    ifelse(is.na(`Monate Letzte Spende`), NA, `Anzahl Spenden` / `Monate Letzte Spende`)
  )
  
  train_data$`Spende Maerz 2007` <- factor(train_data$`Spende Maerz 2007`, levels = c("0","1"))
  train_data_clean <- subset(train_data, !is.na(`Spende Maerz 2007`) & !is.na(`Spenden Rate`))
  
  # GLM-Modell
  glm_model <- glm(
    `Spende Maerz 2007` ~ `Anzahl Spenden` + `Monate Letzte Spende` +
      `Spenden Rate` + `Monate Erste Spende`,
    data   = train_data_clean,
    family = binomial(link = "logit")
  )
}

# 2) Vorhersagen + Konfusionsmatrix ------------------------------------------
#    Wir erzeugen Predicted-Klassen (0/1) via Schwelle 0.5
train_data_clean$Predictions <- predict(glm_model, newdata=train_data_clean, type="response")
train_data_clean$Predictions <- ifelse(train_data_clean$Predictions >= 0.5, "1", "0")
train_data_clean$Predictions <- factor(train_data_clean$Predictions, levels=c("0","1"))

# confusionMatrix (aus caret)
conf_mat <- confusionMatrix(
  data      = train_data_clean$Predictions,
  reference = train_data_clean$`Spende Maerz 2007`
)

# 3) Text-Panel mit Konfusionsmatrix -----------------------------------------
conf_text_lines <- capture.output(conf_mat)
conf_bg  <- rectGrob(gp = gpar(fill = "#002B55", col = NA))  # Dunkelblau
conf_txt <- textGrob(
  paste(conf_text_lines, collapse = "\n"),
  x    = 0.03, y = 0.97,
  just = c("left", "top"),
  gp   = gpar(col = "white", fontsize = 10, fontfamily = "Courier")
)
conf_panel <- grobTree(conf_bg, conf_txt)

# 4) Kennzahlen (Accuracy, Sensitivity, Precision, Recall) --------------------
acc         <- conf_mat$overall["Accuracy"]
sens        <- conf_mat$byClass["Sensitivity"]
prec        <- conf_mat$byClass["Pos Pred Value"]  # caret nennt das "Pos Pred Value"
rec         <- conf_mat$byClass["Sensitivity"]     # Gleicher Wert wie sens
metrics_df  <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Precision", "Recall"),
  Value  = c(acc, sens, prec, rec)
)

p_metrics <- ggplot(metrics_df, aes(x=reorder(Metric, Value), y=Value)) +
  geom_bar(stat="identity", fill="#3399FF", color="black") +
  coord_flip() +
  ylim(0, 1) +  # Da alle Werte zw. 0 und 1
  labs(
    title="Kennzahlen (Train)",
    x="Metrik",
    y="Wert"
  ) +
  theme_minimal(base_size=14) +
  theme(
    plot.title = element_text(face="bold", size=16),
    axis.title = element_text(size=14),
    axis.text  = element_text(size=11)
  )

metrics_panel <- ggplotGrob(p_metrics)

# 5) Kommentarbox unten ------------------------------------------------------
comment_bg   <- rectGrob(gp = gpar(fill = rgb(1,1,1,0.2), col=NA))
comment_text <- textGrob(
  "Erkenntnisse:\n\n• Modell erreicht solide Werte\n• Sensitivität = Recall\n\nSchlussfolgerung:\n• Balancierung oder Feintuning möglich\n• Nächstes Ziel: Testdaten evaluieren",
  x=0.05, y=0.95, just=c("left","top"),
  gp=gpar(col="black", fontsize=12)
)
comment_panel <- grobTree(comment_bg, comment_text)

# 6) Titelbalken oben (violett) ----------------------------------------------
title_bg  <- rectGrob(gp = gpar(fill = "#7F3FBF", col=NA))
title_txt <- textGrob(
  "Metriken und Konfusionsmatrix",
  gp = gpar(fontsize=28, fontface="bold", col="white"),
  x  = 0.5, y = 0.5
)
title_panel <- grobTree(title_bg, title_txt)

# 7) PDF-Erzeugung (A4 Querformat) -------------------------------------------
pdf("Präsentation/Folie8_Metriken_ConfMatrix.pdf", width=11.7, height=8.3)

# Layout analog Folie4: 4 Spalten je Zeile
# Zeile 1 => Titel, Zeile 2 => links conf_matrix, rechts metrics
# Zeile 3 => Kommentarbox
grid.arrange(
  title_panel,    # (1) -> Zeile1, Spalte1..4
  conf_panel,     # (2) -> Zeile2, Spalte1..2
  metrics_panel,  # (3) -> Zeile2, Spalte3..4
  comment_panel,  # (4) -> Zeile3, Spalte1..4
  layout_matrix = rbind(
    c(1, 1, 1, 1),   # Titel in ganzer Breite
    c(2, 2, 3, 3),   # Links ConfMatrix, rechts Balkendiagramm
    c(4, 4, 4, 4)    # Kommentarbox in ganzer Breite
  ),
  heights = c(0.8, 3, 2)
)

dev.off()


###############################################################################
# FOLIE 9 – Modellleistung & ROC-Kurve (1 Seite, kein Leerblatt, Layout = Folie 3–8)
###############################################################################

# Pakete laden ----------------------------------------------------------------
library(tidyverse)
library(caret)
library(pROC)
library(grid)
library(gridExtra)
library(ggplot2)

# 1) Daten & Modell sicherstellen ---------------------------------------------
if (!exists("glm_model") || !exists("train_data_clean")) {
  
  train_data <- read.csv("bloodtrain.csv", stringsAsFactors = FALSE)
  
  colnames(train_data) <- c(
    "ID",
    "Monate Letzte Spende",
    "Anzahl Spenden",
    "Gesamtvolumen",
    "Monate Erste Spende",
    "Spende Maerz 2007"
  )
  
  # Neuer Prädiktor
  train_data$`Monate Letzte Spende`[train_data$`Monate Letzte Spende` == 0] <- NA
  train_data$`Spenden Rate` <- with(train_data,
                                    ifelse(is.na(`Monate Letzte Spende`), NA,
                                           `Anzahl Spenden` / `Monate Letzte Spende`))
  
  train_data$`Spende Maerz 2007` <- factor(train_data$`Spende Maerz 2007`, levels = c("0", "1"))
  
  train_data_clean <- subset(train_data, !is.na(`Spende Maerz 2007`) & !is.na(`Spenden Rate`))
  
  glm_model <- glm(
    `Spende Maerz 2007` ~ `Anzahl Spenden` + `Monate Letzte Spende` +
      `Spenden Rate` + `Monate Erste Spende`,
    data = train_data_clean,
    family = binomial(link = "logit")
  )
}

# 2) Vorhersagen & ROC-Kurve --------------------------------------------------
train_data_clean$Prob_1 <- predict(glm_model, newdata = train_data_clean, type = "response")

roc_obj <- roc(
  response  = train_data_clean$`Spende Maerz 2007`,
  predictor = train_data_clean$Prob_1,
  levels    = c("0", "1")
)

# ROC-Plot erzeugen (nur ggroc, kein zusätzlicher geom_line)
p_roc <- ggroc(roc_obj, legacy.axes = TRUE) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey50") +
  labs(
    title = paste0("ROC-Kurve (AUC = ", round(auc(roc_obj), 3), ")"),
    x = "1 - Spezifität",
    y = "Sensitivität"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "none"
  )

roc_panel <- ggplotGrob(p_roc)

# 3) Textzusammenfassung (links) ----------------------------------------------
roc_text <- paste(
  "Modell: Logistische Regression\n",
  "Metrik: ROC/AUC zur Einschätzung der Trennschärfe\n\n",
  "AUC = ", round(auc(roc_obj), 3), "\n",
  "Interpretation:\n• AUC nahe 1 → sehr gute Trennung\n• AUC ~ 0.5 → zufällig\n",
  sep = ""
)

roc_txt_grob <- textGrob(
  roc_text,
  x = 0.03, y = 0.97,
  just = c("left", "top"),
  gp = gpar(col = "black", fontsize = 11.5, fontfamily = "mono")
)

roc_left_panel <- grobTree(rectGrob(gp = gpar(fill = "#E3F2FD", col = NA)), roc_txt_grob)

# 4) Titelbalken --------------------------------------------------------------
title_bg  <- rectGrob(gp = gpar(fill = "#7F3FBF", col = NA))
title_txt <- textGrob(
  "Modellleistung & ROC-Kurve",
  gp = gpar(fontsize = 28, fontface = "bold", col = "white"),
  x = 0.5, y = 0.5
)
title_panel <- grobTree(title_bg, title_txt)

# 5) Kommentarbox unten -------------------------------------------------------
comment_text <- textGrob(
  "Erkenntnisse:\n\n• AUC-Wert > 0.8 → sehr gute Trennschärfe\n• ROC-Kurve zeigt stabiles Modellverhalten\n\nSchlussfolgerung:\n• Modell kann Zielvariable gut vorhersagen.",
  x = 0.05, y = 0.95, just = c("left", "top"),
  gp = gpar(col = "black", fontsize = 12)
)
comment_panel <- grobTree(rectGrob(gp = gpar(fill = rgb(1, 1, 1, 0.2), col = NA)), comment_text)

# 6) PDF-Ausgabe --------------------------------------------------------------
pdf("Präsentation/Folie9_ROC_Modellleistung.pdf", width = 11.7, height = 8.3)

# ⚠️ KEIN grid.newpage() und KEIN grid.rect() → keine leere Seite!
# Direktes Layout (eine Seite, sauber)

grid.arrange(
  title_panel,
  arrangeGrob(roc_left_panel, roc_panel, ncol = 2, widths = c(2, 3)),
  comment_panel,
  nrow = 3,
  heights = c(0.8, 4.5, 1.7)
)

dev.off()


