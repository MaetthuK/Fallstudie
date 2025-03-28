###############################################################################
# GESAMTES SKRIPT für die Erstellung einer mehrseitigen PDF-Präsentation
# Name der finalen PDF: "Präsentation.pdf"
# 
# Folien 1, 2 und 10 sind nur Platzhalter;
# Folie 3–9 enthalten alle Diagramme und Statistiken. 
# Mit diesem Code sollten keine Leerseiten mehr entstehen.
###############################################################################


# 1) Pakete laden -------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(pROC)
library(gridExtra)  # für grid.arrange, tableGrob etc.
library(grid)       # für grid.rect, textGrob
library(GGally)     # für ggpairs

# 2) CSV-Dateien (Train/Test) einlesen ----------------------------------------
train_data <- read.csv("bloodtrain.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
test_data  <- read.csv("bloodtest.csv",  header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Spaltennamen anpassen (Train)
colnames(train_data) <- c(
  "ID",
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende",
  "Spende Maerz 2007"
)

# Spaltennamen anpassen (Test)
colnames(test_data) <- c(
  "ID",
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende"
)

# Kennzeichnen, aus welchem Datensatz (für vergleichende Plots)
train_data$Dataset <- "Train"
test_data$Dataset  <- "Test"

# Kombinierte Daten (für Boxplots, Histogramme usw.)
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

# Kurzer Blick in die Konsole
cat("Train-Daten:\n")
str(train_data)
cat("\nTest-Daten:\n")
str(test_data)

###############################################################################
# GLM-Modell-Vorbereitung (für Folien 7–9)
###############################################################################

# Daten für Modell: 
glm_train <- train_data %>%
  mutate(
    `Monate Letzte Spende` = ifelse(`Monate Letzte Spende` == 0, NA, `Monate Letzte Spende`),
    `Spenden Rate` = ifelse(
      is.na(`Monate Letzte Spende`), NA,
      `Anzahl Spenden` / `Monate Letzte Spende`
    ),
    `Spende Maerz 2007` = factor(`Spende Maerz 2007`, levels = c("0", "1"))
  ) %>%
  filter(!is.na(`Spende Maerz 2007`) & !is.na(`Spenden Rate`))

glm_model <- glm(
  `Spende Maerz 2007` ~ `Anzahl Spenden` + `Monate Letzte Spende` +
    `Spenden Rate` + `Monate Erste Spende`,
  data   = glm_train,
  family = binomial(link = "logit")
)

# Für Konfusionsmatrix (Folie 8)
glm_train$Predictions <- predict(glm_model, newdata = glm_train, type = "response")
glm_train$Predictions <- ifelse(glm_train$Predictions >= 0.5, "1", "0")
glm_train$Predictions <- factor(glm_train$Predictions, levels = c("0", "1"))
conf_mat <- confusionMatrix(
  data      = glm_train$Predictions,
  reference = glm_train$`Spende Maerz 2007`
)

# Für ROC (Folie 9)
glm_train$Prob_1 <- predict(glm_model, newdata = glm_train, type = "response")
roc_obj <- roc(
  response  = glm_train$`Spende Maerz 2007`,
  predictor = glm_train$Prob_1,
  levels    = c("0", "1")
)


###############################################################################
# PDF-Erstellung für ALLE Folien (1 bis 10) in EINEM Dokument
###############################################################################
pdf("Präsentation.pdf", width = 11.7, height = 8.3)  # A4 Querformat

###############################################################################
# FOLIE 1 (Platzhalter)
###############################################################################
grid.newpage()
grid.rect(gp = gpar(fill = "#CCCCCC", col = NA))
grid.text(
  "FOLIE 1 (noch leer / Platzhalter)",
  x = 0.5, y = 0.5,
  gp = gpar(fontsize = 24, fontface = "bold")
)

###############################################################################
# FOLIE 2 (Platzhalter)
###############################################################################
grid.newpage()
grid.rect(gp = gpar(fill = "#DDDDDD", col = NA))
grid.text(
  "FOLIE 2 (noch leer / Platzhalter)",
  x = 0.5, y = 0.5,
  gp = gpar(fontsize = 24, fontface = "bold")
)

###############################################################################
# FOLIE 3: STATISTISCHE KENNZAHLEN (Train/Test)
###############################################################################
grid.newpage()  # wir bauen diese Folie komplett mit grid-Befehlen auf

summary_train <- summary(train_data |> select(-Dataset))
summary_test  <- summary(test_data  |> select(-Dataset))

train_text <- paste(capture.output(summary_train), collapse = "\n")
test_text  <- paste(capture.output(summary_test), collapse = "\n")

train_rows <- nrow(train_data)
test_rows  <- nrow(test_data)

spende_counts <- table(train_data$`Spende Maerz 2007`)
spende_text <- paste0(
  "Spende März 2007 ja/nein:\nja:   ", spende_counts["1"],
  "\nnein: ", spende_counts["0"]
)

# Dynamische Höhenberechnung
line_height <- 0.015
train_lines <- length(capture.output(summary_train))
test_lines  <- length(capture.output(summary_test))

train_box_height <- 0.07 + train_lines * line_height
test_box_height  <- 0.07 + test_lines * line_height

# Y-Positionen
y_train_title <- 0.84
y_train_box   <- y_train_title - 0.035

y_output_box  <- y_train_box - train_box_height - 0.025  
y_test_title  <- y_output_box - 0.18
y_test_box    <- y_test_title - 0.04

# Hintergrund
grid.rect(gp = gpar(fill = "white", col = NA))
# Kopfbereich
grid.rect(
  x = 0.5, y = 1, width = 1, height = 0.12,
  just = c("center", "top"),
  gp = gpar(fill = "#005DAA", col = NA)
)
grid.text(
  "Statistische Kennzahlen für Trainings– und Testdaten",
  x = 0.5, y = 0.965,
  gp = gpar(fontsize = 24, fontface = "bold", col = "white")
)

# Trainingsdaten
grid.text(
  "2.1.1 Trainingsdaten Zusammenfassung",
  x = 0.06, y = y_train_title,
  just = c("left", "top"),
  gp = gpar(fontsize = 14, fontface = "bold")
)
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

# Output Variable
pushViewport(viewport(
  x = 0.5, y = y_output_box,
  width = 0.33, height = 0.10,
  just = c("center", "top")
))
grid.rect(gp = gpar(fill = "#0277BD", col = "black", lwd = 1.5))
grid.text("Output Variable",
          x = 0.05, y = 0.90, just = c("left", "top"),
          gp = gpar(fontsize = 11, fontface = "bold", col = "white"))
grid.text(spende_text,
          x = 0.05, y = 0.68, just = c("left", "top"),
          gp = gpar(fontsize = 10, col = "white", fontfamily = "mono"))
popViewport()

# Testdaten
grid.text(
  "2.1.3 Testdaten Zusammenfassung",
  x = 0.06, y = y_test_title,
  just = c("left", "top"),
  gp = gpar(fontsize = 14, fontface = "bold")
)
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


###############################################################################
# FOLIE 4: Boxplots (Train vs. Test)
###############################################################################
grid.newpage()

# Diagramme erzeugen
p1 <- ggplot(combined_data, aes(x = Dataset, y = `Monate Letzte Spende`, fill = Dataset)) +
  geom_boxplot(color = "black", notch = TRUE) +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Monate Letzte Spende", x = "Datensatz", y = "Monate") +
  theme_minimal()

p2 <- ggplot(combined_data, aes(x = Dataset, y = `Anzahl Spenden`, fill = Dataset)) +
  geom_boxplot(color = "black", notch = TRUE) +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Anzahl Spenden", x = "Datensatz", y = "Anzahl") +
  theme_minimal()

p3 <- ggplot(combined_data, aes(x = Dataset, y = Gesamtvolumen, fill = Dataset)) +
  geom_boxplot(color = "black", notch = TRUE) +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Gesamtvolumen", x = "Datensatz", y = "Volumen") +
  theme_minimal()

p4 <- ggplot(combined_data, aes(x = Dataset, y = `Monate Erste Spende`, fill = Dataset)) +
  geom_boxplot(color = "black", notch = TRUE) +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Monate Erste Spende", x = "Datensatz", y = "Monate") +
  theme_minimal()

# Zusätzliche Grobs
title_bg   <- rectGrob(gp = gpar(fill = "#7F3FBF", col = NA))
title_txt  <- textGrob(
  "Boxplots Train vs. Test",
  gp = gpar(fontsize = 28, col = "white", fontface = "bold")
)
title_panel <- grobTree(title_bg, title_txt)

comment_bg   <- rectGrob(gp = gpar(fill = rgb(1,1,1,0.2), col = NA))
comment_text <- textGrob(
  "Erkenntnisse:\n\n• Verteilungen der Merkmale\n• Kein starker Train/Test-Shift",
  x = 0.05, y = 0.95, just = c("left", "top"),
  gp = gpar(col = "black", fontsize = 12)
)
comment_panel <- grobTree(comment_bg, comment_text)

# Anordnung auf einer Seite (newpage=FALSE, da wir schon grid.newpage() gemacht haben)
grid.arrange(
  title_panel, p1, p2, p3, p4, comment_panel,
  layout_matrix = rbind(
    c(1, 1, 1, 1),
    c(2, 3, 4, 5),
    c(6, 6, 6, 6)
  ),
  heights = c(0.8, 3, 2),
  newpage = FALSE
)


###############################################################################
# FOLIE 5: Histogramme (Train vs. Test) + Klassenverteilung
###############################################################################
grid.newpage()

p1 <- ggplot(combined_data, aes(x = `Monate Letzte Spende`, fill = Dataset)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Monate Letzte Spende", x = "Monate", y = "Frequenz") +
  theme_minimal()

p2 <- ggplot(combined_data, aes(x = `Anzahl Spenden`, fill = Dataset)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Anzahl Spenden", x = "Anzahl", y = "Frequenz") +
  theme_minimal()

p3 <- ggplot(combined_data, aes(x = Gesamtvolumen, fill = Dataset)) +
  geom_histogram(binwidth = 500, position = "dodge", color = "black") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Gesamtvolumen", x = "Volumen", y = "Frequenz") +
  theme_minimal()

p4 <- ggplot(combined_data, aes(x = `Monate Erste Spende`, fill = Dataset)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Monate Erste Spende", x = "Monate", y = "Frequenz") +
  theme_minimal()

p5 <- ggplot(train_data, aes(x = factor(`Spende Maerz 2007`), fill = "Train")) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Train" = "blue")) +
  scale_x_discrete(labels = c("0" = "nein", "1" = "ja")) +
  labs(title = "Klassenverteilung (Train)", x = "Spende im März 2007?", y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")

title_bg   <- rectGrob(gp = gpar(fill = "#7F3FBF", col = NA))
title_txt  <- textGrob(
  "Histogramme Train vs. Test",
  gp = gpar(fontsize = 28, col = "white", fontface = "bold")
)
title_panel <- grobTree(title_bg, title_txt)

comment_bg   <- rectGrob(gp = gpar(fill = rgb(1,1,1,0.2), col = NA))
comment_text <- textGrob(
  "Erkenntnisse:\n\n• Verteilungen ähnlich\n• Test hat weniger hohe Spenderzahlen\n• ~60% 'ja', 40% 'nein'",
  x = 0.05, y = 0.95, just = c("left", "top"),
  gp = gpar(col = "black", fontsize = 12)
)
comment_panel <- grobTree(comment_bg, comment_text)

grid.arrange(
  title_panel, p1, p2, p3, p4, p5, comment_panel,
  layout_matrix = rbind(
    c(1, 1, 1, 1),
    c(2, 3, 4, 5),
    c(6, 6, 7, 7)
  ),
  heights = c(0.8, 3, 2),
  newpage = FALSE
)


###############################################################################
# FOLIE 6: Pairs-Plot & Korrelationsmatrix
###############################################################################
grid.newpage()

# Zusätzliche Variable "Spendetakt" ins train_data
train_data$Spendetakt <- with(
  train_data,
  ifelse(
    `Monate Letzte Spende` == 0,
    NA,
    `Anzahl Spenden` / `Monate Letzte Spende`
  )
)

num_data   <- train_data %>% select(where(is.numeric), -ID)
cor_matrix <- cor(num_data, use = "pairwise.complete.obs")

# Pairs-Plot
ggp <- ggpairs(
  train_data,
  columns = c("Monate Letzte Spende", "Anzahl Spenden", 
              "Gesamtvolumen", "Monate Erste Spende", "Spendetakt"),
  mapping = aes(color = factor(`Spende Maerz 2007`)),
  title   = "Pairs-Plot Trainingsdaten"
) + theme_minimal()

ggp_grob <- as.grob(function() print(ggp))

# Korrelation
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
    cex.main    = 1.2,
    mar         = c(0, 0, 2, 0),
    font.main   = 2
  )
})

comment_1 <- textGrob(
  "Erkenntnisse:\n• Hohe Korrelation von Spenden & Volumen\n• Spendetakt korreliert ebenso",
  gp = gpar(col = "black", fontsize = 12),
  x = 0.05, y = 0.95, just = c("left", "top")
)
comment_2 <- textGrob(
  "Fazit:\n• 'Gesamtvolumen' redundant\n• 'Spendetakt' als Feature nutzen",
  gp = gpar(col = "black", fontsize = 12),
  x = 0.05, y = 0.95, just = c("left", "top")
)

title_bg  <- rectGrob(gp = gpar(fill = "#7F3FBF", col = NA))
title_txt <- textGrob(
  "Beziehung der Variablen",
  gp = gpar(fontsize = 24, col = "white", fontface = "bold"),
  x = 0.5, y = 0.5
)
title_panel <- grobTree(title_bg, title_txt)

grid.arrange(
  title_panel,
  ggp_grob,
  corr_grob,
  comment_1,
  comment_2,
  layout_matrix = rbind(
    c(1, 1),
    c(2, 3),
    c(4, 5)
  ),
  heights = c(0.15, 0.65, 0.2),
  newpage = FALSE
)


###############################################################################
# FOLIE 7: Modellerstellung & Variable Importance
###############################################################################
grid.newpage()

model_summary_txt <- capture.output(summary(glm_model))
summary_bg   <- rectGrob(gp = gpar(fill = "grey", col = NA))
summary_txt  <- textGrob(
  paste(model_summary_txt, collapse = "\n"),
  x = 0.03, y = 0.97,
  just = c("left", "top"),
  gp   = gpar(col = "black", fontsize = 10, fontfamily = "Courier")
)
summary_panel <- grobTree(summary_bg, summary_txt)

# Variable Importance
var_imp    <- varImp(glm_model, scale = FALSE)
var_imp_df <- data.frame(
  Variable   = rownames(var_imp),
  Importance = var_imp$Overall
)

p_varImp <- ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#3399FF", color = "black") +
  coord_flip() +
  labs(title = "Variable Importance (Logit)", x = "Prädiktor", y = "Wichtigkeit") +
  theme_minimal(base_size = 14)

plot_panel <- ggplotGrob(p_varImp)

title_bg  <- rectGrob(gp = gpar(fill = "#7F3FBF", col = NA))
title_txt <- textGrob(
  "Modellerstellung & Variable Importance",
  gp = gpar(fontsize = 28, fontface = "bold", col = "white"),
  x  = 0.5, y = 0.5
)
title_panel <- grobTree(title_bg, title_txt)

comment_bg   <- rectGrob(gp = gpar(fill = rgb(1,1,1,0.2), col = NA))
comment_text <- textGrob(
  "Erkenntnisse:\n\n• Höchste Bedeutung: Monate Letzte Spende\n• 'Spenden Rate' liefert Mehrwert",
  x = 0.05, y = 0.95, just = c("left", "top"),
  gp = gpar(col = "black", fontsize = 12)
)
comment_panel <- grobTree(comment_bg, comment_text)

grid.arrange(
  title_panel, summary_panel, plot_panel, comment_panel,
  layout_matrix = rbind(
    c(1, 1, 1, 1),
    c(2, 2, 3, 3),
    c(4, 4, 4, 4)
  ),
  heights = c(0.8, 3, 2),
  newpage = FALSE
)


###############################################################################
# FOLIE 8: Metriken und Konfusionsmatrix
###############################################################################
grid.newpage()

conf_text_lines <- capture.output(conf_mat)
conf_bg  <- rectGrob(gp = gpar(fill = "#002B55", col = NA))
conf_txt <- textGrob(
  paste(conf_text_lines, collapse = "\n"),
  x    = 0.03, y = 0.97,
  just = c("left", "top"),
  gp   = gpar(col = "white", fontsize = 10, fontfamily = "Courier")
)
conf_panel <- grobTree(conf_bg, conf_txt)

acc  <- conf_mat$overall["Accuracy"]
sens <- conf_mat$byClass["Sensitivity"]
prec <- conf_mat$byClass["Pos Pred Value"]
rec  <- conf_mat$byClass["Sensitivity"]  

metrics_df  <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Precision", "Recall"),
  Value  = c(acc, sens, prec, rec)
)

p_metrics <- ggplot(metrics_df, aes(x = reorder(Metric, Value), y = Value)) +
  geom_bar(stat = "identity", fill = "#3399FF", color = "black") +
  coord_flip() +
  ylim(0, 1) +
  labs(title = "Kennzahlen (Train)", x = "Metrik", y = "Wert") +
  theme_minimal(base_size = 14)

metrics_panel <- ggplotGrob(p_metrics)

comment_bg   <- rectGrob(gp = gpar(fill = rgb(1,1,1,0.2), col = NA))
comment_text <- textGrob(
  "Erkenntnisse:\n\n• Modellwerte solide\n• Sensitivität = Recall\n\nFeintuning möglich",
  x=0.05, y=0.95, just=c("left","top"),
  gp=gpar(col="black", fontsize=12)
)
comment_panel <- grobTree(comment_bg, comment_text)

title_bg  <- rectGrob(gp = gpar(fill = "#7F3FBF", col=NA))
title_txt <- textGrob(
  "Metriken und Konfusionsmatrix",
  gp = gpar(fontsize=28, fontface="bold", col="white"),
  x  = 0.5, y = 0.5
)
title_panel <- grobTree(title_bg, title_txt)

grid.arrange(
  title_panel, conf_panel, metrics_panel, comment_panel,
  layout_matrix = rbind(
    c(1, 1, 1, 1),
    c(2, 2, 3, 3),
    c(4, 4, 4, 4)
  ),
  heights = c(0.8, 3, 2),
  newpage = FALSE
)


###############################################################################
# FOLIE 9: Modellleistung & ROC-Kurve
###############################################################################
grid.newpage()

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

roc_text <- paste(
  "Modell: Logistische Regression\n",
  "Metrik: ROC/AUC zur Trennschärfe\n\n",
  "AUC = ", round(auc(roc_obj), 3), "\n",
  "Interpretation:\n• 1 = perfekte Trennung, 0.5 = Zufall",
  sep = ""
)

roc_txt_grob <- textGrob(
  roc_text,
  x = 0.03, y = 0.97,
  just = c("left", "top"),
  gp = gpar(col = "black", fontsize = 11.5, fontfamily = "mono")
)
roc_left_panel <- grobTree(
  rectGrob(gp = gpar(fill = "#E3F2FD", col = NA)),
  roc_txt_grob
)

title_bg  <- rectGrob(gp = gpar(fill = "#7F3FBF", col = NA))
title_txt <- textGrob(
  "Modellleistung & ROC-Kurve",
  gp = gpar(fontsize = 28, fontface = "bold", col = "white"),
  x = 0.5, y = 0.5
)
title_panel <- grobTree(title_bg, title_txt)

comment_text <- textGrob(
  "Erkenntnisse:\n\n• AUC > 0.8 → gute Trennschärfe\n• Nächster Schritt: Testdatenprädiktion",
  x = 0.05, y = 0.95, just = c("left", "top"),
  gp = gpar(col = "black", fontsize = 12)
)
comment_panel <- grobTree(
  rectGrob(gp = gpar(fill = rgb(1,1,1,0.2), col = NA)),
  comment_text
)

grid.arrange(
  title_panel,
  arrangeGrob(roc_left_panel, roc_panel, ncol = 2, widths = c(2, 3)),
  comment_panel,
  nrow = 3,
  heights = c(0.8, 4.5, 1.7),
  newpage = FALSE
)

###############################################################################
# FOLIE 10 (Platzhalter)
###############################################################################
grid.newpage()
grid.rect(gp = gpar(fill = "#EEEEEE", col = NA))
grid.text(
  "FOLIE 10 (noch leer / Platzhalter)",
  x = 0.5, y = 0.5,
  gp = gpar(fontsize = 24, fontface = "bold")
)

###############################################################################
# PDF-Ende
###############################################################################
dev.off()

cat("\nFertig! Die Datei 'Präsentation.pdf' wurde erstellt, ohne zusätzliche Leerseiten.\n")
