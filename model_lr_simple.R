##############################################################################
# SCRIPT: Erzeugt Folien (*.png) im Ordner "Präsentation"
##############################################################################
# Hinweis: 
# - Wandelt PDF-Dateien in PNG (Folie1 & Folie2) um.
# - Läd CSV-Daten, erstellt Summary (Folie4), einen Boxplot (Folie6) 
#   und ein Histogramm-Grid (Folie7) mit eingebetteten Erkenntnisboxen.
##############################################################################
library(here)
presentation_dir <- here::here("Präsentation")
if (!dir.exists(presentation_dir)) dir.create(presentation_dir)

# --- Folie 1 & Folie 2: PDF -> PNG -------------------------
library(pdftools)
pdf_convert(
  pdf       = here::here("Titelseite.pdf"),
  pages     = 1,
  filenames = file.path(presentation_dir, "Folie1.png"),
  dpi       = 300
)
pdf_convert(
  pdf       = here::here("Workflow.pdf"),
  pages     = 1,
  filenames = file.path(presentation_dir, "Folie2.png"),
  dpi       = 300
)

# --- Daten laden & Spalten anpassen -----------------------
train_data <- read.csv(here::here("bloodtrain.csv"))
test_data  <- read.csv(here::here("bloodtest.csv"))
colnames(train_data) <- c("ID", "Monate Letzte Spende", "Anzahl Spenden",
                          "Gesamtvolumen", "Monate Erste Spende", "Spende Maerz 2007")
colnames(test_data)  <- c("ID", "Monate Letzte Spende", "Anzahl Spenden",
                          "Gesamtvolumen", "Monate Erste Spende")

# --- Folie 4: Summary als PNG ------------------------------
train_sum <- capture.output(summary(train_data))
test_sum  <- capture.output(summary(test_data))
png(file.path(presentation_dir, "Folie4.png"), width = 800, height = 600)
par(mar=c(0,0,0,0))
plot.new()
text(0.5, 0.5, paste(
  "Trainingsdaten Zusammenfassung:",
  paste(train_sum, collapse = "\n"),
  "\n\nTestdaten Zusammenfassung:",
  paste(test_sum, collapse = "\n"),
  sep = "\n"), cex = 0.7)
dev.off()

# --- Folie 6: Boxplot mit Erkenntnistext -------------------
library(dplyr)
library(tidyr)
library(ggplot2)
# Kennzeichnung und Zusammenführen von Train- und Testdaten
train_data$Dataset <- "Train"
test_data$Dataset  <- "Test"
df <- bind_rows(
  select(train_data, Dataset, `Anzahl Spenden`, Gesamtvolumen,
         `Monate Erste Spende`, `Monate Letzte Spende`),
  select(test_data, Dataset, `Anzahl Spenden`, Gesamtvolumen,
         `Monate Erste Spende`, `Monate Letzte Spende`)
)
# z-Standardisierung und Umwandlung in Long-Format
df_scaled <- df %>% mutate(across(!Dataset, scale))
df_long <- pivot_longer(df_scaled,
                        cols = c("Anzahl Spenden", "Gesamtvolumen",
                                 "Monate Erste Spende", "Monate Letzte Spende"),
                        names_to = "Variable", values_to = "Wert")
p <- ggplot(df_long, aes(x = Variable, y = Wert, fill = Dataset)) +
  geom_boxplot(notch = TRUE, notchwidth = 0.3, outlier.shape = 21,
               alpha = 0.7, color = "black", size = 1) +
  labs(title = "Skalierte numerische Variablen", y = "Skalierter Wert",
       caption = "Erkenntnisse:\n• Boxplots zeigen die Verteilung z-standardisierter Variablen\nSchlussfolgerung:\n• Trainings- und Testdaten gehören zur gleichen Population") +
  theme_minimal(base_size = 14) +
  theme(plot.caption = element_text(hjust = 0.5, face = "bold", color = "violet"),
        panel.background = element_rect(fill = "#ffe6f7", color = NA),
        plot.background  = element_rect(fill = "#ffe6f7", color = NA))
ggsave(filename = file.path(presentation_dir, "Folie6.png"),
       plot = p, width = 10, height = 5, dpi = 1200, bg = "#ffe6f7")

# --- Folie 7: Histogramm-Grid mit Erkenntnisbox -----------
library(gridExtra)
library(grid)
plot1 <- ggplot(df, aes(x = `Monate Letzte Spende`, fill = Dataset)) +
  geom_histogram(position = "dodge", bins = 20, alpha = 0.7, color = "black") +
  labs(title = "Monate seit letzter Spende", x = "Monate", y = "Häufigkeit") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_minimal(base_size = 16)
plot2 <- ggplot(df, aes(x = `Anzahl Spenden`, fill = Dataset)) +
  geom_histogram(position = "dodge", bins = 15, alpha = 0.7, color = "black") +
  labs(title = "Anzahl Spenden", x = "Anzahl", y = "Häufigkeit") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_minimal(base_size = 16)
plot3 <- ggplot(df, aes(x = Gesamtvolumen, fill = Dataset)) +
  geom_histogram(position = "dodge", bins = 20, alpha = 0.7, color = "black") +
  labs(title = "Gesamtvolumen Spenden", x = "Volumen", y = "Häufigkeit") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_minimal(base_size = 16)
plot4 <- ggplot(df, aes(x = `Monate Erste Spende`, fill = Dataset)) +
  geom_histogram(position = "dodge", bins = 20, alpha = 0.7, color = "black") +
  labs(title = "Monate seit erster Spende", x = "Monate", y = "Häufigkeit") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_minimal(base_size = 16)
grid_plots <- arrangeGrob(plot1, plot2, plot3, plot4, ncol = 2)
# Erkenntnisbox als separater grob
annotation <- textGrob("Erkenntnisse:\n• Ähnliche Verteilungen bei beiden Datensätzen\n• Leichte Unterschiede bei Anzahl Spenden\nSchlussfolgerung:\n• Unterschiede können die Modellleistung beeinflussen",
                       gp = gpar(fontsize = 12, fontface = "bold", col = "violet"), 
                       just = "center")
final_plot <- arrangeGrob(grid_plots, annotation, ncol = 1, heights = c(4, 1))
ggsave(filename = file.path(presentation_dir, "Folie7.png"),
       plot = final_plot, width = 10, height = 7, dpi = 300)
