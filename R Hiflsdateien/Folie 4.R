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