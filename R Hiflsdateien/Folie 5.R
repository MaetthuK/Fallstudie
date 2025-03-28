library(ggplot2)
library(grid)
library(gridExtra)
library(here)

# Vorausgesetzt, combined_data und train_data sind bereits im Workspace

# p1: Histogramm - Monate Letzte Spende (binwidth=1)
p1 <- ggplot(combined_data, aes(x = `Monate Letzte Spende`, fill = Dataset)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Monate Letzte Spende", x = "Monate", y = "Frequenz") +
  theme_minimal()

# p2: Histogramm - Anzahl Spenden (binwidth=1)
p2 <- ggplot(combined_data, aes(x = `Anzahl Spenden`, fill = Dataset)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Anzahl Spenden", x = "Anzahl", y = "Frequenz") +
  theme_minimal()

# p3: Histogramm - Gesamtvolumen (binwidth=500)
p3 <- ggplot(combined_data, aes(x = Gesamtvolumen, fill = Dataset)) +
  geom_histogram(binwidth = 500, position = "dodge", color = "black") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Gesamtvolumen", x = "Volumen", y = "Frequenz") +
  theme_minimal()

# p4: Histogramm - Monate Erste Spende (binwidth=5)
p4 <- ggplot(combined_data, aes(x = `Monate Erste Spende`, fill = Dataset)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  labs(title = "Monate Erste Spende", x = "Monate", y = "Frequenz") +
  theme_minimal()

# p5: Balkendiagramm - Klassenverteilung Spende Maerz 2007 (Train)
p5 <- ggplot(train_data, aes(x = factor(`Spende Maerz 2007`), fill = "Train")) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Train" = "blue")) +
  scale_x_discrete(labels = c("0" = "nein", "1" = "ja")) +
  labs(title = "Klassenverteilung (Train)", x = "Spende im März 2007?", y = "Anzahl") +
  theme_minimal() +
  theme(legend.position = "none")

# Titelbalken grob
title_bg  <- rectGrob(gp = gpar(fill = "#7F3FBF", col = NA))
title_txt <- textGrob(
  "Histogramme Train vs. Test",
  gp = gpar(fontsize = 28, col = "white", fontface = "bold")
)
title_panel <- grobTree(title_bg, title_txt)

# Kommentarbox
comment_bg   <- rectGrob(gp = gpar(fill = rgb(1, 1, 1, 0.2), col = NA))
comment_text <- textGrob(
  "Erkenntnisse:\n\n• Verteilungen ähnlich, \n  aber Test hat weniger hohe Spenderzahlen\n• Klassenverteilung: ~60% ja, 40% nein\n\nSchlussfolgerung:\n• Kein starker Train/Test-Shift\n• Modellauswahl kann auf stabilen Daten basieren",
  x = 0.05, y = 0.95, just = c("left", "top"),
  gp = gpar(col = "black", fontsize = 12)
)
comment_panel <- grobTree(comment_bg, comment_text)

# PDF erzeugen (A4 Querformat: 11.7 x 8.3 Zoll)
presentation_dir <- here::here("Präsentation")
pdf_filename <- file.path(presentation_dir, "Folie5_Histogramme_train_test.pdf")
pdf(pdf_filename, width = 11.7, height = 8.3)

# Anordnung via grid.arrange: 
# Zeile 1: Titel, Zeile 2: Histogramme p1 bis p4, Zeile 3: p5 und Kommentarbox
grid.arrange(
  title_panel,
  arrangeGrob(p1, p2, p3, p4, ncol = 4),
  arrangeGrob(p5, comment_panel, ncol = 2, widths = c(0.5, 0.5)),
  nrow = 3,
  heights = c(0.8, 3, 2)
)

dev.off()