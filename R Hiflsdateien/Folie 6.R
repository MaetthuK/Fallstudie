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

# KEIN grid.newpage(), KEIN grid.rect(fill="white") => keine leere Seite 1

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
