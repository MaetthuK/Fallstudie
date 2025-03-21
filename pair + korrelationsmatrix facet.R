##############################################################################
# 3.3) PAIRS-PLOT & KORRELATIONSMATRIX NEBENEINANDER (OHNE VERZERRUNG)
# ----------------------------------------------------------------------------
# Ziel:
#   - Wie bisher: Leerzeichen in den Spaltennamen (z.B. "Monate Letzte Spende").
#   - Neuer Praediktor "Spendetakt" = (Anzahl Spenden) / (Monate Letzte Spende).
#   - 0 in "Monate Letzte Spende" => NA setzen, um Division durch 0 zu vermeiden.
#   - Links: Pairs-Plot, rechts: Korrelationsmatrix, gemeinsam in einem Plot
#     nebeneinander, ohne dass die Korrelationsmatrix verzerrt wird.
##############################################################################

# 1) DATENSATZ EINLESEN UND VORBEREITEN
# ----------------------------------------------------------------------------
train_data <- read.csv("bloodtrain.csv", stringsAsFactors = FALSE)

# Spaltennamen wie in den vorherigen Plots
colnames(train_data) <- c(
  "ID",
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende",
  "Spende Maerz 2007"
)

# 2) 0 -> NA in "Monate Letzte Spende"
# ----------------------------------------------------------------------------
# Prüfen, ob es überhaupt Werte == 0 gibt
has_zero <- any(train_data$`Monate Letzte Spende` == 0, na.rm = TRUE)
if (has_zero) {
  train_data$`Monate Letzte Spende`[
    train_data$`Monate Letzte Spende` == 0
  ] <- NA
}

# 3) NEUER PRAEDIKTOR "SPENDETAKT"
# ----------------------------------------------------------------------------
# Spendetakt = (Anzahl Spenden) / (Monate Letzte Spende)
train_data$Spendetakt <- train_data$`Anzahl Spenden` /
  train_data$`Monate Letzte Spende`

# 4) KORRELATIONSMATRIX BERECHNEN
# ----------------------------------------------------------------------------
library(corrplot)  # Falls nicht installiert: install.packages("corrplot")
num_data <- train_data[sapply(train_data, is.numeric)]

# "ID" entfernen, falls vorhanden und unnoetig
if ("ID" %in% names(num_data)) {
  num_data <- num_data[, !names(num_data) %in% "ID"]
}

# Korrelation mit pairwise.complete.obs
cor_matrix <- cor(num_data, use = "pairwise.complete.obs")

# 5) PAKETE FÜR BASE-PLOT-KOMBINATION LADEN
# ----------------------------------------------------------------------------
library(gridGraphics)  # Um Base-Plots in Grid-Objekte zu verwandeln
library(gridExtra)     # Um Grid-Objekte nebeneinander anzuordnen
library(grid)          # Fuer grid.newpage(), viewport(), etc.

# 6) PAIRS-PLOT-FUNKTION DEFINIEREN
# ----------------------------------------------------------------------------
numeric_vars <- c(
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende",
  "Spendetakt"
)

# Farbgebung: 1 => grün, 0 => rot
point_colors <- ifelse(train_data$`Spende Maerz 2007` == 1, "green", "red")

plot_pairs <- function() {
  pairs(
    train_data[, numeric_vars],
    col       = point_colors,
    pch       = 16,       # Geschlossene Kreise
    cex       = 1.2,      # Punktgroesse
    font.main = 2,        # Fettschrift fuer Titel
    font.lab  = 2,
    main      = "Pairs-Plot der Trainingsdaten",
    cex.main  = 1.4,
    cex.labels= 1.2
  )
}

# 7) KORRELATIONSMATRIX-FUNKTION DEFINIEREN (PTY = "S" ERZWINGT QUADRAT)
# ----------------------------------------------------------------------------
plot_corr <- function() {
  par(pty = "s")  # Quadratisches Plotfenster erzwingen
  
  corrplot(
    cor_matrix,
    method      = "color",   # Farbkacheln
    type        = "upper",   # Nur obere Haelfte
    tl.col      = "black",
    tl.srt      = 45,
    addCoef.col = "black",   # Zahlen in Schwarz
    number.cex  = 0.8,
    title       = "Korrelationsmatrix (mit Spendetakt)",
    mar         = c(0, 0, 2, 0),
    cex.main    = 1.4,
    font.main   = 2
  )
}

# 8) PAIRS-PLOT ALS GROB ERFASSEN
# ----------------------------------------------------------------------------
grid.newpage()       # Neue Grafikseite
grid.echo(plot_pairs)
pairs_grob <- grid.grab()

# 9) KORRELATIONSMATRIX ALS GROB ERFASSEN
# ----------------------------------------------------------------------------
grid.newpage()       # Neue Grafikseite
grid.echo(plot_corr)
corr_grob <- grid.grab()

# 10) BEIDE GRAFIKEN NEBENEINANDER ANORDNEN (BREITENVERHAELTNIS STEUERN)
# ----------------------------------------------------------------------------
grid.newpage()  # Neue Grafikseite fuer das finale Layout

# widths = c(3, 2) => erster Plot 3 Teile, zweiter Plot 2 Teile
# Falls die Korrelationsmatrix mehr Platz benoetigt, einfach z.B. (2,3) setzen
grid.arrange(
  pairs_grob,
  corr_grob,
  ncol   = 2,
  widths = c(3, 2)
)

# FERTIG: LINKS DER PAIRS-PLOT, RECHTS DIE KORRELATIONSMATRIX
#         OHNE VERZERRUNG, NEBENEINANDER IN EINEM EINZIGEN PLOT.
