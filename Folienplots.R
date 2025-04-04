
# 1. Setup ----


## 1.1 Packages and Libraries ----
# install.packages(c("tidyverse", "ggplot2", "dplyr", "corrplot", "caret", "pROC"))

library(tidyverse)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(pROC)


## 1.2 CSV-Dateien Anbindung und Umbenennung Spalten und daten kombinierren----
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


# 2) COMBINE INTO ONE DATA FRAME (“combined_data”) FOR YOUR BOXPLOTS

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

## 1.3 Erste Zeilen anzeigen ----
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





#################################################################################
## Folie 4: Boxplot Verleich der Trainings- und Testdaten ----

# 4 Boxplots (oben) + lila Kommentarbox (unten) + Main Title oben


# Wir erstellen ein A4 Querformat für eine Prästenation.
# Die seite hat den Grunton violett (#7F3FBF)
# Die Schriften aller Daten sind weiss und auch die Rahmen

# Wir erstellen ein Präsentationsrasater:

# Wir legen 3 Zeilen x 4 Spalten an:

#   Zeile 1: Folienüberschrift: "
# "Vergleich der Trainings- und Testdaten", Grösse 24 Punkt umrandet weiss 
# (weisse Schrift, Weisser Rahmen da ja A4 Blatt violett)
# Zentriert  


#   Zeile 2: Boxplot (4 kombinierte einzelplots)
# die wir erstellen wollen. Der Die überschriften "Wert" und "Datensatz" weglassen
# Subplots 1,2,3,4 (jeweils 1 Boxplot)
# Jeweils links und rechts der Seite 15 Zeichen platz lassen (Flächte bleibt violett)  

#   Zeile 3:lila Kasten mit Erkenntnissen und Schlussfolgerungen
# Text: "Erkenntnisse" in weiss, 1.4 fach, fett, links oben
#Subplot 5,5,5,5 (ein großer lila Kasten)




# KOMPLETTES BEISPIEL
# - 3 Zeilen × 4 Spalten Layout
# - Zeile 1: Titel (Panel 1,1,1,1)
# - Zeile 2: 4 Boxplots (Panels 2,3,4,5)
# - Zeile 3: Lila Kommentarbox (Panel 6,6,6,6)
#
# Wichtig: wir rufen layout() NUR EINMAL, und zeichen alle Boxplots
# in der Reihenfolge, die layout(...) vorgibt. 


# Falls Du ein "reines" A4 Querformat willst, rufe z.B.:
# pdf("boxplot_annot_a4quer.pdf", width=11.69, height=8.27)
# # ... Code ...
# dev.off()


# 1) Layout definieren (3×4) ---

layout(
  matrix(c(
    1,1,1,1,   # Panel 1 => Titel über 4 Spalten
    2,3,4,5,   # Panels 2..5 => 4 Boxplots
    6,6,6,6    # Panel 6 => Kommentarbox unten über 4 Spalten
  ), nrow=3, byrow=TRUE),
  #heights = c(1.0, 3.5, 1.3)  # Erste Zeile: 1.0 Einheiten, zweite: 3.5, dritte: 1.3
  heights = c(1.0, 3.5, 2.5)  # Erste Zeile: 1.0 Einheiten, zweite: 3.5, dritte: 1.3
)


# 1) GLOBALE Farb/Schrift-Einstellungen ----

par(
  bg       = "black",   # Hintergrund des A4 Blattes
  fg       = "blue",      
  col.axis = "white",
  col.lab  = "white",
  col.main = "white"
)

# Panel 1 => Titel ----

par(mar=c(1, 1, 1, 1)) 
plot.new()
usr <- par("usr")
# Rechteck in violett (evtl. überflüssig, da bg=violett):
rect(usr[1], usr[3], usr[2], usr[4], col="#7F3FBF", border=NA)

# Großer weißer Text zentriert
title_str <- "Vergleich der Trainings- und Testdaten"
text(
  x=0.5, y=0.5,
  labels=title_str,
  col="white",
  cex=5,        # Schrifthöhe
  font=2,         # fett
  adj=c(0.5,0.5)  # zentriert
)


# Panels 2..5 => 4 Boxplots ----




# Panel 2 (Boxplot 1)
par(mar=c(3,3,2,1))
boxplot(
  `Monate Letzte Spende` ~ Dataset,
  data   = combined_data,
  notch  = TRUE,
  col    = c("red","blue"),
  border = "white",
  main   = "Monate Letzte Spende",
  xlab   = "",
  ylab   = ""
)

# Panel 3 (Boxplot 2)
par(mar=c(3,3,2,1))
boxplot(
  `Anzahl Spenden` ~ Dataset,
  data   = combined_data,
  notch  = TRUE,
  col    = c("red","blue"),
  border = "white",
  main   = "Anzahl Spenden",
  xlab   = "",
  ylab   = ""
)

# Panel 4 (Boxplot 3)
par(mar=c(3,3,2,1))
boxplot(
  Gesamtvolumen ~ Dataset,
  data   = combined_data,
  notch  = TRUE,
  col    = c("red","blue"),
  border = "white",
  main   = "Gesamtvolumen",
  xlab   = "",
  ylab   = ""
)

# Panel 5 (Boxplot 4)
par(mar=c(3,3,2,1))
boxplot(
  `Monate Erste Spende` ~ Dataset,
  data   = combined_data,
  notch  = TRUE,
  col    = c("red","blue"),
  border = "white",
  main   = "Monate Erste Spende",
  xlab   = "",
  ylab   = ""
)


# Panel 6 => Kommentarbox unten ----

par(mar = c(2, 2, 2, 2))
plot.new()
usr <- par("usr")

# Rotes Rechteck als Hintergrund:
rect(
  xleft   = usr[1],
  ybottom = usr[3],
  xright  = usr[2],
  ytop    = usr[4],
  col     = "red",
  border  = NA
)

# Weißer Text: "Erkenntnisse"
text(
  x     = 0.02, 
  y     = 0.80,
  labels= "Erkenntnisse",
  col   = "white", 
  cex   = 2.4, 
  font  = 2, 
  adj   = c(0, 0.5)
)

text(
  x     = 0.02, 
  y     = 0.55,
  labels= paste(
    " • Verteilungen der Variablen sind nicht unterschiedlich für Trainings- und Testdaten\n",
    "• Kerben der Boxen überlappen\n",
    "• Variabilität ist im Testdatensatz teilweise z. B. bei \"Anzahl Spenden\" höher."
  ),
  col   = "white", 
  cex   = 1.8, 
  adj   = c(0, 0.5)
)

# Weißer Text: "Schlussfolgerung"
text(
  x     = 0.02, 
  y     = 0.15,
  labels= "Schlussfolgerung\n",
  col   = "white", 
  cex   = 2.4, 
  font  = 2, 
  adj   = c(0, 0.5)
)

text(
  x     = 0.02, 
  y     = 0.05,
  labels= paste(
    "Man kann annehmen, dass Test- und Trainingsdaten Stichproben aus derselben Population sind.\n",
    ""
  ),
  col   = "white", 
  cex   = 1.8,
  adj   = c(0, 0.5)
)




#################################################################################
# Folie 5: Histogramme der Zielvariable ----


## R-Skript: "Folie": Vergleich der Verteilungen (Train vs. Test)
##            + Ein großer Titel oben (violette Leiste)
##            + 4 Histogramme nebeneinander
##            + Balkendiagramm (Klassenverteilung) unten links
##            + Kommentarkasten unten rechts


## 1) BENÖTIGTE PAKETE ----
# install.packages(c("tidyverse","ggplot2","dplyr","gridExtra"))
library(tidyverse)
library(gridExtra)  # für grid.arrange, tableGrob etc.
library(grid)       # für grid.rect, grid.text


## 3) EINZELNE PLOTS DEFINIEREN ----
## 3.1) Histogramm 1: Monate Letzte Spende
p1 <- ggplot(combined_data, aes(x=`Monate Letzte Spende`, fill=Dataset)) +
  geom_histogram(binwidth=1, position="dodge", color="black") +
  scale_fill_manual(values=c("Train"="blue","Test"="green")) +
  labs(title="Monate Letzte Spende", x="Monate", y="Frequenz") +
  theme_minimal()

## 3.2) Histogramm 2: Anzahl Spenden
p2 <- ggplot(combined_data, aes(x=`Anzahl Spenden`, fill=Dataset)) +
  geom_histogram(binwidth=1, position="dodge", color="black") +
  scale_fill_manual(values=c("Train"="blue","Test"="green")) +
  labs(title="Anzahl Spenden", x="Anzahl", y="Frequenz") +
  theme_minimal()

## 3.3) Histogramm 3: Gesamtvolumen
p3 <- ggplot(combined_data, aes(x=Gesamtvolumen, fill=Dataset)) +
  geom_histogram(binwidth=500, position="dodge", color="black") +
  scale_fill_manual(values=c("Train"="blue","Test"="green")) +
  labs(title="Gesamtvolumen", x="Volumen", y="Frequenz") +
  theme_minimal()

## 3.4) Histogramm 4: Monate Erste Spende
p4 <- ggplot(combined_data, aes(x=`Monate Erste Spende`, fill=Dataset)) +
  geom_histogram(binwidth=5, position="dodge", color="black") +
  scale_fill_manual(values=c("Train"="blue","Test"="green")) +
  labs(title="Monate Erste Spende", x="Monate", y="Frequenz") +
  theme_minimal()

## 3.5) Balkendiagramm der Zielvariable (Train)
p5 <- ggplot(train_data, aes(x=factor(`Spende Maerz 2007`), fill="Train")) +
  geom_bar(color="black") +
  scale_fill_manual(values=c("Train"="blue")) +
  scale_x_discrete(labels=c("0"="nein","1"="ja")) +
  labs(
    title="Klassenverteilung (Spende im März 2007, Trainingsdaten)",
    x="Klasse", y="Anzahl"
  ) +
  theme_minimal() +
  theme(legend.position="none")


## 4) "TITEL"-GROB ERSTELLEN ----


title_grob <- textGrob(
  "Vergleich Verteilungen Trainings- und Testdaten",
  gp = gpar(fontsize=32, col="grey", fontface="bold")  # große weiße Schrift
)

title_bg <- rectGrob(
  gp=gpar(fill="#7F3FBF", col=NA)  # violette Rechteckfläche
)

# Wir kombinieren das Rechteck + Titeltext mit "grobTree"
title_panel <- grobTree(title_bg, 
                        textGrob("",
                                 x=unit(0,"npc"), y=unit(0,"npc")), 
                        title_grob)


## 5) "KOMMENTAR"-GROB ERSTELLEN (Erkenntnisse-Box) ----


comment_bg <- rectGrob(
  gp=gpar(fill=rgb(1,1,1,0.15), col="black")                                    # Rahmenfarbe Box
  # z.B. halb-durchsichtiges Weiß oder so; anpassbar
)

comment_text <- textGrob(
  "Erkenntnisse\n
• Monate Letzte Spende: Verteilungen weitgehend ähnlich \n
• Anzahl Spenden: Im Testdatensatz weniger hohe Spenderzahlen\n
• Gesamtvolumen: tendenziell kleinere Werte im Test\n
• Monate Erste Spende: Testdatensatz <-> potenziell andere Spenderhistorie
\n
Schlussfolgerung\n
- Möglicherweise leichte Stichprobendifferenz \n
- Modell sollte train/test-Shift abfedern können
",
  x=0.02, y=0.98, just=c("left","top"),
  gp=gpar(col="black", fontsize=14, fontface="plain")                           # Schriftfarbe Box
)

comment_panel <- grobTree(comment_bg, comment_text)


## 6) DIE "FERTIGE FOLIE" MIT grid.arrange ----


# Wir bauen 3 Zeilen:
# Zeile A: Großer Titel
# Zeile B: 4 Plots nebeneinander
# Zeile C: Links Balkendiagramm p5, rechts der Kommentar

###############################################################################
## KAPITEL 1: PAKETE & SETUP
###############################################################################

library(tidyverse)
library(gridExtra)  # für grid.arrange
library(grid)       # für rectGrob, textGrob etc.

# Wir gehen davon aus, dass du train_data und test_data eingelesen hast
# und combined_data (train+test) verfügbar ist.

###############################################################################
## KAPITEL 2: HISTOGRAMME DEFINIEREN
##    - p1: Mit Y-Achsenbeschriftung "Frequenz" und Legende (links)
##    - p2, p3, p4: Ohne Y-Achsenbeschriftung und ohne Legende
###############################################################################

# p1: Histogramm "Monate Letzte Spende"
#    - Rot = Test, Blau = Train
#    - Legende links (legend.position="left")
#    - y="Frequenz"
p1 <- ggplot(combined_data, aes(x=`Monate Letzte Spende`, fill=Dataset)) +
  geom_histogram(binwidth=1, position="dodge", color="black") +
  scale_fill_manual(values=c("Test"="red","Train"="blue")) +
  labs(
    title="Monate Letzte Spende",
    x="Monate",
    y="Frequenz"
  ) +
  theme_minimal() +
  theme(legend.position="left")

# p2: Histogramm "Anzahl Spenden"
#    - KEINE Legende, KEINE Y-Achsenbeschriftung
p2 <- ggplot(combined_data, aes(x=`Anzahl Spenden`, fill=Dataset)) +
  geom_histogram(binwidth=1, position="dodge", color="black") +
  scale_fill_manual(values=c("Test"="red","Train"="blue")) +
  labs(
    title="Anzahl Spenden",
    x="Anzahl",
    y=NULL
  ) +
  theme_minimal() +
  theme(legend.position="none")

# p3: Histogramm "Gesamtvolumen"
#    - KEINE Legende, KEINE Y-Achsenbeschriftung
p3 <- ggplot(combined_data, aes(x=Gesamtvolumen, fill=Dataset)) +
  geom_histogram(binwidth=500, position="dodge", color="black") +
  scale_fill_manual(values=c("Test"="red","Train"="blue")) +
  labs(
    title="Gesamtvolumen",
    x="Volumen",
    y=NULL
  ) +
  theme_minimal() +
  theme(legend.position="none")

# p4: Histogramm "Monate Erste Spende"
#    - KEINE Legende, KEINE Y-Achsenbeschriftung
p4 <- ggplot(combined_data, aes(x=`Monate Erste Spende`, fill=Dataset)) +
  geom_histogram(binwidth=5, position="dodge", color="black") +
  scale_fill_manual(values=c("Test"="red","Train"="blue")) +
  labs(
    title="Monate Erste Spende",
    x="Monate",
    y=NULL
  ) +
  theme_minimal() +
  theme(legend.position="none")

###############################################################################
## KAPITEL 3: BALKENDIAGRAMM p5 (unten links)
###############################################################################

# p5: Balkendiagramm für "Spende Maerz 2007" im Trainingsdatensatz
#    - Nur blau (Train)
#    - Keine Legende
p5 <- ggplot(train_data, aes(x=factor(`Spende Maerz 2007`), fill="Train")) +
  geom_bar(color="black") +
  scale_fill_manual(values=c("Train"="blue")) +
  scale_x_discrete(labels=c("0"="nein","1"="ja")) +
  labs(
    title="Klassenverteilung (Spende im März 2007, Trainingsdaten)",
    x="Klasse",
    y="Anzahl"
  ) +
  theme_minimal() +
  theme(legend.position="none")

###############################################################################
## KAPITEL 4: KOMMENTARBOX RECHTS (Erkenntnisse und Schlussfolgerung)
###############################################################################

# "comment_bg": halb-transparentes weißes Rechteck
comment_bg <- rectGrob(
  gp=gpar(fill=rgb(1,1,1,0.15), col="white")
)

# "comment_text": grüner Text mit Bullet-Points
comment_text <- textGrob(
  "Erkenntnisse\n• Monate Letzte Spende: Verteilungen weitgehend ähnlich\n• Anzahl Spenden: Im Testdatensatz weniger hohe Spenderzahlen\n• Gesamtvolumen: tendenziell kleinere Werte im Test\n• Monate Erste Spende: evtl. andere Historie\n\nSchlussfolgerung\n- Möglicherweise leichte Stichprobendifferenz\n- Modell sollte train/test-Shift abfedern können",
  x=0.02, y=0.98, just=c("left","top"),
  gp=gpar(col="black", fontsize=14)
)

comment_panel <- grobTree(comment_bg, comment_text)

###############################################################################
## KAPITEL 5: TITEL-PANEL OBEN
###############################################################################

title_grob <- textGrob(
  "Vergleich Verteilungen Trainings- und Testdaten",
  gp = gpar(fontsize=32, col="white", fontface="bold")
)
title_bg   <- rectGrob(gp=gpar(fill="#7F3FBF", col=NA))
title_panel <- grobTree(title_bg, title_grob)

###############################################################################
## KAPITEL 6: ZUSAMMENSETZEN DES GESAMT-PLOTS MIT grid.arrange
###############################################################################

grid.arrange(
  # Reihenfolge der Panels:
  #  1) = title_panel
  #  2) = p1 (mit Legende & y=Frequenz)
  #  3) = p2
  #  4) = p3
  #  5) = p4
  #  6) = p5 (Balkendiagramm)
  #  7) = comment_panel (Box)
  
  title_panel,
  p1,
  p2,
  p3,
  p4,
  p5,
  comment_panel,
  
  # Layout: 3 Zeilen, 4 Spalten
  layout_matrix = rbind(
    c(1,1,1,1),   # Zeile 1 => Titel
    c(2,3,4,5),   # Zeile 2 => 4 Histogramme
    c(6,6,7,7)    # Zeile 3 => p5 links, Kommentar rechts
  ),
  nrow=3,
  heights=c(0.8, 3, 1.5)
)





#################################################################################
# Folie 6: Pairsplot + Korrelationsmatrix ----
##############################################################################
## GESAMTE FOLIE "Trainingsdaten – Beziehung der Variablen" (Screenshot 2)
##
##  Layout: 3 Zeilen × 2 Spalten
##    Zeile 1 (Panel1,1 => breiter Titel-Balken)
##    Zeile 2 => Pairs-Plot (links) & Korrelationsmatrix (rechts)
##    Zeile 3 => Box "Erkenntnisse..." (links) & Box "Neuer Prädiktor" (rechts)
##############################################################################

#### 1) Pakete + Daten einlesen ####
# Falls noch nicht installiert: install.packages("corrplot")
library(corrplot)

train_data <- read.csv("bloodtrain.csv", stringsAsFactors = FALSE)

colnames(train_data) <- c(
  "ID",
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende",
  "Spende Maerz 2007"
)

# 0 in "Monate Letzte Spende" => NA
z_rows <- train_data$`Monate Letzte Spende` == 0
if(any(z_rows, na.rm=TRUE)){
  train_data$`Monate Letzte Spende`[z_rows] <- NA
}

# Neuer Prädiktor "Spendetakt"
train_data$Spendetakt <- with(train_data, `Anzahl Spenden` / `Monate Letzte Spende`)

# Farbgebung: 1 => grün, 0 => rot
point_colors <- ifelse(train_data$`Spende Maerz 2007` == 1, "green", "red")

# Korrelationsmatrix (nur numerische Variablen, ID entfernen)
num_data <- train_data[sapply(train_data, is.numeric)]
if("ID" %in% names(num_data)){
  num_data <- num_data[, !names(num_data) %in% "ID"]
}
cor_matrix <- cor(num_data, use="pairwise.complete.obs")

##############################################################################
#### 2) layout(...) für das gesamte "Folie"-Layout
##############################################################################
layout(
  matrix(c(
    1,1,   # Zeile 1 => Panel1 breit (Titel)
    2,3,   # Zeile 2 => Panel2 (links), Panel3 (rechts)
    4,5    # Zeile 3 => Panel4 (links), Panel5 (rechts)
  ), nrow=3, byrow=TRUE),
  widths  = c(1,1),
  heights = c(1.2, 4, 2.5)
)

#### Globale par-Einstellungen für ALLE Panels ####
par(
  bg       = "#7F3FBF",  # Violetter "Folie"-Hintergrund
  fg       = "black",
  col.lab  = "black",
  col.axis = "black"
)


##############################################################################
#### PANEL 1: Titelbalken (Zeile1, Spalten1+2)
##############################################################################
par(mar=c(0,0,0,0))  # minimaler Rand
plot.new()
usr <- par("usr")

# Weißer Rahmen um den Balken:
rect(
  usr[1], usr[3], usr[2], usr[4],
  col=NA, border="white", lwd=3
)

text(
  x=0.5, y=0.5,
  labels="Trainingsdaten – Beziehung der Variablen",
  col="white", font=2, cex=2.5
)


##############################################################################
#### PANEL 2: Pairs-Plot (Zeile2, Spalte1)
##############################################################################
par(mar=c(4,4,3,1))
pairs(
  train_data[, c("Monate Letzte Spende","Anzahl Spenden","Gesamtvolumen","Monate Erste Spende","Spendetakt")],
  col        = ifelse(train_data$`Spende Maerz 2007`==1, "green","red"),
  main       = "Pairs-Plot Trainingsdaten",
  font.main  = 2,
  cex.main   = 1.4,
  height     = 1.5,
  width      = 1.5,
  
)


##############################################################################
#### PANEL 3: Korrelation (Zeile2, Spalte2)
##############################################################################
par(mar=c(4,2,3,2))
corrplot(
  cor_matrix,
  method      = "color",
  type        = "upper",
  tl.col      = "black",
  tl.srt      = 45,
  addCoef.col = "black",
  number.cex  = 0.9,
  title       = "Korrelationsmatrix (mit Spendetakt)",
  mar         = c(0,0,2,0),
  cex.main    = 1.4,
  font.main   = 2
)

##############################################################################
#### PANEL 4: Box "Erkenntnisse & Schlussfolgerung" (Zeile3, Spalte1)
##############################################################################
par(mar=c(0,0,0,0))
plot.new()
usr <- par("usr")

# halbtransparentes weißes Rechteck
rect(
  usr[1], usr[3], usr[2], usr[4],
  col=rgb(1,1,1,0.15), border="white", lwd=2
)

# Titel "Erkenntnisse"
text(
  x=0.02, y=0.85, adj=c(0,1),
  labels="Erkenntnisse",
  col="white", font=2, cex=1.6
)

txt_erk <- paste(
  "• 'Anzahl Spenden' und 'Gesamtvolumen' haben Korrelation 1",
  "• Korrelation neuer Prädiktor 'Spendetakt' hoch zu 'Anzahl Spenden' & 'Gesamtvolumen'",
  sep="\n"
)
text(
  x=0.02, y=0.7, adj=c(0,1),
  labels=txt_erk,
  col="white", cex=1.2
)

# Titel "Schlussfolgerung"
text(
  x=0.02, y=0.3, adj=c(0,1),
  labels="Schlussfolgerung",
  col="white", font=2, cex=1.6
)

text(
  x=0.02, y=0.15, adj=c(0,1),
  labels="• 'Anzahl Spenden' beibehalten und 'Gesamtvolumen' weglassen",
  col="white", cex=1.2
)

##############################################################################
#### PANEL 5: Box "Neuer Prädiktor" (Zeile3, Spalte2)
##############################################################################
par(mar=c(0,0,0,0))
plot.new()
usr <- par("usr")

rect(
  usr[1], usr[3], usr[2], usr[4],
  col=rgb(1,1,1,0.15), border="white", lwd=2
)

text(
  x=0.02, y=0.85, adj=c(0,1),
  labels="Neuer Prädiktor 'Spendetakt'",
  col="white", font=2, cex=1.4
)

text(
  x=0.02, y=0.65, adj=c(0,1),
  labels="= 'Anzahl Spenden' / 'Monate Letzte Spende'",
  col="white", cex=1.2
)

# FERTIG
##############################################################################






