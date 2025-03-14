# Variante A) mit list.files() und file.remove() (empfohlen):
files <- list.files("C:/Users/matth/OneDrive/AB1_R Projekte aktuell/Fallstudie/Präsentation", full.names = TRUE)
file.remove(files)

# Variante B) via system(...) und Windows-CMD (mit *.* um auch .txt Dateien zu löschen):
system('cmd /c "del /F /Q \"C:/Users/matth/OneDrive/AB1_R Projekte aktuell/Fallstudie/Präsentation\\*.*\""')

##############################################################################
# SCRIPT FÜR PRÄSENTATION: Erzeugt Folien (*.png) im Ordner "Präsentation"
##############################################################################
# Beschreibung:
#  - Wandelt PDF-Dateien in PNG-Folien um und speichert alle Folien im
#    Unterordner "Präsentation", der sich direkt im R-Projekt befindet.
#  - Erzeugt CSV-Tabellen- und Boxplot-Grafiken (PNG) sowie ein Histogramm-Grid.
#  - Erstellt ein HTML-Dokument mit 10 Folien, in dem alle Grafiken eingebettet werden.
#
# Hinweis:
#  - Vor Ausführung wird überprüft, ob der Ordner "Präsentation" existiert.
#    Falls nicht, wird er automatisch erstellt.
##############################################################################

# --- Vorbereitung: Ordner "Präsentation" erstellen -------------------------
library(here)
presentation_dir <- here::here("Präsentation")
if (!dir.exists(presentation_dir)) {
  dir.create(presentation_dir)
}

##############################################################################
# KAPITEL 0) PDF -> PNG (Folie 1 und Folie 2 erzeugen)
##############################################################################
# Beschreibung:
#  - Wandelt "Titelseite.pdf" (Seite 1) in "Folie1.png" und
#    "Workflow.pdf" (Seite 1) in "Folie2.png" um.
# Benötigtes Paket: pdftools (ggf. install.packages("pdftools"))

# --- 0.0) Pakete laden ------------------------------------------------------
library(pdftools)

# --- 0.1) Folie 1 (Titelseite) ---------------------------------------------
pdf_convert(
  pdf       = here::here("Titelseite.pdf"),                                # Quelle PDF-Datei
  pages     = 1,                                                           # Verwende Seite 1
  filenames = file.path(presentation_dir, "1) Titelbild.png"),                     # Ziel-Datei im Ordner "Präsentation"
  dpi       = 300
)

# --- 0.2) Folie 2 (Workflow) -----------------------------------------------
pdf_convert(
  pdf       = here::here("Workflow.pdf"),                                  # Quelle PDF-Datei
  pages     = 1,                                                           # Verwende Seite 1
  filenames = file.path(presentation_dir, "2) Workflow Diagramm.png"),                     # Ziel-Datei im Ordner "Präsentation"
  dpi       = 300
)

##############################################################################
# KAPITEL A) Daten laden & Spalten umbenennen
##############################################################################
# Beschreibung:
#  - Lädt Trainings- und Testdaten aus CSV-Dateien und passt die Spaltennamen an.

# --- A.0) Pakete laden ------------------------------------------------------
# (here Paket ist bereits geladen)

# --- A.1) CSV-Daten einlesen -----------------------------------------------
train_data <- read.csv(here::here("bloodtrain.csv"))
test_data  <- read.csv(here::here("bloodtest.csv"))

# --- A.2) Spaltennamen anpassen (Train) -------------------------------------
colnames(train_data) <- c(
  "ID",
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende",
  "Spende Maerz 2007"
)

# --- A.3) Spaltennamen anpassen (Test) --------------------------------------
colnames(test_data) <- c(
  "ID",
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende"
)

##############################################################################
# KAPITEL B) Summary-Text einfangen (Folie 4)
##############################################################################
# Beschreibung:
#  - Speichert die Konsolenausgabe von summary() der Trainings- und Testdaten
#    in den Objekten train_sum und test_sum.
# --- B.1) Summary-Ausgabe abfangen -----------------------------------------
train_sum <- capture.output(summary(train_data))
test_sum  <- capture.output(summary(test_data))

# --- B.2) Konsolenausgaben (optional, zum Debuggen) ------------------------
print("✅ Summary-Statistiken erfolgreich eingefangen!")
print("Trainingsdaten Zusammenfassung:")
print(train_sum)
print("Testdaten Zusammenfassung:")
print(test_sum)

# --- B.3) PNG-Folie "Statistische Kennzahlen" erstellen (A4 Quer) -----------
# Wir erstellen einen strukturierten Frame mit Überschriften, zusammengefassten 
# Datenblöcken und einem äußeren Seitenrahmen.

library(grid)

# Pfad zur PNG-Datei (A4 Quer: 297mm x 210mm)
png_filename <- file.path(presentation_dir, "4) Summary_Statistische_Kennzahlen.png")

# Öffne PNG-Gerät mit den spezifizierten Abmaßen und Auflösung
png(filename = png_filename, width = 297, height = 210, units = "mm", res = 300)

# Starte eine neue Seite und ziehe einen äußeren Rahmen, der die gesamte Seite umgibt.
grid.newpage()
grid.rect(gp = gpar(lwd = 3, col = "black"))  # äußerer Seitenrahmen

# Definiere das Layout mittels grid.layout: 5 Zeilen für unterschiedliche Bereiche
layout_heights <- unit(c(0.15, 0.1, 0.325, 0.1, 0.325), "npc")
pushViewport(viewport(layout = grid.layout(nrow = 5, ncol = 1, heights = layout_heights)))

# =============================================================================
# Zeile 1: Titel
# =============================================================================
# Zeichne den Hintergrund für die gesamte A4-Seite (Hellgrün)
grid.rect(x = 0.5, y = 0.5, width = 1, height = 1,
      gp = gpar(fill = "#ccffcc", col = NA))

# =============================================================================
# Zeile 1: Titel mit einem dunkleren Hintergrund (präsentationswirksam)
# =============================================================================
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1, width = 0.9))
grid.rect(gp = gpar(fill = "#669966", col = "black", lwd = 2))
grid.text("Statistische Kennzahlen", 
      gp = gpar(fontsize = 24, fontface = "bold", col = "white"))
popViewport()

# =============================================================================
# Zeile 2: Untertitel für Trainingsdaten
# =============================================================================
grid.text("Trainingsdaten Zusammenfassung", 
      vp = viewport(layout.pos.row = 2, layout.pos.col = 1),
      gp = gpar(fontsize = 16, fontface = "bold"))

# =============================================================================
# Zeile 3: Trainingsdaten Summary-Block mit rotem Hintergrund und schmalerer Breite
# =============================================================================
train_text <- paste(train_sum, collapse = "\n")
pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1, width = 0.9))
grid.rect(gp = gpar(fill = "#FFCCCC", col = "black", lwd = 2))
grid.text(train_text, 
      x = 0.5, y = 0.98, just = c("center", "top"),
      gp = gpar(fontfamily = "Courier", fontsize = 10))
popViewport()

# =============================================================================
# Zeile 4: Untertitel für Testdaten
# =============================================================================
grid.text("Testdaten Zusammenfassung", 
      vp = viewport(layout.pos.row = 4, layout.pos.col = 1),
      gp = gpar(fontsize = 16, fontface = "bold"))

# =============================================================================
# Zeile 5: Testdaten Summary-Block mit blauem Hintergrund und schmalerer Breite
# =============================================================================
test_text <- paste(test_sum, collapse = "\n")
pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 1, width = 0.9))
grid.rect(gp = gpar(fill = "#CCCCFF", col = "black", lwd = 2))
grid.text(test_text, 
      x = 0.5, y = 0.98, just = c("center", "top"),
      gp = gpar(fontfamily = "Courier", fontsize = 10))
popViewport()

# Beende den Layout-Viewport
popViewport()

# Schließe das Grafikgerät und speichere die PNG-Datei.
dev.off()

print("✅ PNG-Folie 'Statistische Kennzahlen' erfolgreich erstellt!")





##############################################################################
# KAPITEL C) Boxplot erzeugen & als PNG speichern (Folie 6)
##############################################################################
# Beschreibung:
#  - Erstellt einen Boxplot (ggplot2) für z-standardisierte numerische Variablen.
#  - Der Boxplot wird als PNG (Folie 6) im Ordner "Präsentation" gespeichert.

# --- C.0) Pakete laden ------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# --- C.1) Kennzeichnung der Datensätze (Train vs. Test) ---------------------
train_data$Dataset <- "Train"
test_data$Dataset  <- "Test"

# --- C.2) Zusammenführen relevanter Spalten in gemeinsamen df --------------
df <- bind_rows(
  select(train_data, Dataset,
         `Anzahl Spenden`, Gesamtvolumen,
         `Monate Erste Spende`, `Monate Letzte Spende`),
  select(test_data, Dataset,
         `Anzahl Spenden`, Gesamtvolumen,
         `Monate Erste Spende`, `Monate Letzte Spende`)
)

# --- C.3) Skalieren (z-Standardisierung) ------------------------------------
df_scaled <- df %>%
  mutate(across(!Dataset, scale))

# --- C.4) Long-Format vorbereiten (für ggplot) ------------------------------
df_long <- pivot_longer(
  df_scaled,
  cols      = c(
    "Anzahl Spenden",
    "Gesamtvolumen",
    "Monate Erste Spende",
    "Monate Letzte Spende"
  ),
  names_to  = "Variable",
  values_to = "Wert"
)

# --- C.5) Boxplot erstellen -------------------------------------------------
p <- ggplot(df_long, aes(x = Variable, y = Wert, fill = Dataset)) +
  geom_boxplot(
    notch         = TRUE,    # Kerben anzeigen
    notchwidth    = 0.3,
    outlier.shape = 21,      
    alpha         = 0.7,
    color         = "black",
    size          = 1
  ) +
  # Reduziert zusätzlichen Leerraum an den Achsen
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Skalierte numerische Variablen",
    x     = NULL,
    y     = "Skalierter Wert"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background    = element_rect(fill = "#ffe6f7", color = NA),
    plot.background     = element_rect(fill = "#ffe6f7", color = NA),
    legend.background   = element_rect(fill = "#ffe6f7", color = NA),
    legend.key          = element_rect(fill = "#ffe6f7", color = NA),
    
    # Überschriftseinstellungen
    plot.title          = element_text(
      color = "black", size = 18, face = "bold", hjust = 0.5 
    ),
    legend.position     = "right",
    legend.text         = element_text(color = "black"),
    legend.title        = element_text(color = "black", face = "bold"),
    axis.text           = element_text(color = "black"),
    axis.title          = element_text(color = "black"),
    plot.margin         = margin(5, 20, 5, 20)
  )

# --- C.6) Boxplot als PNG speichern (Folie 6) -------------------------------
folie6_path <- file.path(presentation_dir, "6) Boxplot Vergleich_test_train.png")
ggsave(
  filename = folie6_path,
  plot     = p,
  width    = 10,
  height   = 5,
  dpi      = 1200,
  bg       = "#ffe6f7"
)

##############################################################################
# KAPITEL D) Vergleich der Verteilungen (Train vs. Test) (Folie 7)
##############################################################################
# Beschreibung:
#  - Erzeugt vier Histogramme für verschiedene Variablen und ordnet diese in einem Grid an.
#  - Speichert das Grid als PNG-Datei (Folie7_Vergleich_Train_Test.png) im Ordner "Präsentation".

# --- D.0) Pakete laden ------------------------------------------------------
library(gridExtra)  # Für arrangeGrob() bzw. grid.arrange()

# --- D.1) Histogramme für die Verteilungen erstellen ------------------------
plot1 <- ggplot(df, aes(x = `Monate Letzte Spende`, fill = Dataset)) +
  geom_histogram(position = "dodge", bins = 20, alpha = 0.7, color = "black") +
  labs(title = "Monate seit letzter Spende",
       x = "Monate", y = "Häufigkeit") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_minimal(base_size = 16)

plot2 <- ggplot(df, aes(x = `Anzahl Spenden`, fill = Dataset)) +
  geom_histogram(position = "dodge", bins = 15, alpha = 0.7, color = "black") +
  labs(title = "Anzahl Spenden",
       x = "Anzahl", y = "Häufigkeit") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_minimal(base_size = 16)

plot3 <- ggplot(df, aes(x = Gesamtvolumen, fill = Dataset)) +
  geom_histogram(position = "dodge", bins = 20, alpha = 0.7, color = "black") +
  labs(title = "Gesamtvolumen Spenden",
       x = "Volumen", y = "Häufigkeit") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_minimal(base_size = 16)

plot4 <- ggplot(df, aes(x = `Monate Erste Spende`, fill = Dataset)) +
  geom_histogram(position = "dodge", bins = 20, alpha = 0.7, color = "black") +
  labs(title = "Monate seit erster Spende",
       x = "Monate", y = "Häufigkeit") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_minimal(base_size = 16)

# --- D.2) Histogramme in einem Grid anordnen --------------------------------
combined_plot <- arrangeGrob(plot1, plot2, plot3, plot4,
                             ncol = 2,
                             top  = "Vergleich der Verteilungen: Trainings- vs. Testdaten")

# --- D.3) Grid als PNG-Datei speichern --------------------------------------
folie7_path <- file.path(presentation_dir, "7) Histogramm Vergleich_train_test.png")
ggsave(
  filename = folie7_path,
  plot     = combined_plot,
  width    = 10,
  height   = 5,
  dpi      = 300
)

print("✅ Folie 7 erfolgreich erstellt (Histogramm-Grid als PNG gespeichert!)")

##############################################################################
# KAPITEL E) HTML-Seite mit 10 Folien/Slides erzeugen
##############################################################################
# Beschreibung:
#  - Baut ein HTML-Dokument via htmltools, in dem jede Folie als DIV mit speziellem
#    Hintergrund dargestellt wird.
#  - In Folie 4 werden die Summary-Statistiken von Trainings- und Testdaten angezeigt.
#  - In Folie 6 und Folie 7 werden die zuvor gespeicherten PNG-Bilder eingebettet.
##############################################################################

# --- E.0) Pakete laden ------------------------------------------------------
library(htmltools)

# --- E.1) HTML-Struktur definieren ------------------------------------------
# Hier werden die Pfade zu den im Ordner "Präsentation" gespeicherten Bildern genutzt.
page <- tags$html(
  tags$head(
    tags$title("Foliensatz: Summary & Boxplot"),
    tags$style("
      /* @page: Format A4 Querformat mit 1.0cm Rand */
      @page {
        size: A4 landscape;
        margin: 1.0cm;
      }
      body {
        font-family: Arial, sans-serif;
        margin: 0;
        padding: 0;
      }
      h1 {
        background-color: #007ACC;
        color: white;
        padding: 10px;
        margin: 0;
      }
      .slide {
        padding: 10px 20px;
        margin-bottom: 10px;
        page-break-after: always;
      }
      .slide1  { background-color: #fafafa; }
      .slide2  { background-color: #e6f0ff; }
      .slide3  { background-color: #f9f0e6; }
      .slide4  { background-color: #f2e6ff; }
      .slide5  { background-color: #e6fff5; }
      .slide6  { background-color: #ffe6f7; }
      .slide7  { background-color: #fffbe6; }
      .slide8  { background-color: #e6ffe6; }
      .slide9  { background-color: #ffe6e6; }
      .slide10 { background-color: #f0f0f0; }

      h2 {
        font-size: 20px;
      }
      .summary-block {
        background-color: #f8f9fa;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 30px;
      }
      pre {
        font-family: 'Courier New', Courier, monospace;
        font-size: 14px;
        white-space: pre-wrap;
      }
      .subtitle {
        color: white;
        font-size: 18px;
        margin-top: 0;
        margin-bottom: 20px;
      }
      .download-link {
        display: inline-block;
        margin-top: 15px;
        padding: 8px 12px;
        background-color: #007BFF;
        color: #FFFFFF;
        text-decoration: none;
        border-radius: 5px;
        font-weight: bold;
      }
      .download-link:hover {
        background-color: #0056b3;
      }
      .violett-box {
        margin-top: 20px;
        background-color: #5C2E66;
        color: white;
        padding: 15px;
        border-radius: 5px;
      }
    ")
  ),
  tags$body(
    
    # Folie 1: Titelbild (PDF -> PNG)
    tags$div(class = "slide slide1",
             tags$h1("Folie 1: Titelbild"),
             tags$img(
               src = "Präsentation/Folie1.png",
               style = "max-width: 100%; height: auto;"
             ),
             tags$a(
               href     = here::here("Titelseite.pdf"),
               download = NA,
               class    = "download-link",
               "Titelseite (PDF Download)"
             )
    ),
    
    # Folie 2: Workflow Diagramm (PDF -> PNG)
    tags$div(class = "slide slide2",
             tags$h1("Folie 2: Workflow Diagramm"),
             tags$img(
               src = "Präsentation/Folie2.png",
               style = "max-width: 100%; height: auto;"
             ),
             tags$a(
               href     = here::here("Workflow.pdf"),
               download = NA,
               class    = "download-link",
               "Workflow (PDF Download)"
             )
    ),
    
    # Folie 3: Platzhalter für weiteren Inhalt
    tags$div(class = "slide slide3",
             tags$h1("Folie 3: ..."),
             tags$p("Hier könnte ein weiterer Inhalt stehen.")
    ),
    
    # Folie 4: Statistische Kennzahlen (Summary der Datensätze)
    tags$div(class = "slide slide4",
             tags$h1("Folie 4: Statistische Kennzahlen"),
             tags$h2("Trainingsdaten Zusammenfassung"),
             tags$p("Mit 'summary' gewinnen wir einen statistischen Überblick."),
             tags$div(
               class = "summary-block",
               tags$pre(paste(train_sum, collapse = "\n"))
             ),
             tags$h2("Testdaten Zusammenfassung"),
             tags$p("Auch die Testdaten werden zusammengefasst."),
             tags$div(
               class = "summary-block",
               tags$pre(paste(test_sum, collapse = "\n"))
             )
    ),
    
    # Folie 5: Platzhalter für weiteren Inhalt
    tags$div(class = "slide slide5",
             tags$h1("Folie 5: ..."),
             tags$p("Noch ein Platzhalter für weiteren Inhalt.")
    ),
    
    # Folie 6: Boxplot (PNG)
    tags$div(class = "slide slide6",
             tags$h1("Folie 6: Boxplot der z-standardisierten Variablen"),
             tags$img(
               src   = "Präsentation/Folie6.png",
               style = "max-width: 100%; height: auto;"
             ),
             tags$div(
               class = "violett-box",
               tags$strong("Erkenntnisse:"),
               tags$ul(
                 tags$li("Die Boxplots zeigen die Verteilung der z-standardisierten numerischen Variablen."),
                 tags$li("Unterschiede zwischen Train und Test deuten auf ähnliche Verteilungen hin.")
               ),
               tags$br(),
               tags$strong("Schlussfolgerung:"),
               tags$p("Die Trainings- und Testdaten scheinen aus der gleichen Population zu stammen.")
             )
    ),
    
    # Folie 7: Vergleich der Verteilungen (Histogramm-Grid, PNG)
    tags$div(class = "slide slide7",
             tags$h1("Folie 7: Vergleich der Verteilungen Test vs. Train"),
             tags$img(
               src = "Präsentation/Folie7_Vergleich_Train_Test.png",
               style = "max-width: 100%; height: auto;"
             ),
             tags$div(
               class = "violett-box",
               tags$strong("Erkenntnisse:"),
               tags$ul(
                 tags$li("Trainings- und Testdaten haben ähnliche Verteilungen."),
                 tags$li("Leichte Unterschiede bei der Anzahl der Spenden im Testset."),
                 tags$li("Das Gesamtvolumen im Testset ist etwas niedriger.")
               ),
               tags$br(),
               tags$strong("Schlussfolgerung:"),
               tags$p("Die Unterschiede könnten die Modellleistung beeinflussen.")
             )
    ),
    
    # Folie 8: Platzhalter für weiteren Inhalt
    tags$div(class = "slide slide8",
             tags$h1("Folie 8: Weiterer Inhalt"),
             tags$p("Noch mehr Platzhalter...")
    ),
    
    # Folie 9: Platzhalter für weiteren Inhalt
    tags$div(class = "slide slide9",
             tags$h1("Folie 9: Weiterer Inhalt"),
             tags$p("Noch mehr Platzhalter...")
    ),
    
    # Folie 10: Platzhalter für weiteren Inhalt
    tags$div(class = "slide slide10",
             tags$h1("Folie 10: Weiterer Inhalt"),
             tags$p("Noch mehr Platzhalter...")
    )
  )
)






##############################################################################
# KAPITEL F) HTML-Seite anzeigen
##############################################################################
# Beschreibung:
#  - Mit html_print(page) wird die erzeugte HTML-Seite in RStudio bzw. im Browser angezeigt.
##############################################################################
html_print(page)
