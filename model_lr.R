##############################################################################
# SKRIPT FÜR PRÄSENTATION
# ----------------------------------------------------------------------------
# Dieses Skript führt folgende Schritte durch:
#   1) Alte PNG-Dateien im Ordner "Präsentation" löschen
#   2) Zwei PDF-Dateien ("Titelseite.pdf", "Workflow.pdf") -> PNG konvertieren
#   3) Trainings- und Testdaten laden, Spalten umbenennen
#   4) "Statistische Kennzahlen"-Folie (PNG) erzeugen
#   5) Boxplot-Folie (PNG) + Histogramm-Folie (PNG)
#   6) HTML-Dokument mit 10 Folien erstellen & darin alle PNG einbetten
#   7) Zusätzlicher Beispiel-Plot, der nur im RStudio-Viewer angezeigt wird
#
# Jeder Abschnitt ist mit KAPITEL-Kommentaren gekennzeichnet. 
# Zusätzlich werden die einzelnen Codezeilen kommentiert, 
# damit Sie genau verstehen, was passiert.
##############################################################################


##############################################################################
# KAPITEL 0) Aufräumen: alte PNGs löschen
##############################################################################
# Hier werden bereits existierende PNG-Dateien im Ordner "Präsentation" gelöscht.
# Damit ist sichergestellt, dass wir eine "frische" Ausgangslage für neue Folien haben.

# Dieser Pfad zeigt auf den Ordner, in dem die PNG-Folien liegen sollen:
presentation_path <- "C:/Users/matth/OneDrive/AB1_R Projekte aktuell/Fallstudie/Präsentation"

# 1) Liste aller PNG-Dateien ermitteln:
files <- list.files(
  presentation_path,    # Hier suchen wir
  pattern = "\\.png$",  # Nur Dateien mit ".png"
  full.names = TRUE     # Den kompletten Pfad zurückliefern
)

# 2) Datei-Löschversuch per R-Funktion (kann Fehlermeldungen geben, wenn gesperrt)
file.remove(files)

# 3) Zweiter Löschversuch via Windows-CMD (Option B)
#    Achtung: funktioniert nur unter Windows; 
#    auch hier kann es "Permission denied" geben, wenn eine Datei noch geöffnet ist.
system(
  paste0(
    'cmd /c "del /F /Q \"', 
    presentation_path, 
    '\\*.*\""'
  )
)


##############################################################################
# KAPITEL 1) Vorbereitung & Ordner "Präsentation" anlegen
##############################################################################
# In diesem Abschnitt laden wir Pakete und erzeugen den Ordner "Präsentation"
# (falls er noch nicht existiert).

# Pakete laden:

library(here)       # Für here::here() -> robustere Pfadangaben
library(pdftools)   # Zum Umwandeln von PDF -> PNG (pdf_convert)
library(dplyr)      # Data-Wrangling-Funktionen (bind_rows, select, mutate usw.)
library(tidyr)      # Reshaping von Daten (pivot_longer)
library(ggplot2)    # Für unsere Grafiken (Boxplot, Histogramm usw.)
library(grid)       # Low-Level-Grafikfunktionen (grid.rect, grid.text etc.)
library(gridExtra)  # Für arrangeGrob() -> mehrere Plots in einem Grid
library(htmltools)  # Für das Erstellen des HTML-Dokuments (html_print etc.)

# Pfad zum Unterordner "Präsentation" in unserem Projektverzeichnis:
presentation_dir <- here::here("Präsentation")

# Falls der Ordner nicht existiert, anlegen:
if (!dir.exists(presentation_dir)) {
  dir.create(presentation_dir)
}


##############################################################################
# KAPITEL 2) PDF -> PNG (Titelseite & Workflow)
##############################################################################
# Wir konvertieren 2 PDF-Dateien in PNGs. 
# Die PDFs heißen "Titelseite.pdf" und "Workflow.pdf".
# Ausgabe sind "1) Titelbild.png" und "2) Workflow Diagramm.png" im Ordner.

# 1) "Titelseite.pdf" -> "1) Titelbild.png"
pdf_convert(
  pdf       = here::here("Titelseite.pdf"),                  # Eingabe-PDF
  pages     = 1,                                             # Nur Seite 1
  filenames = file.path(presentation_dir, "1) Titelbild.png"),  # Ausgabedatei
  format    = "png",                                         # PNG-Format
  dpi       = 300                                            # 300 dpi
)

# 2) "Workflow.pdf" -> "2) Workflow Diagramm.png"
pdf_convert(
  pdf       = here::here("Workflow.pdf"),
  pages     = 1,
  filenames = file.path(presentation_dir, "2) Workflow Diagramm.png"),
  format    = "png",
  dpi       = 300
)


##############################################################################
# KAPITEL A) Daten laden & Spalten umbenennen
##############################################################################
# Wir lesen die Trainings- und Testdaten ein (bloodtrain.csv, bloodtest.csv)
# und passen die Spaltennamen an (von englisch auf deutsch, etc.).

# A.1) CSV-Dateien einlesen:
train_data <- read.csv(here::here("bloodtrain.csv"))
test_data  <- read.csv(here::here("bloodtest.csv"))

# A.2) Spaltennamen Trainingsdaten anpassen:
colnames(train_data) <- c(
  "ID",
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende",
  "Spende Maerz 2007"  # Zielvariable
)

# A.3) Spaltennamen Testdaten anpassen:
colnames(test_data) <- c(
  "ID",
  "Monate Letzte Spende",
  "Anzahl Spenden",
  "Gesamtvolumen",
  "Monate Erste Spende"
)


##############################################################################
# KAPITEL B) Summary-Text einfangen (Folie 4) und PNG erzeugen
##############################################################################
# Wir erzeugen eine "Statistische-Kennzahlen-Folie" im PNG-Format.
# Zuerst fangen wir mit capture.output() die Konsolenausgabe von summary() ein.
# Dann bauen wir via grid.* eine Art "Layout" auf und platzieren
# die Textinhalte. Das Ergebnis wird als PNG abgespeichert.

# B.1) Summary-Ausgaben einfangen:
train_sum <- capture.output(summary(train_data))  # Zusammenfassung Train
test_sum  <- capture.output(summary(test_data))   # Zusammenfassung Test

# B.2) Konsolen-Info (optional):
message("✅ Summary-Statistiken erfolgreich eingefangen!")
message("Trainingsdaten Zusammenfassung:")
print(train_sum)
message("Testdaten Zusammenfassung:")
print(test_sum)

# B.3) PNG-Folie "Statistische Kennzahlen" (A4 Querformat) erzeugen
png_filename <- file.path(presentation_dir, "4) Summary_Statistische_Kennzahlen.png")

png(
  filename = png_filename,    # Name der zu erstellenden PNG-Datei
  width    = 297,            # 297 mm (A4-Querbreite)
  height   = 210,            # 210 mm (A4-Höhe)
  units    = "mm",           # Einheit der obigen Maße
  res      = 300             # Auflösung
)

# ---- Neues Grafik-Device ist jetzt aktiv ----
grid.newpage()  # Neue Seite starten

# 1) Hellgrüner Seitenhintergrund
grid.rect(
  x = 0.5, y = 0.5, 
  width = 1, height = 1,
  gp = gpar(fill = "#ccffcc", col = NA)
)

# 2) Äußerer schwarzer Rahmen
grid.rect(
  gp = gpar(lwd = 3, col = "black", fill = NA)
)

# 3) Layout definieren: 5 Zeilen (z.B. für Titel, Zwischenüberschrift, Inhalt etc.)
layout_heights <- unit(c(0.15, 0.1, 0.325, 0.1, 0.325), "npc")
pushViewport(
  viewport(layout = grid.layout(nrow = 5, ncol = 1, heights = layout_heights))
)

# Zeile 1: Titelbalken
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.rect(gp = gpar(fill = "#669966", col = "black", lwd = 2))
grid.text(
  "Statistische Kennzahlen",
  gp = gpar(fontsize = 24, fontface = "bold", col = "white")
)
popViewport()

# Zeile 2: Untertitel "Trainingsdaten"
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
grid.text(
  "Trainingsdaten Zusammenfassung",
  gp = gpar(fontsize = 16, fontface = "bold")
)
popViewport()

# Zeile 3: Rotes Rechteck + Train-Summary
pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1, width = 0.9))
grid.rect(gp = gpar(fill = "#FFCCCC", col = "black", lwd = 2))
train_text <- paste(train_sum, collapse = "\n")
grid.text(
  train_text,
  x = 0.5, y = 0.98,
  just = c("center", "top"),
  gp = gpar(fontfamily = "Courier", fontsize = 10)
)
popViewport()

# Zeile 4: Untertitel "Testdaten"
pushViewport(viewport(layout.pos.row = 4, layout.pos.col = 1))
grid.text(
  "Testdaten Zusammenfassung",
  gp = gpar(fontsize = 16, fontface = "bold")
)
popViewport()

# Zeile 5: Blaues Rechteck + Test-Summary
pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 1, width = 0.9))
grid.rect(gp = gpar(fill = "#CCCCFF", col = "black", lwd = 2))
test_text <- paste(test_sum, collapse = "\n")
grid.text(
  test_text,
  x = 0.5, y = 0.98,
  just = c("center", "top"),
  gp = gpar(fontfamily = "Courier", fontsize = 10)
)
popViewport()

# Layout-Viewport schließen
popViewport()

# PNG-Gerät schließen -> Dateischreibung abgeschlossen
dev.off()

# Log-Nachricht in Konsole
message("✅ PNG-Folie 'Statistische Kennzahlen' erfolgreich erstellt!")


##############################################################################
# KAPITEL C) Boxplot erzeugen (Folie 6)
##############################################################################
# Wir erstellen einen z-standardisierten Boxplot mit ggplot2 (Train vs. Test).
# Er wird als PNG gespeichert.

# C.1) Kennzeichnung der Datensätze: "Train" vs. "Test"
train_data$Dataset <- "Train"
test_data$Dataset  <- "Test"

# C.2) Relevante Spalten aus train/test "binden":
df <- bind_rows(
  select(train_data,
         Dataset, 
         `Anzahl Spenden`,
         Gesamtvolumen,
         `Monate Erste Spende`,
         `Monate Letzte Spende`),
  select(test_data,
         Dataset,
         `Anzahl Spenden`,
         Gesamtvolumen,
         `Monate Erste Spende`,
         `Monate Letzte Spende`)
)

# C.3) z-Standardisierung (alle numerischen außer "Dataset")
df_scaled <- df %>%
  mutate(across(!Dataset, scale))

# C.4) in ein Long-Format "schmelzen" (Variable, Wert)
df_long <- pivot_longer(
  df_scaled,
  cols      = c("Anzahl Spenden", "Gesamtvolumen", "Monate Erste Spende", "Monate Letzte Spende"),
  names_to  = "Variable",
  values_to = "Wert"
)

# C.5) Boxplot (ggplot)
p_box <- ggplot(df_long, aes(x = Variable, y = Wert, fill = Dataset)) +
  geom_boxplot(
    notch         = TRUE,   # Kerbe im Boxplot
    notchwidth    = 0.3,
    outlier.shape = 21,     # Outlier-Kreise
    alpha         = 0.7,    # Transparenz
    color         = "black",
    size          = 1
  ) +
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
    plot.title          = element_text(size = 18, face = "bold", hjust = 0.5),
    legend.position     = "right",
    axis.text           = element_text(color = "black"),
    plot.margin         = margin(5, 20, 5, 20)
  )

# C.6) Speichern des Boxplots (Folie 6)
folie6_path <- file.path(presentation_dir, "6) Boxplot Vergleich_test_train.png")
ggsave(
  filename = folie6_path,
  plot     = p_box,
  width    = 10,
  height   = 5,
  dpi      = 1200,
  bg       = "#ffe6f7"
)


##############################################################################
# KAPITEL D) Histogramm-Vergleich Train vs. Test (Folie 7)
##############################################################################
# Hier erstellen wir vier Histogramme (ggplot) für 4 Variablen.
# Danach werden diese 4 Plots in einem Grid (2x2) kombiniert und als PNG gespeichert.

# D.1) Einzelplots definieren
plot1 <- ggplot(df, aes(x = `Monate Letzte Spende`, fill = Dataset)) +
  geom_histogram(position = "dodge", bins = 20, alpha = 0.7, color = "black") +
  labs(title = "Monate seit letzter Spende", x = "Monate", y = "Häufigkeit") +
  theme_minimal(base_size = 16)

plot2 <- ggplot(df, aes(x = `Anzahl Spenden`, fill = Dataset)) +
  geom_histogram(position = "dodge", bins = 15, alpha = 0.7, color = "black") +
  labs(title = "Anzahl Spenden", x = "Anzahl", y = "Häufigkeit") +
  theme_minimal(base_size = 16)

plot3 <- ggplot(df, aes(x = Gesamtvolumen, fill = Dataset)) +
  geom_histogram(position = "dodge", bins = 20, alpha = 0.7, color = "black") +
  labs(title = "Gesamtvolumen Spenden", x = "Volumen", y = "Häufigkeit") +
  theme_minimal(base_size = 16)

plot4 <- ggplot(df, aes(x = `Monate Erste Spende`, fill = Dataset)) +
  geom_histogram(position = "dodge", bins = 20, alpha = 0.7, color = "black") +
  labs(title = "Monate seit erster Spende", x = "Monate", y = "Häufigkeit") +
  theme_minimal(base_size = 16)

# D.2) Zu einem 2x2 Grid kombinieren
combined_plot <- arrangeGrob(
  plot1, plot2, plot3, plot4,
  ncol = 2,  # 2 Spalten, 2 Zeilen
  top  = "Vergleich der Verteilungen: Trainings- vs. Testdaten"  # Titel
)

# D.3) Speichern (Folie 7)
folie7_path <- file.path(presentation_dir, "7) Histogramm Vergleich_train_test.png")
ggsave(
  filename = folie7_path,
  plot     = combined_plot,
  width    = 10,
  height   = 5,
  dpi      = 300
)

message("✅ Folie 7 erfolgreich erstellt (Histogramm-Grid als PNG gespeichert!)")


##############################################################################
# KAPITEL E) HTML-Seite mit 10 Folien/Slides erzeugen
##############################################################################
# In diesem Schritt bauen wir ein HTML-Dokument (10 Folien).
# Jede Folie hat ein <div> mit einer bestimmten Hintergrundfarbe.
# Folie 4 zeigt z.B. das zusammengefasste Summary, Folie 6 den Boxplot, Folie 7 das Histogramm.

page <- tags$html(
  # HEAD-Bereich
  tags$head(
    tags$title("Foliensatz: Summary & Boxplot"),  # <title> im Browser
    # CSS-Styles inline
    tags$style("
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
        page-break-after: always; /* Jede Folie auf neue Seite (PDF-Druck) */
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
        margin-top: 10px;
        margin-bottom: 10px;
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
        white-space: pre-wrap; /* Zeilen nicht abschneiden, sondern umbrechen */
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
  
  # BODY-Bereich
  tags$body(
    # Folie 1: Titelbild
    tags$div(class = "slide slide1",
             tags$h1("Folie 1: Titelbild"),
             tags$img(
               src   = "Präsentation/1) Titelbild.png", 
               style = "max-width: 100%; height: auto;"
             ),
             tags$a(
               href     = here::here("Titelseite.pdf"),
               download = NA,
               class    = "download-link",
               "Titelseite (PDF Download)"
             )
    ),
    
    # Folie 2: Workflow
    tags$div(class = "slide slide2",
             tags$h1("Folie 2: Workflow Diagramm"),
             tags$img(
               src   = "Präsentation/2) Workflow Diagramm.png",
               style = "max-width: 100%; height: auto;"
             ),
             tags$a(
               href     = here::here("Workflow.pdf"),
               download = NA,
               class    = "download-link",
               "Workflow (PDF Download)"
             )
    ),
    
    # Folie 3
    tags$div(class = "slide slide3",
             tags$h1("Folie 3: ..."),
             tags$p("Hier könnte weiterer Inhalt stehen.")
    ),
    
    # Folie 4: Summary Stats
    tags$div(class = "slide slide4",
             tags$h1("Folie 4: Statistische Kennzahlen"),
             tags$h2("Trainingsdaten Zusammenfassung"),
             tags$p("Mit 'summary()' gewinnen wir einen ersten statistischen Überblick."),
             tags$div(
               class = "summary-block",
               tags$pre(paste(train_sum, collapse = "\n"))  # Der zusammengefasste Text
             ),
             tags$h2("Testdaten Zusammenfassung"),
             tags$p("Auch die Testdaten werden zusammengefasst."),
             tags$div(
               class = "summary-block",
               tags$pre(paste(test_sum, collapse = "\n"))   # Der zusammengefasste Text
             )
    ),
    
    # Folie 5
    tags$div(class = "slide slide5",
             tags$h1("Folie 5: ..."),
             tags$p("Noch ein Platzhalter für weiteren Inhalt.")
    ),
    
    # Folie 6: Boxplot
    tags$div(class = "slide slide6",
             tags$h1("Folie 6: Boxplot der z-standardisierten Variablen"),
             tags$img(
               src   = "Präsentation/6) Boxplot Vergleich_test_train.png",
               style = "max-width: 100%; height: auto;"
             ),
             tags$div(
               class = "violett-box",
               tags$strong("Erkenntnisse:"),
               tags$ul(
                 tags$li("Die Boxplots zeigen die Verteilung der z-standardisierten numerischen Variablen."),
                 tags$li("Unterschiede zwischen Train und Test sind eher gering, was auf ähnliche Datenquellen hindeutet.")
               ),
               tags$br(),
               tags$strong("Schlussfolgerung:"),
               tags$p("Die Trainings- und Testdaten stammen vermutlich aus einer ähnlichen Grundgesamtheit.")
             )
    ),
    
    # Folie 7: Histogramm-Grid
    tags$div(class = "slide slide7",
             tags$h1("Folie 7: Vergleich der Verteilungen Test vs. Train"),
             tags$img(
               src = "Präsentation/7) Histogramm Vergleich_train_test.png",
               style = "max-width: 100%; height: auto;"
             ),
             tags$div(
               class = "violett-box",
               tags$strong("Erkenntnisse:"),
               tags$ul(
                 tags$li("Trainings- und Testdaten haben sehr ähnliche Verteilungen."),
                 tags$li("Leichte Abweichungen z.B. bei der Anzahl Spenden im oberen Bereich.")
               ),
               tags$br(),
               tags$strong("Schlussfolgerung:"),
               tags$p("Die Unterschiede könnten die Modellperformance beeinflussen, sind aber nicht massiv.")
             )
    ),
    
    # Folie 8
    tags$div(class = "slide slide8",
             tags$h1("Folie 8: Weiterer Inhalt"),
             tags$p("Platzhalter...")
    ),
    
    # Folie 9
    tags$div(class = "slide slide9",
             tags$h1("Folie 9: Weiterer Inhalt"),
             tags$p("Platzhalter...")
    ),
    
    # Folie 10
    tags$div(class = "slide slide10",
             tags$h1("Folie 10: Weiterer Inhalt"),
             tags$p("Platzhalter...")
    )
  )
)


##############################################################################
# KAPITEL F) HTML-Seite anzeigen (html_print)
##############################################################################
# Wir können das erstellte HTML-Dokument direkt im RStudio-Viewer oder Browser ansehen.
# Alternativ kann man das Dokument in eine .html-Datei schreiben.

# 1) Direktanzeige in RStudio-Viewer/Browser (sofern von RStudio unterstützt):
html_print(page)

# 2) Falls man eine HTML-Datei im Ordner "Präsentation" erzeugen möchte,
#    kann man stattdessen/un zusätzlich:
# writeLines(as.character(page), con = file.path(presentation_dir, "Praesentation.html"))

message("✅ HTML-Foliensatz erzeugt & im Viewer angezeigt (ggf. RStudio-Viewer).")


##############################################################################
# KAPITEL G) Zusätzlicher Plot nur im RStudio-Viewer
##############################################################################
# Beispiel: Ein einfacher Plot (Base-R) oder ggplot, der NICHT als PNG gespeichert wird.
# Dies kann hilfreich sein, um Zwischenergebnisse / Debug-Infos im Viewer zu prüfen.

# G.1) Base-R-Beispiel: Kleines Balkendiagramm im Viewer
#     (Erzeugt KEINE Datei, nur RStudio-Plot-Fenster)
some_counts <- table(train_data$`Spende Maerz 2007`)  # Häufigkeit 0/1
barplot(
  some_counts,
  main = "Zusätzlicher Plot: Spende März 2007 (Train)",
  xlab = "Spende-Kategorie",
  ylab = "Häufigkeit",
  col  = c("skyblue", "orange")
)

message("✅ Zusätzlicher Viewer-Plot (Barplot) wurde erstellt.")
