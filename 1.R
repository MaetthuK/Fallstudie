##############################################################################
# KAPITEL 0) PDF -> PNG: Folien 1 und 2 extrahieren
##############################################################################
# Falls du aus "Titelseite.pdf" (Seite 1) und "Workflow.pdf" (Seite 1) 
# PNG-Dateien generieren willst.

# Falls noch nicht installiert:
# install.packages("pdftools")

library(pdftools)
library(here)

# --- 0.1) Folie 1 -----------------------------------------------------------
pdf_convert(
  pdf       = here::here("Titelseite.pdf"),  # Pfad zur PDF (mind. 1 Seite)
  pages     = 1,                             # Seite 1
  filenames = here::here("Folie1.png"),      # Ziel-Dateiname
  dpi       = 300
)

# --- 0.2) Folie 2 -----------------------------------------------------------
# Falls "Workflow.pdf" nur 1 Seite hat, nimm pages=1
pdf_convert(
  pdf       = here::here("Workflow.pdf"),    # Pfad zur PDF
  pages     = 1,                             # Seite 1
  filenames = here::here("Folie2.png"),      # Ziel-Dateiname
  dpi       = 300
)


##############################################################################
# KAPITEL A) Daten laden & Spalten umbenennen
##############################################################################
# Lädt Trainings- und Testdaten und passt Spaltennamen an.

library(here)

# --- A.1) Daten einlesen ----------------------------------------------------
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
# capture.output() sammelt die Konsolenausgabe von summary(...)

train_sum <- capture.output(summary(train_data))
test_sum  <- capture.output(summary(test_data))


##############################################################################
# KAPITEL C) Boxplot erzeugen & als Bild abfangen (Folie 6)
##############################################################################
# Boxplot, der den Hintergrund in #ffe6f7 hat und keinen weissen Rand zeigt.

library(dplyr)
library(tidyr)
library(ggplot2)
library(base64enc)

# --- C.1) Kennzeichnung der Datensaetze --------------------------------------
train_data$Dataset <- "Train"
test_data$Dataset  <- "Test"

# --- C.2) Zusammenführen relevanter Spalten ---------------------------------
df <- bind_rows(
  select(train_data, Dataset,
         `Anzahl Spenden`, Gesamtvolumen,
         `Monate Erste Spende`, `Monate Letzte Spende`),
  select(test_data, Dataset,
         `Anzahl Spenden`, Gesamtvolumen,
         `Monate Erste Spende`, `Monate Letzte Spende`)
)

# --- C.3) Skalieren (z-Standardisierung) -------------------------------------
df_scaled <- df %>%
  mutate(across(!Dataset, scale))

# --- C.4) Long-Format -------------------------------------------------------
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

# --- C.5) Boxplot mit minimalem Weissraum ------------------------------------
p <- ggplot(df_long, aes(x = Variable, y = Wert, fill = Dataset)) +
  geom_boxplot(
    notch         = TRUE,
    notchwidth    = 0.3,
    outlier.shape = 21,
    alpha         = 0.7
  ) +
  # Keine Achsen-Expansion => kein zusaetzlicher Leerraum
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Skalierte numerische Variablen",
    x     = NULL,
    y     = "Skalierter Wert"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    # Hintergrund auf Folienfarbe #ffe6f7
    panel.background    = element_rect(fill = "#ffe6f7", color = NA),
    plot.background     = element_rect(fill = "#ffe6f7", color = NA),
    legend.background   = element_rect(fill = "#ffe6f7", color = NA),
    legend.key          = element_rect(fill = "#ffe6f7", color = NA),
    
    plot.title          = element_text(
      color = "black", size = 18, face = "bold", hjust = 0.5
    ),
    legend.position     = "right",
    legend.text         = element_text(color = "black"),
    legend.title        = element_text(color = "black", face = "bold"),
    axis.text           = element_text(color = "black"),
    axis.title          = element_text(color = "black"),
    aspect.ratio        = 0.25,
    # Plot-Rand vollstaendig entfernen
    plot.margin         = margin(0, 0, 0, 0)
  )

# --- C.6) PNG-Datei erzeugen und Base64-String -------------------------------
# bg = #ffe6f7 => PNG selbst hat keinen weissen Rand
plotfile <- tempfile(fileext = ".png")
png(plotfile, width = 1000, height = 500, res = 96, bg = "#ffe6f7")
print(p)
dev.off()

img_b64 <- base64enc::dataURI(file = plotfile, mime = "image/png")


##############################################################################
# KAPITEL D) HTML-Seite mit 10 „Foliens“ (Abschnitten)
##############################################################################
library(htmltools)

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
    
    # Folie 1
    tags$div(class = "slide slide1",
             tags$h1("Folie 1: Titelbild"),
             tags$img(
               src = here::here("Folie1.png"),
               style = "max-width: 100%; height: auto;"
             ),
             tags$a(
               href     = here::here("Titelseite.pdf"),
               download = NA,
               class    = "download-link",
               "Titelseite (PDF Download)"
             )
    ),
    
    # Folie 2
    tags$div(class = "slide slide2",
             tags$h1("Folie 2: Workflow Diagramm"),
             tags$img(
               src = here::here("Folie2.png"),
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
             tags$p("Hier koennte ein weiterer Inhalt stehen.")
    ),
    
    # Folie 4
    tags$div(class = "slide slide4",
             tags$h1("Folie 4: Statistische Kennzahlen"),
             tags$h2("Trainingsdaten Zusammenfassung"),
             tags$p("Mit 'summary' gewinnen wir einen statistischen Ueberblick."),
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
    
    # Folie 5
    tags$div(class = "slide slide5",
             tags$h1("Folie 5: ..."),
             tags$p("Noch ein Platzhalter fuer weiteren Inhalt.")
    ),
    
    # Folie 6
    tags$div(class = "slide slide6",
             tags$h1("Folie 6: Vergleich der Trainings- und Testdaten"),
             tags$img(
               src   = img_b64,
               style = "max-width: 100%; height: auto;"
             ),
             tags$div(
               class = "violett-box",
               tags$strong("Erkenntnisse:"),
               tags$ul(
                 tags$li("Verteilungen der Variablen sind nicht sehr unterschiedlich (Train vs. Test)."),
                 tags$li("Kerben der Boxen ueberlappen => Variabilitaet im Testdatensatz teils geringer.")
               ),
               tags$br(),
               tags$strong("Schlussfolgerung:"),
               tags$p("Train- und Testdaten sind Stichproben aus derselben Population.")
             )
    ),
    
    # Folie 7
    tags$div(class = "slide slide7",
             tags$h1("Folie 7: Weiterer Inhalt"),
             tags$p("Noch mehr Platzhalter...")
    ),
    
    # Folie 8
    tags$div(class = "slide slide8",
             tags$h1("Folie 8: Weiterer Inhalt"),
             tags$p("Noch mehr Platzhalter...")
    ),
    
    # Folie 9
    tags$div(class = "slide slide9",
             tags$h1("Folie 9: Weiterer Inhalt"),
             tags$p("Noch mehr Platzhalter...")
    ),
    
    # Folie 10
    tags$div(class = "slide slide10",
             tags$h1("Folie 10: Weiterer Inhalt"),
             tags$p("Noch mehr Platzhalter...")
    )
  )
)

##############################################################################
# KAPITEL F) HTML-Seite anzeigen
##############################################################################
html_print(page)
