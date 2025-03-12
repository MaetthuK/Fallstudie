# Zu beachten ist dass es das Ziel des Modells ist Nichtspender vorherzusagen.
# So können diese gezielt angesprochen werden um sie zu motivieren zu spenden.
# Dies impliziert dass der AUC möglichst niedrig sein sollte.
# Nachfolgend kurze Erläuterung der Schritte:
# 1. Setup
# 2. Datenverständnis
# 3. Explorative Datenanalyse
# 4. Feature Engineering
# 5. Modellierung und Evaluation
# 6. Fazit

# Modell: Logistische Regression Blood Donation
#
# Dieses Skript lädt Trainings- und Testdaten, führt eine explorative Datenanalyse durch,
# ermöglicht einfaches Feature Engineering (Skalierung, Transformationen, Clustering, Entfernen unwichtiger Variablen, Bootstrapping),
# trainiert ein logistisches Regressionsmodell und generiert abschliessend eine CSV-Datei mit Vorhersagen.
#
# Die Code-Struktur ist in Kapitel gegliedert und umfassend kommentiert, um sowohl fachlich versierten Personen
# als auch dem Management einen transparenten Einblick in den Workflow zu geben. 
# Die Vorgehensweise orientiert sich am gezeigten Beispiel.
# Es werden bewusst alle Schritte sichtbar gemacht, von der Datenerkundung bis hin zur Modellierung, 
# Optimierung und abschliessender Kommunikation.

## Pakete und Bibliotheken laden oder installieren ----

load_or_install <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

# Liste der benötigten Pakete
required_packages <- c("caret", "corrplot", "dplyr", "DT", "e1071", "GGally", 
                       "ggplot2", "gridExtra", "here", "pROC", "scales", 
                       "shiny", "shinyjs", "tidyr")

# Pakete laden oder installieren
load_or_install(required_packages)

# Manuelles Laden der Bibliotheken (optional, um sicherzugehen)
library(caret)     
library(corrplot)  
library(dplyr)     
library(DT)        
library(e1071)     
library(GGally)    
library(ggplot2)   
library(gridExtra) 
library(here)      
library(pROC)      
library(scales)    
library(shiny)     
library(shinyjs)   
library(tidyr)

# A - App-UI ----
ui <- fluidPage(
  
  useShinyjs(),  # shinyjs initialisieren
  
  # A1 Titel der App ----
  titlePanel("Blutspende-Datenanalyse"),
  
  # A2 CSS-Styling für die App ----
  tags$style(HTML(" 
    .dataTables_wrapper {
      margin-left: 0px !important;
    }
    .tab-pane h5 {
      margin-left: 0px;
    }
    table.dataTable {
      width: auto !important;
      white-space: nowrap;
    }
    .table {
      text-align: left;
    }
  ")),
  
  # A3 Sidebar-Layout ----
  sidebarLayout(
    sidebarPanel(
      
      # A3.1 Modellparameter Wahl ----
      h3("Modellparameter"),
      width = 3,
      
      # Dynamische Auswahl der Praediktoren
      uiOutput("predictors_ui"),
      
      # Button zum Trainieren des Modells
      actionButton(inputId = "trainModel", label = "Modell trainieren"),
      
      
      actionButton("applyBestEngineering", "Bestes gefundenes Feature Engineering ausführen"),
      
      tags$hr(),
      
      # A3.2 Feature Engineering ----
      h4("Feature Engineering"),
      helpText("Manuelle Praediktoren, Skalierung und Clustering, um das Modell ggf. zu verbessern."),
      
      # Skalierung
      actionButton("scaleData", "Daten skalieren (0-1)"),
      br(), br(),
      
      # Clustering
      actionButton("runClustering", "Clustering durchführen"),
      br(), br(),
      
      # Hinweis auf weitere Transformationsmöglichkeiten
      # Zusätzliche Buttons für weitere Transformationen und Anpassungen:
      h4("Weitere Verbesserungen:"),
      helpText("Daten transformieren (z. B. log, sqrt), Bootstrapping, Entfernen unwichtiger Variablen."),
      
      # Log-Transformation
      actionButton("transformData", "Log-Transformation auf 'MonateLetzteSpende' anwenden"),
      br(), br(),
      
      # Gesamtvolumen entfernen
      actionButton("removeGesamtvolumen", "Gesamtvolumen entfernen"),
      br(), br(),
      
      # Square-root-Transformation auf AnzahlSpenden
      actionButton("sqrtAnzahlSpenden", "Square-root-Transformation auf 'AnzahlSpenden' anwenden"),
      br(), br(),
      
      # Bootstrapping
      actionButton("applyBootstrapping", "Bootstrapping auf Trainingsdaten anwenden"),
      br(), br(),
      
      tags$p("
             WICHTIG: Ziel des Feature Engineerings ist es, einen niedrigeren AUC-Wert zu erhalten!
             Ein niedrigerer AUC-Wert bedeutet in diesem Kontext, dass wir Nichtspender 
             besser identifizieren (sie also vom Modell als solche korrekt einstufen) 
             und diese gezielt bewerben können.
             "),
      
      # CSV erstellen (Vorhersagen)
      actionButton("createCSV", "CSV erstellen"),
      br(),
      textOutput("csvStatus"),
      br(), br(),
      
      # Reset Button
      actionButton("reset", "Reset App"
      ),
      
      # Zusätzliche Anzeige der ROC-Kurve in der Sidebar
      h4("ROC-Kurve (Sidebar)"),
      plotOutput("fazitROC_sidebar", height = "300px")
    ),
    
    
    
    
    
    
    
    
    # A4 Hauptbereich (Tabs) ----
    mainPanel(
      tabsetPanel(
        
        # 1. Setup ----
        tabPanel(
          title = "1. Setup",
          br(),
          # 1.1 Pakete und Bibliotheken ----
          h4("1.1 Pakete und Bibliotheken"),
          tags$p("Hier werden die benötigten Pakete geladen, um die weitere Analyse und Modellierung zu ermöglichen. 
                 Ohne diese Pakete können bestimmte Funktionen (z. B. Visualisierungen, Modelltraining oder Datenbearbeitung) nicht ausgeführt werden."),
          verbatimTextOutput("loadedPackages"),
          br(),
          
          # 1.2 CSV-Dateien Anbindung ----
          h4("1.2 Anbindung der CSV-Dateien"),
          tags$p("Die Trainings- und Testdaten werden aus externen CSV-Dateien eingelesen. 
                  Diese Daten bilden die Grundlage für die gesamte Analyse. 
                  Der Trainingsdatensatz enthält bereits die Zielvariable (ob im März 2007 gespendet wurde), 
                  der Testdatensatz hingegen nicht."),
          verbatimTextOutput("csvConnectionStatus"),
          br(),
          
          # 1.3 Trainingsdaten Vorschau ----
          h4("1.3 Trainingsdaten Vorschau:"),
          tags$p("Diese Vorschau zeigt einen kleinen Ausschnitt der Trainingsdaten (z. B. die ersten 5 Zeilen), 
                  um einen ersten Eindruck über die Variablen und deren Struktur zu erhalten."),
          DTOutput("trainDataPreview"),
          br(),
          
          # 1.4 Testdaten Vorschau ----
          h4("1.4 Testdaten Vorschau:"),
          tags$p("Auch vom Testdatensatz wird eine Vorschau angezeigt, um sicherzustellen, dass er korrekt geladen wurde."),
          DTOutput("testDataPreview")
        ),
        
        # 2. Datenverständnis ----
        tabPanel(
          title = "2. Datenverständnis",
          tabsetPanel(
            
            # 2.1 Summary und Struktur Daten ----
            tabPanel(
              title = "2.1 Summary und Struktur Daten",
              h3("2.1 Datenzusammenfassung und Struktur"),
              tags$p(
                "In diesem Abschnitt werden die Trainings- und Testdaten summarisch betrachtet. 
                 Die 'summary'-Funktion gibt einen Überblick über Minimum, Maximum, Median, Mittelwert und Quartile, 
                 während 'str' die interne Struktur (Datentypen, Anzahl Beobachtungen und Variablen) darstellt."
              ),
              tags$p(
                "Diese Informationen helfen dabei, ein erstes Gefühl für die Daten zu bekommen, 
                 mögliche Ausreisser, fehlende Werte oder ungewöhnliche Verteilungen aufzudecken. 
                 Insbesondere interessieren uns Verteilungen von Spendenhäufigkeiten und Zeiträumen seit der letzten bzw. ersten Spende."
              ),
              
              tags$ul(
                style = "line-height: 1.0;",
                tags$li("MonateLetzteSpende: Wie viele Monate sind seit der letzten Spende vergangen?"),
                tags$li("AnzahlSpenden: Wie oft hat eine Person insgesamt gespendet?"),
                tags$li("Gesamtvolumen: Wie viel Blut wurde bisher insgesamt gespendet?"),
                tags$li("MonateErsteSpende: Wie lange ist die erste Spende her?")
              ),
              
              br(),
              fluidRow(
                # 2.1.1 Trainingsdaten Summary
                column(
                  width = 6,
                  h4("2.1.1 Trainingsdaten Zusammenfassung"),
                  tags$p("Mit 'summary' gewinnen wir einen statistischen Überblick über die Verteilung der einzelnen Variablen. 
                          Dies hilft, Ausreisser, fehlende Werte oder ungewöhnliche Verteilungen zu erkennen."),
                  verbatimTextOutput("trainSummary")
                ),
                # 2.1.2 Trainingsdaten Struktur
                column(
                  width = 6,
                  h4("2.1.2 Trainingsdaten Struktur"),
                  tags$p("Die 'str'-Funktion zeigt uns Datentypen, Anzahl Zeilen und Spalten. So sehen wir auf einen Blick, ob alles wie erwartet eingelesen wurde."),
                  verbatimTextOutput("strTrain")
                )
              ),
              
              tags$p("
                Die Zielvariable 'SpendeMaerz2007' ist besonders wichtig, 
                da sie das Vorhersageziel darstellt. Sie zeigt, ob ein/e Spender/in im März 2007 gespendet hat (Ja/Nein).
              "),
              
              # Verteilung der Zielvariable
              fluidRow(
                column(
                  width = 4,
                  h4("Verteilung der Zielvariable im Trainingsdatensatz"),
                  plotOutput("targetDistributionPlot", height = "300px")
                ),
                column(
                  width = 8,
                  h5("Rund ein Viertel der Spender hat im März 2007 erneut gespendet."),
                  br(),
                  h5("Die Zielvariable ist leicht unbalanciert, was bei der Modellierung berücksichtigt werden sollte."),
                  br(),
                  h5("Die Verteilung zeigt, dass die meisten Spender im März 2007 nicht gespendet haben.")
                )
              ),
              
              br(),
              fluidRow(
                # 2.1.3 Testdaten Zusammenfassung
                column(
                  width = 6,
                  h4("2.1.3 Testdaten Zusammenfassung"),
                  tags$p("Auch die Testdaten werden zusammengefasst, um sicherzustellen, dass deren Verteilungen vergleichbar sind."),
                  verbatimTextOutput("testSummary")
                ),
                # 2.1.4 Testdaten Struktur
                column(
                  width = 6,
                  h4("2.1.4 Struktur Testdaten"),
                  tags$p("Hier sehen wir, ob der Testdatensatz korrekt formatiert ist und ob die erwarteten Spalten vorliegen."),
                  verbatimTextOutput("strTest")
                )
              ),
              br()
            ),
            
            # 2.2 Datenbereinigung ----
            tabPanel(
              title = "2.2 Datenbereinigung",
              h3("2.2 Datenbereinigung"),
              tags$p("Bevor wir ein Modell bauen, müssen die Daten bereinigt werden. Dies umfasst das Entfernen von fehlenden Werten, 
                     das Erkennen und Behandeln von Duplikaten sowie die abschliessende Überprüfung, ob die bereinigten Daten für die Modellierung geeignet sind."),
              br(),
              
              fluidRow(
                column(
                  width = 6,
                  h4("2.2.1 Fehlende Werte"),
                  tags$p("
                    Fehlende Werte können Modelle stark beeinflussen. 
                    Hier wird geprüft, ob es fehlende Werte in den Trainings- oder Testdaten gibt.
                  "),
                  tableOutput("missingValues")
                ),
                column(
                  width = 6,
                  h4("2.2.2 Duplikate in Testdaten"),
                  tags$p("
                    Duplikate können die Modellbewertung verfälschen. 
                    Hier prüfen wir, ob im Testdatensatz doppelte IDs oder Datensätze vorkommen. 
                    Diese werden entfernt, um eine eindeutige Testgrundlage zu haben.
                  "),
                  tableOutput("duplikate")
                )
              ),
              
              br(),
              fluidRow(
                column(
                  width = 6,
                  h4("2.2.3 Bereinigte Testdaten Zusammenfassung"),
                  tags$p("
                    Nach dem Entfernen der Duplikate überprüfen wir erneut die Testdaten. 
                    Die statistischen Kennzahlen sollten weitgehend unverändert bleiben, 
                    nur die Anzahl der Beobachtungen reduziert sich auf eindeutige Einträge.
                  "),
                  verbatimTextOutput("cleanedTestSummary")
                ),
                column(
                  width = 6,
                  h4("2.2.4 Struktur bereinigte Testdaten"),
                  tags$p("
                    Abschliessend werfen wir einen Blick auf die Struktur der bereinigten Testdaten, 
                    um sicherzugehen, dass Datentypen und Anzahl Beobachtungen passen. 
                    Nun sind Trainings- und Testdaten bereit für die weitere Analyse.
                  "),
                  verbatimTextOutput("cleanedTestStr")
                )
              )
            ),
            
            # 2.3 Bedeutung Gesamtvolumen ----
            tabPanel(
              title = "2.3 Bedeutung Gesamtvolumen",
              h3("2.3 Bedeutung Gesamtvolumen"),
              tags$p("
        In diesem Abschnitt wird mittels einer Variable-Importance-Analyse gezeigt, 
        welche Variablen für das Modell besonders wichtig sind. Hier wird auch 'Gesamtvolumen' betrachtet, 
        um festzustellen, ob diese Variable für die Modellvorhersage relevant ist.
      "),
              tags$p("
        Eine niedrige Wichtigkeit des 'Gesamtvolumen' im Vergleich zu anderen Variablen 
        deutet darauf hin, dass diese Variable für die Modellvorhersage kaum Relevanz besitzt.
        Dadurch können wir schon früh begründen, warum wir 'Gesamtvolumen' im späteren Verlauf 
        (ab Kapitel 3) nicht mehr berücksichtigen oder sogar explizit entfernen.
      "),
              plotOutput("varImportanceGesamtvolumen", height = "600px")
            )
          )
        ),
        
        # 3. Explorative Datenanalyse ----
        tabPanel(
          title = "3. Explorative Datenanalyse",
          tabsetPanel(
            
            # 3.1 Boxplots mit Ausreissererkennung ----
            tabPanel(
              title = "3.1 Boxplots mit Ausreissererkennung",
              h3("3.1 Boxplots mit Ausreissererkennung"),
              
              # Sehr gute Beschreibung der Boxplots
              fluidRow(
                column(
                  width = 6,
                  plotOutput("dataVisualization_outlier", height = "900px")
                ),
                column(
                  width = 6,
                  tags$p("
        In 3.1 werden die numerischen Variablen aus Train- und Testdatensatz mittels Boxplots 
        einander gegenübergestellt. Der Zweck besteht darin, auf einen Blick Unterschiede in der 
        Verteilung, Streuung und potenzielle Ausreisser zwischen Trainings- und Testdaten sichtbar zu machen.
      "),
                  
                  tags$h4("Was zeigt der Boxplot allgemein?"),
                  tags$p("Ein Boxplot ist ein kompaktes Diagramm, um die Verteilung einer numerischen Variablen darzustellen. Er enthält folgende Elemente:"),
                  tags$ul(
                    tags$li(tags$b("Box (Kasten):"), "Der untere Rand der Box ist das 1. Quartil (Q1), der obere Rand ist das 3. Quartil (Q3). Die Box umfasst den mittleren 50%-Bereich der Daten."),
                    tags$li(tags$b("Median-Linie innerhalb der Box:"), "Eine horizontale Linie in der Box zeigt den Median an."),
                    tags$li(tags$b("Whiskers (Antennen):"), "Diese Linien reichen typischerweise bis 1.5 * IQR über Q3 bzw. unter Q1 hinaus."),
                    tags$li(tags$b("Ausreißer-Punkte:"), "Punkte ausserhalb der Whiskers sind Ausreißer.")
                  ),
                  
                  tags$h4("Zusätzliche Informationen im Plot"),
                  tags$p("Es werden mit Hilfe von stat_summary() weitere Informationen angezeigt:"),
                  tags$ul(
                    tags$li(tags$b("Grüner Punkt (Mean):"), "Markiert den Mittelwert."),
                    tags$li(tags$b("Lila Errorbars (Konfidenzintervall um den Mittelwert):"), "Zeigen Unsicherheit im Mittelwert an.")
                  ),
                  
                  tags$h4("Erkenntnisse aus der Gegenüberstellung (Train vs. Test)"),
                  tags$ul(
                    tags$li(tags$b("Lage (Median/Mean):"), "Unterscheide sich die Boxen, sind die Daten unterschiedlich verteilt."),
                    tags$li(tags$b("Streuung (Boxgrösse, Whiskerlänge):"), "Zeigt Variabilität der Daten."),
                    tags$li(tags$b("Ausreißer:"), "Können auf besondere Fälle oder Datenprobleme hindeuten.")
                  ),
                  
                  tags$h4("Fazit"),
                  tags$p("Die Boxplots liefern einen ersten visuellen Eindruck über Ähnlichkeiten und Unterschiede zwischen Trainings- und Testdaten.")
                )
              )
            ),
            
            # 3.2 Vergleich der Verteilungen ----
            tabPanel(
              title = "3.2 Vergleich der Verteilungen",
              h3("3.2 Vergleich der Verteilungen zwischen Train und Test"),
              tags$p("
                In diesem Abschnitt werden die Verteilungen wichtiger Variablen (z. B. Anzahl Spenden, 
                Monate seit letzter Spende) via Histogramme gegenübergestellt. 
                Dadurch erkennen wir, ob der Testdatensatz ähnliche Merkmale aufweist wie der Trainingsdatensatz.
              "),
              plotOutput("trainTestComparison", height = "900px")
            ),
            
            # 3.3 Pairs Plot ----
            tabPanel(
              title = "3.3 Pairs Plot",
              h3("3.3 Pairs Plot (Train und Test)"),
              tags$p("
                Ein Pairs Plot visualisiert Streudiagramme, Dichteverteilungen und Korrelationen aller numerischer Variablen. 
                Dies hilft, Beziehungen zwischen Variablen aufzudecken.
              "),
              plotOutput("pairsPlot", height = "900px")
            ),
            
            # 3.4 Korrelationsmatrix ----
            tabPanel(
              title = "3.4 Korrelationsmatrix",
              h3("3.4 Korrelationsmatrix"),
              tags$p("
                Die Korrelationsmatrix zeigt lineare Zusammenhänge zwischen den Variablen. 
                Hohe Korrelationen können auf Redundanzen hinweisen.
              "),
              plotOutput("correlationPlotEnhanced", height = "900px")
            ),
            
            # 3.5 Streumass ----
            tabPanel(
              title = "3.5 Streumass",
              h3("3.5 Streumass"),
              tags$p("
                Hier werden Varianz, Standardabweichung (Std_Abweichung) und der Interquartilsabstand (IQR) berechnet. 
                Diese Kennzahlen liefern Indizien über Streuung und Variabilität in den Daten.
              "),
              verbatimTextOutput("streumassSummary")
            ),
            
            # 3.6 Vergleich Original vs. Log-Transformation ----
            tabPanel(
              title = "3.6 Vergleich Original vs. Log-Transformation",
              h3("3.6 Vergleich Original vs. Log-Transformation"),
              tags$p("
                Eine Log-Transformation kann helfen, stark schiefe Verteilungen zu verbessern. 
                Dadurch können Residuen normaler verteilt sein, was die Regressionsanalyse stabiler macht.
              "),
              tags$ul(
                tags$li("Oben links: Original-Skala, Anzahl Spenden vs. Monate letzte Spende."),
                tags$li("Oben rechts: Log-transformierte Skala, ln(Monate letzte Spende) vs. Anzahl Spenden."),
                tags$li("Unten: Residuenplots für beide Fälle. Eine Log-Transformation kann Residuen gleichmässiger verteilen.")
              ),
              plotOutput("distributionPlot", height = "900px")
            )
          )
        ),
        
        # 4. Feature Engineering ----
        tabPanel(
          title = "4. Feature Engineering",
          tabsetPanel(
            
            # 4.1 Neue Prädiktoren, Skalierung & Clustering, Entfernen & Transformieren ----
            tabPanel(
              title = "4.1 Feature Engineering Massnahmen",
              h3("4.1 Feature Engineering Massnahmen"),
              tags$p("
                Im Feature Engineering werden Variablen transformiert, skaliert oder neu erstellt, 
                um das Modell weiter zu verbessern. Wir haben Buttons zum Skalieren (0-1), 
                zum Durchführen eines einfachen k-Means Clustering, zur Log-Transformation bestimmter Variablen, 
                zum Entfernen unwichtiger Variablen (z. B. Gesamtvolumen), zur Square-root-Transformation 
                von 'AnzahlSpenden' sowie zur Anwendung von Bootstrapping.
              "),
              tags$p("
                Diese Schritte können optional genutzt werden, bevor das Modell trainiert wird, 
                um die Modellgüte (hier gezielt einen niedrigeren AUC) zu erreichen. 
                Ein niedrigerer AUC bedeutet in diesem Fall, dass wir Nichtspender genauer identifizieren können.
              "),
              tags$p("
               Bedienung der Buttons:
               1) 'Daten skalieren (0-1)': 
                  Normalisiert alle numerischen Variablen auf den Bereich [0,1]. Dies verändert die Skala der Daten, 
                  erleichtert ggf. Clustering-Methoden, hat aber nicht zwingend direkten Einfluss auf die AUC.
              "),
              tags$p("
               2) 'Clustering durchführen':
                  Führt ein k-Means-Clustering (k=3) durch und zeigt die PCA-Visualisierung. 
                  Dies dient der Exploration. Die Clusterlabels werden nicht automatisch dem Modell hinzugefügt. 
                  Um die Wirkung auf die AUC zu prüfen, müsste man die Clusterausprägung als neue Variable 
                  ins Modell integrieren. Aktuell dient es nur der Einsicht.
               
               3) 'Log-Transformation auf 'MonateLetzteSpende' anwenden':
                  Transformiert die Werte von 'MonateLetzteSpende' logarithmisch. Dadurch können schiefe Verteilungen 
                  gemildert werden. Nach dem Anwenden dieser Transformation und erneuten Training des Modells 
                  kann sich die AUC ändern.
                  
               4) 'Gesamtvolumen entfernen':
                  Entfernt die Variable 'Gesamtvolumen' aus Trainings- und Testdaten, wenn diese als unwichtig erkannt wurde. 
                  Dadurch fokussiert sich das Modell auf wichtigere Variablen. 
                  Dies könnte zu einer Verbesserung (niedrigeren AUC) führen, sofern Gesamtvolumen störend wirkt.
                  
               5) 'Square-root-Transformation auf 'AnzahlSpenden' anwenden':
                  Wendet die Quadratwurzel-Transformation auf die Variable 'AnzahlSpenden' an. 
                  Dies kann insbesondere bei schiefen Verteilungen helfen. 
                  Danach sollte das Modell erneut trainiert werden.
                  
               6) 'Bootstrapping auf Trainingsdaten anwenden':
                  Hier wird ein erneutes Training des Modells mit Bootstrapping-Verfahren durchgeführt. 
                  Dadurch werden robustere Schätzungen der Parameter erreicht. Dies kann zu Veränderungen 
                  der AUC führen. Bitte nach dem Bootstrapping-Schritt erneut das Modell trainieren 
                  oder mittels Crossvalidation untersuchen.
              "),
              tags$p("
                Der Einsatz dieser Feature-Engineering-Massnahmen ist flexibel. 
                Man kann einzelne Schritte durchführen, dann das Modell trainieren (Button 'Modell trainieren') 
                und prüfen, ob die AUC gesunken ist. Ziel ist es, durch gezielte Transformationen und 
                Variablenauswahl einen für dieses Problem geeigneten, niedrigeren AUC-Wert zu erzielen.
              "),
              
              # Tabelle/Plots zu Clustering-Ergebnissen:
              tags$p("Nach dem Clustering wird ein PCA-Plot gezeigt, der die Cluster visualisiert. 
                      Dies dient der Exploration und kann in die spätere Feature-Auswahl einfliessen."),
              plotOutput("clusterPlot", height = "300px")
              ,
              
              # Neu: Protokoll-Ausgabe
              tags$h4("Feature-Engineering-Protokoll"),
              tags$p("Hier sehen Sie die Reihenfolge der ausgeführten Schritte und den jeweiligen AUC-Wert nach Trainingsläufen."),
              DTOutput("featureEngineeringLog")
            ),
            
            # 4.2 Wichtigkeit der Variablen ----
            tabPanel(
              title = "4.2 Wichtigkeit der Variablen",
              h3("4.2 Variable Importance"),
              tags$p("
                Sobald ein erstes Modell trainiert ist, kann die Wichtigkeit der Variablen beurteilt werden. 
                Hierbei wird geschaut, welche Variablen den grössten Einfluss auf die Vorhersage haben. 
                Dies kann helfen, unwichtige Variablen auszuschliessen oder sich auf wichtige Merkmale zu konzentrieren.
              "),
              plotOutput("variableImportancePlot", height = "600px")
            )
          )
        ),
        
        tabPanel(
          title = "5. Modell",
          
          # Erste Zeile mit drei Spalten: 5.1.1, 5.1.2, 5.1.3 nebeneinander
          fluidRow(
            column(
              width = 4,
              h4("5.1.1 Koeffizienten und Modellzusammenfassung"),
              tags$p("
        Hier werden die geschätzten Koeffizienten des logistischen Regressionsmodells, 
        sowie Kennzahlen wie AIC, Null- und Residual-Deviance angezeigt. 
        Diese helfen, die Modellgüte und den Einfluss einzelner Variablen zu beurteilen.
      "),
              verbatimTextOutput("modelSummary")
            ),
            column(
              width = 4,
              h4("5.1.2 Modellleistung"),
              tags$p("
        Basierend auf dem trainierten Modell wird die Performance auf den Trainingsdaten gemessen. 
        Metriken wie Genauigkeit, Präzision und Recall zeigen, wie gut das Modell die Zielvariable vorhersagen kann.
      "),
              tableOutput("modelPerformance")
            ),
            column(
              width = 4,
              h4("5.1.3 Erweiterte Metriken"),
              tags$p("
        Neben grundlegenden Kennzahlen werden erweiterte Metriken wie Log Loss, RMSE, R² und MAE berechnet, 
        um ein umfassendes Bild der Modellgüte zu erhalten.
      "),
              verbatimTextOutput("extendedMetrics")
            )
          ),
          
          br(),
          
          # Zweite Zeile mit zwei Spalten: 5.2.1 und 5.2.2 nebeneinander
          fluidRow(
            column(
              width = 6,
              h4("5.2.1 Konfusionsmatrix"),
              tags$p("
        Die Konfusionsmatrix zeigt, wie viele Fälle korrekt und inkorrekt klassifiziert wurden. 
        So sehen wir, ob das Modell eher Probleme damit hat, 'Ja'-Fälle (gespendet) 
        oder 'Nein'-Fälle (nicht gespendet) korrekt zu erkennen.
      "),
              verbatimTextOutput("confusionMatrix")
            ),
            column(
              width = 6,
              h4("5.2.2 Iterative Kreuzvalidierung"),
              tags$p("
        Durch Kreuzvalidierung (z. B. 5-fache Crossvalidation) wird die Modellrobustheit überprüft. 
        Dabei werden die Daten mehrfach in Trainings- und Validierungsteilmengen aufgeteilt. 
        Dies reduziert das Risiko, ein Modell zu erstellen, das nur auf dem gegebenen Trainingsdatensatz gut passt.
      "),
              verbatimTextOutput("cvModelSummary"),
              verbatimTextOutput("cvLogLoss")
            )
          )
        ),
        
        # 6. Fazit ----
        tabPanel(
          title = "6. Fazit",
          tabsetPanel(
            tabPanel(
              title = "6.1 Modellzusammenfassung",
              # 6.1.1 Prädiktoren und Modellzusammenfassung 
              h3("6.1.1 Prädiktoren und Modellzusammenfassung"),
              tags$p("
        Das Fazit fasst die wichtigsten Erkenntnisse zusammen. 
        Welche Variablen sind am wichtigsten? Wie gut schneidet das Modell insgesamt ab? 
        Gibt es Verbesserungspotenzial, etwa durch weitere Feature-Engineering-Schritte, 
        Hyperparameter-Tuning oder komplexere Modelle?
      "),
              verbatimTextOutput("fazitModelSummary"),
              
              h3("6.1.2 Visualisierung der Modellleistung"),
              tags$p("
        Abschliessend werden nochmals wichtige Leistungskennzahlen und die ROC-Kurve visualisiert. 
        Die ROC-Kurve und AUC bieten einen Überblick darüber, wie gut das Modell zwischen 'Ja' und 'Nein' unterscheidet, 
        unabhängig vom gewählten Schwellenwert.
      "),
              
              fluidRow(
                column(
                  width = 6,
                  h4("Zusammenfassung der Modellleistung"),
                  plotOutput("fazitPerformanceMetrics", height = "400px")
                ),
                column(
                  width = 6,
                  h4("ROC-Kurve des Modells (Fazit)"),
                  plotOutput("fazitROC", height = "400px")
                )
              )
            )
          )
        )
      )
    )
  )
)


# B - Server ----
server <- function(input, output, session) # ----
{ 
  
  rv <- reactiveValues(
    train = NULL,
    test = NULL,
    original_train = NULL,
    original_test = NULL,
    newPredictors = c(),
    bestPredictor = NULL,
    bestAccuracy = 0,
    clusterResult = NULL,
    cv_model = NULL,
    bootstrapped_model = NULL
  )
  
  # 1. Setup ----
  # 1.1 Pakete und Bibliotheken ----
  output$loadedPackages <- renderPrint({
    cat("Geladene Pakete:\n")
    cat(paste(required_packages, collapse = ", "))
  })
  
  # 1.2 CSV-Dateien Anbindung ----
  observe({
    # Trainingsdaten laden
    train_path <- here("bloodtrain.csv")
    if (file.exists(train_path)) {
      data_train <- read.csv(train_path, header = TRUE, stringsAsFactors = FALSE)
      colnames(data_train) <- c("ID", "MonateLetzteSpende", "AnzahlSpenden", 
                                "Gesamtvolumen", "MonateErsteSpende", 
                                "SpendeMaerz2007")
      data_train$SpendeMaerz2007 <- factor(
        data_train$SpendeMaerz2007, 
        levels = c(0, 1), 
        labels = c("Nein", "Ja")
      )
      rv$train <- data_train
      rv$original_train <- data_train
    } else {
      showNotification("Trainingsdatei 'bloodtrain.csv' nicht gefunden.", type = "error")
    }
    
    # Testdaten laden
    test_path <- here("bloodtest.csv")
    if (file.exists(test_path)) {
      data_test <- read.csv(test_path, header = TRUE, stringsAsFactors = FALSE)
      colnames(data_test) <- c("ID", "MonateLetzteSpende", "AnzahlSpenden", 
                               "Gesamtvolumen", "MonateErsteSpende")
      rv$test <- data_test
      rv$original_test <- data_test
    } else {
      showNotification("Testdatei 'bloodtest.csv' nicht gefunden.", type = "error")
    }
  })
  
  output$csvConnectionStatus <- renderPrint({
    cat("Train geladen:", ifelse(is.null(rv$train), "Nein", "Ja"), "\n")
    cat("Test geladen:", ifelse(is.null(rv$test), "Nein", "Ja"), "\n")
  })
  
  # 1.3 Trainingsdaten Vorschau ----
  output$trainDataPreview <- renderDT({
    req(rv$train)
    datatable(head(rv$train, 5), options = list(dom = 't'))
  })
  
  # 1.4 Testdaten Vorschau ----
  output$testDataPreview <- renderDT({
    req(rv$test)
    datatable(head(rv$test, 5), options = list(dom = 't'))
  })
  
  
  # 2. Datenverständnis ----
  # 2.1 Summary & Struktur ----
  output$trainSummary <- renderPrint({
    req(rv$train)
    summary(rv$train)
  })
  
  output$strTrain <- renderPrint({
    req(rv$train)
    str(rv$train)
  })
  
  # Verteilung Zielvariable
  output$targetDistributionPlot <- renderPlot({
    req(rv$train)
    if(!"SpendeMaerz2007" %in% colnames(rv$train)) {
      showNotification("Zielvariable 'SpendeMaerz2007' nicht im Trainingsdatensatz gefunden.", type = "error")
      return(NULL)
    }
    
    ggplot(rv$train, aes(x = SpendeMaerz2007, fill = SpendeMaerz2007)) +
      geom_bar(color = "black", alpha = 0.7) +
      scale_fill_manual(values = c("Nein" = "red", "Ja" = "green")) +
      labs(
        title = "Verteilung der Zielvariable (Train)",
        x = "Spende im März 2007",
        y = "Anzahl Beobachtungen"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12),
        legend.position = "none"
      )
  })
  
  output$correlationPlotEnhanced <- renderPlot({
    req(rv$train)
    data_num <- rv$train %>% select_if(is.numeric)
    cor_matrix <- cor(data_num, use = "pairwise.complete.obs")
    
    corrplot::corrplot(
      cor_matrix,
      method = "color",
      type = "upper",
      tl.col = "black",
      tl.srt = 45,
      addCoef.col = "black",
      number.cex = 0.8,
      title = "Korrelationsmatrix",
      cex.main = 1.8,
      font.main = 2,
      mar = c(0, 0, 1, 0)
    )
  }, height = 00)
  
  output$testSummary <- renderPrint({
    req(rv$test)
    summary(rv$test)
  })
  
  output$strTest <- renderPrint({
    req(rv$test)
    str(rv$test)
  })
  
  # 2.2 Datenbereinigung ----
  output$missingValues <- renderTable({
    req(rv$train, rv$test)
    train_missing <- sapply(rv$train, function(col) sum(is.na(col)))
    test_missing <- sapply(rv$test, function(col) sum(is.na(col)))
    
    all_vars <- union(names(train_missing), names(test_missing))
    train_missing <- train_missing[all_vars]
    test_missing <- test_missing[all_vars]
    
    test_missing[is.na(test_missing)] <- "x"
    
    data.frame(
      Variable = all_vars,
      Training = train_missing,
      Test = test_missing,
      stringsAsFactors = FALSE
    )
  })
  
  output$duplikate <- renderTable({
    req(rv$test)
    dup_ids <- rv$test %>%
      group_by(ID) %>%
      summarise(Frequenz = n()) %>%
      filter(Frequenz > 1)
    
    summary_dups <- dup_ids %>%
      group_by(Frequenz) %>%
      summarise(AnzahlZeilen = n()) %>%
      arrange(desc(Frequenz))
    
    summary_dups
  }, rownames = FALSE)
  
  cleaned_test_data <- reactive({
    req(rv$test)
    rv$test %>% distinct() %>% na.omit()
  })
  
  output$cleanedTestSummary <- renderPrint({
    req(cleaned_test_data())
    summary(cleaned_test_data())
  })
  
  output$cleanedTestStr <- renderPrint({
    req(cleaned_test_data())
    str(cleaned_test_data())
  })
  
  # 2.3 Bedeutung Gesamtvolumen ----
  output$varImportanceGesamtvolumen <- renderPlot({
    req(rv$train)
    
    data_model <- rv$train
    data_model$SpendeMaerz2007 <- factor(data_model$SpendeMaerz2007, levels = c("Nein","Ja"))
    
    model <- glm(SpendeMaerz2007 ~ . - ID, data = data_model, family = binomial)
    var_imp <- varImp(model, scale = FALSE)
    var_imp_df <- data.frame(
      Variable = rownames(var_imp),
      Importance = var_imp$Overall,
      stringsAsFactors = FALSE
    )
    
    ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Variable Importance (Gesamtvolumen)",
        x = "Variable",
        y = "Wichtigkeit"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12)
      )
  })
  
  
  # 3. Explorative Datenanalyse ----
  train_data <- reactive({
    req(rv$train)
    rv$train
  })
  
  test_data <- reactive({
    req(rv$test)
    rv$test
  })
  
  # 3.1 Boxplots (Ausreisser) ----
  output$dataVisualization_outlier <- renderPlot({
    req(train_data(), test_data())
    train <- train_data()
    test <- test_data()
    
    train$Datensatz <- "Train"
    test$Datensatz <- "Test"
    combined_data <- bind_rows(train, test)
    
    # Hier verzichten wir ab Kapitel 3 auf Gesamtvolumen (je nach Entscheidung),
    # aktuell ist es noch dabei, wird aber in späteren Schritten entfernt.
    combined_data_long <- combined_data %>%
      select(Datensatz, MonateErsteSpende, MonateLetzteSpende, AnzahlSpenden) %>%
      pivot_longer(cols = -Datensatz, names_to = "Variable", values_to = "Wert")
    
    ggplot(combined_data_long, aes(x = Datensatz, y = Wert, fill = Datensatz)) +
      geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = 0.6, notch = TRUE) +
      geom_jitter(aes(color = Datensatz), width = 0.2, alpha = 0.5) +
      stat_summary(fun = mean, geom = "point", shape = 18, color = "darkgreen", 
                   size = 3, show.legend = FALSE) +
      stat_summary(fun = mean, geom = "text", vjust = -0.5, 
                   aes(label = round(..y.., 2)), color = "darkgreen", 
                   size = 3) +
      stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2, 
                   color = "purple", size = 1, show.legend = FALSE) +
      facet_wrap(~ Variable, scales = "free_y") +
      theme_minimal() +
      labs(
        title = "Boxplot-Vergleich (Train vs Test)",
        x = "Datensatz",
        y = "Wert"
      ) +
      scale_fill_manual(values = c("Train" = "blue", "Test" = "red")) +
      scale_color_manual(values = c("Train" = "blue", "Test" = "red")) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        legend.position = "none"
      )
  })
  
  # 3.2 Vergleich der Verteilungen ----
  output$trainTestComparison <- renderPlot({
    req(train_data(), test_data())
    train <- train_data()
    test <- test_data()
    train$Dataset <- "Train"
    test$Dataset <- "Test"
    
    combined_data <- bind_rows(
      train[, c("MonateLetzteSpende", "AnzahlSpenden", "Gesamtvolumen", 
                "MonateErsteSpende", "Dataset")],
      test[, c("MonateLetzteSpende", "AnzahlSpenden", "Gesamtvolumen", 
               "MonateErsteSpende", "Dataset")]
    )
    
    p1 <- ggplot(combined_data, aes(x = MonateLetzteSpende, fill = Dataset)) + 
      geom_histogram(binwidth = 1, position = "dodge", color = "black") +
      labs(title = "Monate Letzte Spende", x = "Monate", y = "Frequenz") +
      theme_minimal() +
      scale_fill_manual(values = c("Train" = "blue", "Test" = "green"))
    
    p2 <- ggplot(combined_data, aes(x = AnzahlSpenden, fill = Dataset)) +
      geom_histogram(binwidth = 1, position = "dodge", color = "black") +
      labs(title = "Anzahl Spenden", x = "Anzahl", y = "Frequenz") +
      theme_minimal() +
      scale_fill_manual(values = c("Train" = "blue", "Test" = "green"))
    
    p4 <- ggplot(combined_data, aes(x = MonateErsteSpende, fill = Dataset)) +
      geom_histogram(binwidth = 5, position = "dodge", color = "black") +
      labs(title = "Monate Erste Spende", x = "Monate", y = "Frequenz") +
      theme_minimal() +
      scale_fill_manual(values = c("Train" = "blue", "Test" = "green"))
    
    grid.arrange(p1, p2, p4, ncol = 2)
  }, height = 900)
  
  # 3.3 Pairs Plot ----
  output$pairsPlot <- renderPlot({
    req(rv$train, rv$test)
    
    train <- rv$train
    test <- rv$test
    
    train$Dataset <- "Train"
    test$Dataset <- "Test"
    
    if (!"SpendeMaerz2007" %in% colnames(test)) {
      test$SpendeMaerz2007 <- NA
    }
    
    combined_data <- dplyr::bind_rows(
      train[, c("MonateLetzteSpende", "AnzahlSpenden", "MonateErsteSpende", "Dataset", "SpendeMaerz2007")],
      test[, c("MonateLetzteSpende", "AnzahlSpenden", "MonateErsteSpende", "Dataset", "SpendeMaerz2007")]
    )
    
    numeric_vars <- c("MonateLetzteSpende", "AnzahlSpenden", "MonateErsteSpende")
    
    GGally::ggpairs(
      data = combined_data,
      columns = numeric_vars,
      mapping = ggplot2::aes(color = Dataset),
      upper = list(continuous = wrap("cor", size = 5)),
      diag = list(continuous = "barDiag"),
      lower = list(continuous = wrap("smooth", alpha = 0.5, size = 0.5))
    ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Pairs Plot (Train & Test)",
        subtitle = "Oben: Korrelationen | Diagonal: Histogramme | Unten: Scatterplots"
      )
  })
  
  # 3.5 Streumass ----
  output$streumassSummary <- renderPrint({
    req(rv$train)
    numeric_vars <- c("MonateLetzteSpende", "AnzahlSpenden", "Gesamtvolumen", "MonateErsteSpende")
    streumass <- rv$train %>%
      select(all_of(numeric_vars)) %>%
      summarise_all(list(
        Varianz = ~ var(.x, na.rm = TRUE),
        Std_Abweichung = ~ sd(.x, na.rm = TRUE),
        IQR = ~ IQR(.x, na.rm = TRUE)
      ))
    print(streumass)
  })
  
  # 3.6 Vergleich Original vs. Log-Transformation ----
  output$distributionPlot <- renderPlot({
    req(rv$train)  
    x_var <- "AnzahlSpenden"
    y_var <- "MonateLetzteSpende"
    
    train_data_filtered <- rv$train %>% filter(.data[[y_var]] > 0)
    
    lm_model_y <- lm(MonateLetzteSpende ~ AnzahlSpenden, data = train_data_filtered)
    lm_model_logy <- lm(log(MonateLetzteSpende) ~ AnzahlSpenden, data = train_data_filtered)
    
    train_data_filtered$Pred_y <- predict(lm_model_y)
    train_data_filtered$Res_y <- train_data_filtered[[y_var]] - train_data_filtered$Pred_y
    train_data_filtered$Pred_logy <- predict(lm_model_logy)
    train_data_filtered$Res_logy <- log(train_data_filtered[[y_var]]) - train_data_filtered$Pred_logy
    
    library(patchwork)
    p1 <- ggplot(train_data_filtered, aes(x = .data[[x_var]], y = .data[[y_var]])) +
      geom_point(color = "grey50", alpha = 0.6) + 
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = "y vs. x (Original)", x = "Anzahl Spenden (x)", y = "Monate seit letzter Spende (y)")
    
    p2 <- ggplot(train_data_filtered, aes(x = .data[[x_var]], y = log(.data[[y_var]]))) +
      geom_point(color = "grey50", alpha = 0.6) + 
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = "ln(y) vs. x (Log-Transformiert)", x = "Anzahl Spenden (x)", y = "ln(Monate seit letzter Spende)")
    
    p3 <- ggplot(train_data_filtered, aes(x = Pred_y, y = Res_y)) +
      geom_point(color = "grey50", alpha = 0.6) + 
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuum (y)", x = "Vorhergesagte y", y = "y - y_hat")
    
    p4 <- ggplot(train_data_filtered, aes(x = Pred_logy, y = Res_logy)) +
      geom_point(color = "grey50", alpha = 0.6) + 
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "Residuum (ln(y))", x = "Vorhergesagte ln(y)", y = "ln(y) - ln(y_hat)")
    
    (p1 | p2) / (p3 | p4) + plot_annotation(
      title = "Vergleich: Original vs. Log-Transformation",
      subtitle = "Oben: Daten & Regressionsgerade / Unten: Residuenplots"
    )
  })
  
  
  # 4. Feature Engineering ----
  # Im Server-Teil fügen wir ein observeEvent für diesen neuen Button ein:
  # Beispielhafte Definition der log_step Funktion (falls noch nicht vorhanden)
  log_step <- function(schritt, auc = NA) {
    # Falls rv$log noch nicht existiert, initialisieren:
    if(!exists("log", where=rv)) {
      rv$log <- data.frame(
        Zeitpunkt = character(),
        Schritt = character(),
        AUC = numeric(),
        stringsAsFactors = FALSE
      )
    }
    
    rv$log <- rbind(rv$log, data.frame(
      Zeitpunkt = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      Schritt = schritt,
      AUC = ifelse(is.na(auc), NA, round(auc,3)),
      stringsAsFactors = FALSE
    ))
  }
  
  # Bei jedem Feature Engineering Button Event log_step aufrufen:
  
  observeEvent(input$transformData, {
    # ... hier steht Ihr bestehender Code für die Log-Transformation ...
    log_step("Log-Transformation auf 'MonateLetzteSpende' angewendet")
  })
  
  observeEvent(input$sqrtAnzahlSpenden, {
    # ... Code für sqrt-Transformation ...
    log_step("Square-root-Transformation auf 'AnzahlSpenden' angewendet")
  })
  
  observeEvent(input$scaleData, {
    # ... Code für Skalierung ...
    log_step("Daten auf [0,1] skaliert")
  })
  
  observeEvent(input$removeGesamtvolumen, {
    # ... Code für Gesamtvolumen entfernen ...
    log_step("'Gesamtvolumen' entfernt")
  })
  
  observeEvent(input$applyBootstrapping, {
    # ... Code für Bootstrapping ...
    log_step("Bootstrapping auf Trainingsdaten angewendet")
  })
  
  observeEvent(input$runClustering, {
    # ... Code für Clustering ...
    log_step("Clustering (k-Means) durchgeführt")
  })
  
  # Wenn Sie Ihren "Bestes gefundenes Feature Engineering" Button haben:
  observeEvent(input$applyBestEngineering, {
    # ... Code für die Reihenfolge der Transformationen ...
    log_step("Bestes gefundenes Feature-Engineering ausgeführt")
  })
  
  # RenderDT für das Feature-Engineering-Protokoll:
  observeEvent(input$transformData, {
    req(rv$train)
    if("MonateLetzteSpende" %in% names(rv$train)) {
      pos_values <- rv$train$MonateLetzteSpende > 0
      rv$train$MonateLetzteSpende[pos_values] <- log(rv$train$MonateLetzteSpende[pos_values])
      if("MonateLetzteSpende" %in% names(rv$test)) {
        pos_values_test <- rv$test$MonateLetzteSpende > 0
        rv$test$MonateLetzteSpende[pos_values_test] <- log(rv$test$MonateLetzteSpende[pos_values_test])
      }
      showNotification("Log-Transformation auf 'MonateLetzteSpende' angewendet.", type="message")
      log_step("Log-Transformation auf 'MonateLetzteSpende' angewendet")
    }
  })
  
  
  
  
  
  observeEvent(input$applyBestEngineering, {
    # Reihenfolge der Transformationsschritte:
    
    # 1. Log-Transformation auf 'MonateLetzteSpende'
    if("MonateLetzteSpende" %in% names(rv$train)) {
      pos_values <- rv$train$MonateLetzteSpende > 0
      rv$train$MonateLetzteSpende[pos_values] <- log(rv$train$MonateLetzteSpende[pos_values])
      if("MonateLetzteSpende" %in% names(rv$test)) {
        pos_values_test <- rv$test$MonateLetzteSpende > 0
        rv$test$MonateLetzteSpende[pos_values_test] <- log(rv$test$MonateLetzteSpende[pos_values_test])
      }
    }
    showNotification("Log-Transformation auf 'MonateLetzteSpende' angewendet (1. Durchgang).", type="message")
    
    # 2. Square-root-Transformation auf 'AnzahlSpenden'
    if("AnzahlSpenden" %in% names(rv$train)) {
      pos_values <- rv$train$AnzahlSpenden >= 0
      rv$train$AnzahlSpenden[pos_values] <- sqrt(rv$train$AnzahlSpenden[pos_values])
      if("AnzahlSpenden" %in% names(rv$test)) {
        pos_values_test <- rv$test$AnzahlSpenden >= 0
        rv$test$AnzahlSpenden[pos_values_test] <- sqrt(rv$test$AnzahlSpenden[pos_values_test])
      }
    }
    showNotification("Square-root-Transformation auf 'AnzahlSpenden' angewendet (1. Durchgang).", type="message")
    
    # 3. Daten skalieren (0-1)
    numeric_cols <- rv$train %>% select_if(is.numeric) %>% names()
    numeric_cols <- setdiff(numeric_cols, "SpendeMaerz2007")
    if(length(numeric_cols)>0) {
      rv$train[numeric_cols] <- lapply(rv$train[numeric_cols], scales::rescale)
      rv$test[numeric_cols] <- lapply(rv$test[numeric_cols], scales::rescale)
      showNotification("Daten wurden auf [0,1] skaliert.", type="message")
    } else {
      showNotification("Keine numerischen Spalten zum Skalieren gefunden.", type="warning")
    }
    
    # 4. Log-Transformation auf 'MonateLetzteSpende' erneut
    if("MonateLetzteSpende" %in% names(rv$train)) {
      pos_values <- rv$train$MonateLetzteSpende > 0
      rv$train$MonateLetzteSpende[pos_values] <- log(rv$train$MonateLetzteSpende[pos_values])
      if("MonateLetzteSpende" %in% names(rv$test)) {
        pos_values_test <- rv$test$MonateLetzteSpende > 0
        rv$test$MonateLetzteSpende[pos_values_test] <- log(rv$test$MonateLetzteSpende[pos_values_test])
      }
    }
    showNotification("Log-Transformation auf 'MonateLetzteSpende' angewendet (2. Durchgang).", type="message")
    
    # 5. Square-root-Transformation auf 'AnzahlSpenden' erneut
    if("AnzahlSpenden" %in% names(rv$train)) {
      pos_values <- rv$train$AnzahlSpenden >= 0
      rv$train$AnzahlSpenden[pos_values] <- sqrt(rv$train$AnzahlSpenden[pos_values])
      if("AnzahlSpenden" %in% names(rv$test)) {
        pos_values_test <- rv$test$AnzahlSpenden >= 0
        rv$test$AnzahlSpenden[pos_values_test] <- sqrt(rv$test$AnzahlSpenden[pos_values_test])
      }
    }
    showNotification("Square-root-Transformation auf 'AnzahlSpenden' angewendet (2. Durchgang).", type="message")
    
    showNotification("Bestes gefundenes Feature Engineering erfolgreich angewendet! Bitte Modell neu trainieren.", type="message")
  })
  
  
  output$predictors_ui <- renderUI({
    req(rv$train)
    predictor_choices <- setdiff(names(rv$train), c("ID", "SpendeMaerz2007"))
    checkboxGroupInput(
      inputId = "predictors",
      label = "Wähle Prädiktoren:",
      choices = predictor_choices,
      selected = predictor_choices
    )
  })
  
  # Daten Skalieren (0-1)
  observeEvent(input$scaleData, {
    numeric_cols <- rv$train %>% select_if(is.numeric) %>% names()
    numeric_cols <- setdiff(numeric_cols, "SpendeMaerz2007")
    if(length(numeric_cols)>0) {
      rv$train[numeric_cols] <- lapply(rv$train[numeric_cols], scales::rescale)
      rv$test[numeric_cols] <- lapply(rv$test[numeric_cols], scales::rescale)
      showNotification("Daten wurden auf [0,1] skaliert.", type="message")
      log_step("Daten skaliert (0-1)")
    } else {
      showNotification("Keine numerischen Spalten zum Skalieren gefunden.", type="warning")
    }
  })
  
  
  # Clustering
  observeEvent(input$runClustering, {
    req(rv$train)
    numeric_vars <- setdiff(names(rv$train)[sapply(rv$train,is.numeric)], c("ID","SpendeMaerz2007"))
    if(length(numeric_vars)>1) {
      df_cluster <- scale(rv$train[numeric_vars])
      set.seed(123)
      k <- 3
      cl <- kmeans(df_cluster, centers=k)
      rv$clusterResult <- cl
    } else {
      showNotification("Nicht genug numerische Variablen zum Clustern.", type="warning")
    }
  })
  
  output$clusterPlot <- renderPlot({
    req(rv$clusterResult, rv$train)
    numeric_vars <- setdiff(names(rv$train)[sapply(rv$train,is.numeric)], c("ID","SpendeMaerz2007"))
    req(length(numeric_vars)>1)
    df_cluster <- scale(rv$train[numeric_vars])
    pca <- prcomp(df_cluster)
    plot(pca$x[,1], pca$x[,2], col=rv$clusterResult$cluster, pch=19,
         xlab="PC1", ylab="PC2",
         main="PCA-Visualisierung der Cluster")
    legend("topright", legend=paste("Cluster",1:3), col=1:3, pch=19)
  })
  
  # Log-Transformation auf 'MonateLetzteSpende'
  observeEvent(input$transformData, {
    req(rv$train)
    if("MonateLetzteSpende" %in% names(rv$train)) {
      pos_values <- rv$train$MonateLetzteSpende > 0
      rv$train$MonateLetzteSpende[pos_values] <- log(rv$train$MonateLetzteSpende[pos_values])
      if("MonateLetzteSpende" %in% names(rv$test)) {
        pos_values_test <- rv$test$MonateLetzteSpende > 0
        rv$test$MonateLetzteSpende[pos_values_test] <- log(rv$test$MonateLetzteSpende[pos_values_test])
      }
      showNotification("Log-Transformation auf 'MonateLetzteSpende' angewendet.", type="message")
    }
  })
  
  # Gesamtvolumen entfernen
  observeEvent(input$removeGesamtvolumen, {
    req(rv$train, rv$test)
    if("Gesamtvolumen" %in% names(rv$train)) {
      rv$train <- rv$train %>% select(-Gesamtvolumen)
    }
    if("Gesamtvolumen" %in% names(rv$test)) {
      rv$test <- rv$test %>% select(-Gesamtvolumen)
    }
    showNotification("'Gesamtvolumen' wurde aus den Daten entfernt.", type="message")
    
    predictor_choices <- setdiff(names(rv$train), c("ID", "SpendeMaerz2007"))
    updateCheckboxGroupInput(session, "predictors", 
                             choices = predictor_choices,
                             selected = predictor_choices)
  })
  
  # Sqrt-Transformation auf AnzahlSpenden
  observeEvent(input$sqrtAnzahlSpenden, {
    req(rv$train)
    if("AnzahlSpenden" %in% names(rv$train)) {
      pos_values <- rv$train$AnzahlSpenden >= 0
      rv$train$AnzahlSpenden[pos_values] <- sqrt(rv$train$AnzahlSpenden[pos_values])
      if("AnzahlSpenden" %in% names(rv$test)) {
        pos_values_test <- rv$test$AnzahlSpenden >= 0
        rv$test$AnzahlSpenden[pos_values_test] <- sqrt(rv$test$AnzahlSpenden[pos_values_test])
      }
      showNotification("Square-root-Transformation auf 'AnzahlSpenden' angewendet.", type="message")
    }
  })
  
  # Bootstrapping anwenden
  observeEvent(input$applyBootstrapping, {
    req(rv$train)
    set.seed(123)
    predictors_for_cv <- input$predictors
    if(length(predictors_for_cv) == 0) {
      predictors_for_cv <- setdiff(names(rv$train), c("ID","SpendeMaerz2007"))
    }
    form_cv <- as.formula(paste("SpendeMaerz2007 ~", paste(predictors_for_cv, collapse = " + ")))
    
    cv_control_boot <- trainControl(
      method = "boot",
      number = 50,
      classProbs = TRUE,
      summaryFunction = mnLogLoss
    )
    
    boot_model <- train(
      form_cv,
      data = rv$train,
      method = "glm",
      metric = "logLoss",
      maximize = FALSE,
      trControl = cv_control_boot
    )
    
    rv$bootstrapped_model <- boot_model
    showNotification("Bootstrapping auf Trainingsdaten angewendet. Bitte Modell neu trainieren um Effekte zu prüfen.", 
                     type="message")
  })
  
  best_model <- reactive({
    req(rv$train)
    preds <- setdiff(names(rv$train), c("ID","SpendeMaerz2007"))
    if(length(preds) == 0) return(NULL)
    form <- as.formula(paste("SpendeMaerz2007 ~", paste(preds, collapse = " + ")))
    glm(form, data=rv$train, family=binomial())
  })
  
  output$variableImportancePlot <- renderPlot({
    req(best_model())
    var_imp <- varImp(best_model(), scale=FALSE)
    var_imp_df <- data.frame(
      Variable = rownames(var_imp),
      Importance = var_imp$Overall
    )
    ggplot(var_imp_df, aes(x=reorder(Variable, Importance), y=Importance)) +
      geom_bar(stat="identity", fill="skyblue") +
      coord_flip() +
      labs(
        title = "Variable Importance (bester Prädiktorsatz)",
        x = "Variable",
        y = "Wichtigkeit"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size=18, face="bold"),
        axis.title=element_text(size=16),
        axis.text=element_text(size=14)
      )
  })
  
  
  # 5. Modell ----
  
  # Modelltrainging
  trained_model <- eventReactive(input$trainModel, {
    req(rv$train, input$predictors)
    if(length(input$predictors)==0) {
      showNotification("Keine Praediktoren ausgewählt. Wähle mindestens einen Praediktor.", type="error")
      return(NULL)
    }
    form <- as.formula(paste("SpendeMaerz2007 ~", paste(input$predictors, collapse = " + ")))
    model <- glm(form, data=rv$train, family=binomial())
    showNotification("Modell trainiert.", type="message")
    model
  })
  
  # 5.1.1 Koeffizienten und Modellzusammenfassung ----
  output$modelSummary <- renderPrint({
    req(trained_model())
    ms <- summary(trained_model())
    cat("Koeffizienten:\n")
    print(ms$coefficients)
    cat("\nAIC:", ms$aic, "\n")
    cat("Null Deviance:", ms$null.deviance, "\n")
    cat("Residual Deviance:", ms$deviance, "\n")
  })
  
  # 5.1.2 Modellleistung ----
  performance <- reactive({
    req(trained_model())
    prob <- predict(trained_model(), type="response")
    pred_class <- ifelse(prob>0.5,"Ja","Nein")
    actual <- rv$train$SpendeMaerz2007
    cm <- table(Predicted=pred_class, Actual=actual)
    accuracy <- sum(diag(cm))/sum(cm)
    precision <- if("Ja" %in% rownames(cm) && "Ja" %in% colnames(cm)){
      cm["Ja","Ja"]/sum(cm["Ja",])} else {NA}
    recall <- if("Ja" %in% rownames(cm) && "Ja" %in% colnames(cm)){
      cm["Ja","Ja"]/sum(cm[,"Ja"])} else {NA}
    
    data.frame(
      Metrik=c("Genauigkeit","Präzision","Recall"),
      Wert=c(round(accuracy,3), round(precision,3), round(recall,3)),
      stringsAsFactors=FALSE
    )
  })
  
  output$modelPerformance <- renderTable({
    req(performance())
    performance()
  }, rownames=FALSE)
  
  # 5.1.3 Erweiterte Metriken ----
  output$extendedMetrics <- renderPrint({
    req(trained_model())
    prob <- predict(trained_model(), type="response")
    actual <- ifelse(rv$train$SpendeMaerz2007=="Ja",1,0)
    
    eps <- 1e-15
    prob_clipped <- pmax(pmin(prob,1-eps),eps)
    log_loss <- -mean(actual*log(prob_clipped)+(1-actual)*log(1-prob_clipped))
    rmse <- sqrt(mean((actual - prob)^2))
    var_tot <- sum((actual-mean(actual))^2)
    var_res <- sum((actual - prob)^2)
    r_squared <- 1 - var_res/var_tot
    mae <- mean(abs(actual - prob))
    
    cat("Log Loss:", round(log_loss,4), "\n")
    cat("RMSE:", round(rmse,4), "\n")
    cat("R²:", round(r_squared,4), "\n")
    cat("MAE:", round(mae,4), "\n")
  })  
  
  # 5.2.1 Konfusionsmatrix ----
  output$confusionMatrix <- renderPrint({
    req(trained_model())
    prob <- predict(trained_model(), type="response")
    pred_class <- ifelse(prob > 0.5, "Ja", "Nein")
    actual <- rv$train$SpendeMaerz2007
    cm <- confusionMatrix(factor(pred_class, levels=c("Nein","Ja")),
                          factor(actual, levels=c("Nein","Ja")))
    print(cm)
  })
  
  # 5.2.2 Iterative Kreuzvalidierung ----
  observeEvent(input$trainModel, {
    req(rv$train)
    set.seed(123)
    cv_control <- trainControl(
      method = "repeatedcv",
      number = 5,
      repeats = 3,
      classProbs = TRUE,
      summaryFunction = mnLogLoss
    )
    
    predictors_for_cv <- input$predictors
    if(length(predictors_for_cv) == 0) {
      predictors_for_cv <- setdiff(names(rv$train), c("ID","SpendeMaerz2007"))
    }
    
    form_cv <- as.formula(paste("SpendeMaerz2007 ~", paste(predictors_for_cv, collapse = " + ")))
    cv_model <- train(
      form_cv,
      data = rv$train,
      method = "glm",
      metric = "logLoss",
      maximize = FALSE,
      trControl = cv_control
    )
    
    rv$cv_model <- cv_model
  })
  
  output$cvModelSummary <- renderPrint({
    req(rv$cv_model)
    print(rv$cv_model)
  })
  
  output$cvLogLoss <- renderPrint({
    req(rv$cv_model)
    res <- rv$cv_model$results
    cat("Ergebnisse der Kreuzvalidierung:\n")
    print(res)
    cat("\nBeste Einstellung mit minimalem LogLoss:\n")
    print(rv$cv_model$bestTune)
  })
  
  # 5.7 CSV-Erstellung ----
  observeEvent(input$createCSV, {
    req(trained_model(), rv$test)
    prob_test <- predict(trained_model(), newdata = rv$test, type = "response")
    submission_df <- data.frame(
      ID = rv$test$ID,
      Probability_Yes = prob_test,
      stringsAsFactors = FALSE
    )
    
    write.csv(submission_df, file = "glm_submission.csv", row.names = FALSE)
    output$csvStatus <- renderText({
      paste("CSV 'glm_submission.csv' wurde erstellt!")
    })
  })
  
  roc_obj <- reactive({
    req(trained_model())  # Wenn kein Modell trainiert ist, gibt es keinen roc_obj
    prob <- predict(trained_model(), type="response")
    actual <- rv$train$SpendeMaerz2007
    roc(response=actual, predictor=prob, levels=c("Nein","Ja"), direction=">")
  })
  
  # 6. Fazit ----
  output$fazitModelSummary <- renderPrint({
    req(trained_model())
    ms <- summary(trained_model())
    cat("Erkenntnisse aus der Modellzusammenfassung:\n")
    cat("-------------------------------------------------\n")
    cat("Wichtige Praediktoren und ihre Koeffizienten:\n")
    
    coef_summary <- ms$coefficients
    top_predictors <- coef_summary[order(-abs(coef_summary[, "Estimate"])), , drop = FALSE]
    print(top_predictors)
    
    cat("\nAIC des Modells:", ms$aic, "\n")
    cat("Niedrigeres AIC deutet auf ein besser passendes Modell hin.\n")
    cat("-------------------------------------------------\n")
  })
  
  output$fazitPerformanceMetrics <- renderPlot({
    req(performance())
    ggplot(performance(), aes(x = Metrik, y = Wert, fill = Metrik)) +
      geom_bar(stat = "identity", color = "black", alpha = 0.8) +
      labs(
        title = "Zusammenfassung der Modellleistung",
        x = "Metrik",
        y = "Wert"
      ) +
      theme_minimal() +
      scale_fill_manual(values = c("Genauigkeit" = "blue", "Präzision" = "green", "Recall" = "orange")) +
      theme(legend.position = "none")
  })
  
  output$fazitROC <- renderPlot({
    req(roc_obj())
    plot(
      roc_obj(), 
      col = "darkred", 
      main = "ROC-Kurve des Modells (Fazit)", 
      legacy.axes = TRUE
    )
    abline(a = 0, b = 1, lty = 2, col = "gray")
    legend(
      "bottomright", 
      legend = paste("AUC =", round(auc(roc_obj()), 3)), 
      col = "darkred", 
      lwd = 2
    )
  })
  
  
  # Neue Ausgabe für ROC in der Sidebar
  # Beispielhafte Definition der log_step Funktion
  log_step <- function(schritt, auc = NA) {
    # Falls rv$log noch nicht existiert, initialisieren:
    if(!exists("rv$log", where=rv)) {
      rv$log <- data.frame(Zeitpunkt=character(), Schritt=character(), AUC=numeric(), stringsAsFactors=FALSE)
    }
    
    rv$log <- rbind(rv$log, data.frame(
      Zeitpunkt = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      Schritt = schritt,
      AUC = ifelse(is.na(auc), NA, round(auc,3)),
      stringsAsFactors = FALSE
    ))
  }
  
  output$fazitROC_sidebar <- renderPlot({
    req(roc_obj())
    plot(roc_obj(), col="darkred", main="ROC (Sidebar)", legacy.axes=TRUE)
    abline(a=0,b=1,lty=2,col="gray")
    legend("bottomright", legend=paste("AUC =", round(auc(roc_obj()),3)), col="darkred", lwd=2)
  })
  
  
  
  # Reset-Funktion ----
  observeEvent(input$reset, {
    # Statt alle Variablen von Hand zurückzusetzen, nutzen wir session$reload()
    # um die gesamte App neu zu laden. Damit ist der Ausgangszustand wiederhergestellt.
    
    session$reload()
    
    current_tab <- input$mainTabs
    
    # Alle relevanten reaktiven Werte auf den Ursprungszustand zurücksetzen
    rv$train <- rv$original_train
    rv$test <- rv$original_test
    rv$newPredictors <- c()
    rv$bestPredictor <- NULL
    rv$bestAccuracy <- 0
    rv$clusterResult <- NULL
    rv$cv_model <- NULL
    rv$bootstrapped_model <- NULL
    
    # Auch das trainierte Modell ist jetzt indirekt "gelöscht", da kein neuer Modelltrainings-Event ausgelöst wurde.
    
    # Prädiktoren wieder auf Ausgangszustand setzen
    predictor_choices <- setdiff(names(rv$original_train), c("ID","SpendeMaerz2007"))
    updateCheckboxGroupInput(session, "predictors",
                             choices = predictor_choices,
                             selected = predictor_choices)
    
    
    updateTabsetPanel(session, "mainTabs", selected = "5. Modell")
    
    showNotification("App zurückgesetzt. Wechsel zu Register 5.", type="message")
  })
  
  
  
  
  output$fazitROC <- renderPlot({
    req(roc_obj())
    plot(
      roc_obj(), 
      col = "darkred", 
      main = "ROC-Kurve des Modells (Fazit)", 
      legacy.axes = TRUE
    )
    abline(a = 0, b = 1, lty = 2, col = "gray")
    legend(
      "bottomright", 
      legend = paste("AUC =", round(auc(roc_obj()), 3)), 
      col = "darkred", 
      lwd = 2
    )
  })
}

# ---- App starten ----
shinyApp(ui=ui, server=server)


