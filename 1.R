# A1.1 Daten laden ----
train_data <- read.csv(here::here("bloodtrain.csv"))
colnames(train_data)

test_data <- read.csv(here::here("bloodtest.csv"))
colnames(test_data)

submission_format <- read.csv(here::here("submission_format.csv"))
colnames(submission_format)


# A1.2 Spaltenumbenennung ----

# Neue kurze Bezeichnungen für die Spaltennamen Trainingsdaten
colnames(train_data) <- c(
  "ID",                           # X
  "Monate Letzte Spende",         # Months.since.Last.Donation
  "Anzahl Spenden",               # Number.of.Donations
  "Gesamtvolumen",                # Total.Volume.Donated..c.c..
  "Monate Erste Spende",          # Months.since.First.Donation
  "Spende März 2007"              # Made.Donation.in.March.2007
)

colnames(train_data)


# Neue kurze Bezeichnungen für die Spaltennamen Testdaten
colnames(test_data) <- c(
  "ID",                           # X
  "Monate Letzte Spende",         # Months.since.Last.Donation
  "Anzahl Spenden",               # Number.of.Donations
  "Gesamtvolumen",                # Total.Volume.Donated..c.c..
  "Monate Erste Spende"           # Months.since.First.Donation
)

colnames(test_data)


# Neue kurze Bezeichnungen für die Spaltennamen Submission-Format
colnames(submission_format) <- c(
  "ID",                           # X
  "Spende März 2007"              # Made.Donation.in.March.2007
)

colnames(submission_format)


# B1 Foliensatz 2: str Datenanalyse ----







