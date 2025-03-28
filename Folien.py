###############################################################################
# Folien.py – Logistisches Regressionsmodell + Visualisierungen (für VS Code)
# Version mit expliziten Pfadangaben
###############################################################################

import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    confusion_matrix,
    ConfusionMatrixDisplay,
    roc_curve,
    auc,
    classification_report
)

###############################################################################
# 1) Globale Einstellungen: user_paths
###############################################################################

# >>> BITTE HIER anpassen! <<<
# Ordner, in dem die CSV-Dateien liegen
FALLSTUDIE_DIR = r"C:\Users\matth\OneDrive\AB1_R Projekte aktuell\Fallstudie"

# Pfad zur Trainings- und Testdatei
TRAIN_CSV = os.path.join(FALLSTUDIE_DIR, "bloodtrain.csv")
TEST_CSV  = os.path.join(FALLSTUDIE_DIR, "bloodtest.csv")

# Ordner für Output (PNG, CSV)
OUTDIR = os.path.join(FALLSTUDIE_DIR, "Python")

###############################################################################
# 2) Daten laden
###############################################################################

print("Lese Trainingsdaten ein:", TRAIN_CSV)
df_train = pd.read_csv(TRAIN_CSV)

print("Lese Testdaten ein:", TEST_CSV)
df_test  = pd.read_csv(TEST_CSV)

# Spalten umbenennen
df_train.columns = [
    "ID",
    "Monate Letzte Spende",
    "Anzahl Spenden",
    "Gesamtvolumen",
    "Monate Erste Spende",
    "Spende Maerz 2007"
]

df_test.columns = [
    "ID",
    "Monate Letzte Spende",
    "Anzahl Spenden",
    "Gesamtvolumen",
    "Monate Erste Spende"
]

###############################################################################
# 3) Feature Engineering
###############################################################################

# Neuer Prädiktor "Spendetakt" – vermeidet Division durch 0
df_train["Spendetakt"] = (
    df_train["Anzahl Spenden"] /
    df_train["Monate Letzte Spende"].replace(0, np.nan)
)
df_test["Spendetakt"] = (
    df_test["Anzahl Spenden"] /
    df_test["Monate Letzte Spende"].replace(0, np.nan)
)

###############################################################################
# 4) Modelltraining
###############################################################################

features = ["Monate Letzte Spende", "Anzahl Spenden", "Spendetakt"]
X_train = df_train[features].fillna(0)
y_train = df_train["Spende Maerz 2007"]

print("\nStarte Modelltraining...")
model = LogisticRegression(max_iter=1000)
model.fit(X_train, y_train)

###############################################################################
# 5) Modellzusammenfassung (Koeffizienten, Odds Ratios)
###############################################################################

coefficients = pd.DataFrame({
    "Feature": features,
    "Koeffizient": model.coef_[0],
    "Odds Ratio": np.exp(model.coef_[0])
})

print("\nModellzusammenfassung:")
print(coefficients)

###############################################################################
# 6) Vorhersagen – Trainingsdaten
###############################################################################

y_pred = model.predict(X_train)

###############################################################################
# 7) Konfusionsmatrix
###############################################################################

cm = confusion_matrix(y_train, y_pred)
cm_disp = ConfusionMatrixDisplay(confusion_matrix=cm, display_labels=["Nein", "Ja"])
cm_disp.plot(cmap="Blues")
plt.title("Konfusionsmatrix – Trainingsdaten")

# Output-Verzeichnis anlegen (falls nicht existiert)
os.makedirs(OUTDIR, exist_ok=True)

# Speichern
confmatrix_png = os.path.join(OUTDIR, "konfusionsmatrix_train.png")
plt.savefig(confmatrix_png, dpi=120, bbox_inches="tight")
print(f"Konfusionsmatrix gespeichert unter: {confmatrix_png}")

plt.show()

###############################################################################
# 8) ROC-Kurve & AUC
###############################################################################

y_probs = model.predict_proba(X_train)[:, 1]
fpr, tpr, _ = roc_curve(y_train, y_probs)
roc_auc = auc(fpr, tpr)

plt.figure()
plt.plot(fpr, tpr, color="darkorange", label=f"ROC (AUC = {roc_auc:.2f})")
plt.plot([0, 1], [0, 1], linestyle="--", color="navy")
plt.xlabel("False Positive Rate")
plt.ylabel("True Positive Rate")
plt.title("ROC-Kurve – Trainingsdaten")
plt.legend()

roc_png = os.path.join(OUTDIR, "roc_train.png")
plt.savefig(roc_png, dpi=120, bbox_inches="tight")
print(f"ROC-Kurve gespeichert unter: {roc_png}")

plt.show()

###############################################################################
# 9) Klassifikationsbericht
###############################################################################

print("\nKlassifikationsbericht (Train):")
print(classification_report(y_train, y_pred, target_names=["Nein", "Ja"]))

###############################################################################
# 10) Vorhersagen für Testdaten
###############################################################################

X_test = df_test[features].fillna(0)
y_test_pred = model.predict(X_test)
y_test_prob = model.predict_proba(X_test)[:, 1]

df_test["Spende Maerz 2007 Vorhersage"] = y_test_pred
df_test["Wahrscheinlichkeit Ja"] = y_test_prob

###############################################################################
# 11) Ergebnis-Datei schreiben
###############################################################################

prediction_csv = os.path.join(OUTDIR, "bloodtest_predictions.csv")
df_test[["ID", "Spende Maerz 2007 Vorhersage", "Wahrscheinlichkeit Ja"]].to_csv(
    prediction_csv,
    index=False
)

print(f"\nVorhersage-Datei gespeichert unter: {prediction_csv}")
print("\nFertig!")
