# Variante A) mit unlink (empfohlen):
unlink("C:/Users/matth/OneDrive/AB1_R Projekte aktuell/Fallstudie/Präsentation/*.png",
       force = TRUE)

# Variante B) via system(...) und Windows-CMD:
system('cmd /c "del /F /Q \"C:/Users/matth/OneDrive/AB1_R Projekte aktuell/Fallstudie/Präsentation/*.png\""')

