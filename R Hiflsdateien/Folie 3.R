###############################################################################
# 3) FOLIE 3: STATISTISCHE KENNZAHLEN (PDF, A4 QUER)
###############################################################################
train_sum <- capture.output(summary(train_data))
test_sum  <- capture.output(summary(test_data))

presentation_dir <- here::here("Präsentation")
pdf_filename <- file.path(presentation_dir, "Folie3_Statistische_Kennzahlen.pdf")

pdf(
  pdf_filename,
  width=11.7,
  height=8.3
)

grid.newpage()

# Hellgrüner Seitenhintergrund
grid.rect(
  x=0.5, y=0.5,
  width=1, height=1,
  gp=gpar(fill="#ccffcc", col=NA)
)

# Äußerer schwarzer Rahmen
grid.rect(gp=gpar(lwd=3, col="black", fill=NA))

# 5 Zeilen Layout
layout_heights <- unit(c(0.15, 0.1, 0.325, 0.1, 0.325), "npc")
pushViewport(viewport(layout=grid.layout(nrow=5, ncol=1, heights=layout_heights)))

# Zeile 1: Titel
pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
grid.rect(gp=gpar(fill="#669966", col="black", lwd=2))
grid.text(
  "Statistische Kennzahlen",
  gp=gpar(fontsize=24, fontface="bold", col="white")
)
popViewport()

# Zeile 2: Untertitel "Trainingsdaten"
pushViewport(viewport(layout.pos.row=2, layout.pos.col=1))
grid.text(
  "Trainingsdaten Zusammenfassung",
  gp=gpar(fontsize=16, fontface="bold")
)
popViewport()

# Zeile 3: Rotes Rechteck + Summary Train
pushViewport(viewport(layout.pos.row=3, layout.pos.col=1, width=0.9))
grid.rect(gp=gpar(fill="#FFCCCC", col="black", lwd=2))
train_text <- paste(train_sum, collapse="\n")
grid.text(
  train_text,
  x=0.5, y=0.95,
  just=c("center","top"),
  gp=gpar(fontfamily="Courier", fontsize=10, col="black")
)
popViewport()

# Zeile 4: Untertitel "Testdaten"
pushViewport(viewport(layout.pos.row=4, layout.pos.col=1))
grid.text(
  "Testdaten Zusammenfassung",
  gp=gpar(fontsize=16, fontface="bold")
)
popViewport()

# Zeile 5: Blaues Rechteck + Summary Test
pushViewport(viewport(layout.pos.row=5, layout.pos.col=1, width=0.9))
grid.rect(gp=gpar(fill="#CCCCFF", col="black", lwd=2))
test_text <- paste(test_sum, collapse="\n")
grid.text(
  test_text,
  x=0.5, y=0.95,
  just=c("center","top"),
  gp=gpar(fontfamily="Courier", fontsize=10, col="black")
)
popViewport()
popViewport()

dev.off()