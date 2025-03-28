# Neuer Prädiktor
combined_data$Spendentakt <- combined_data$`Anzahl Spenden` / combined_data$`Monate Letzte Spende`

# Benötigte Pakete
library(GGally)
library(reshape2)

# Pairs Plot
p_pairs <- ggpairs(
  combined_data[, c("Anzahl Spenden", "Monate Letzte Spende", "Gesamtvolumen", "Monate Erste Spende", "Spendentakt")]
)

# Korrelationsmatrix plot
cor_mat   <- cor(combined_data[, c("Anzahl Spenden", "Monate Letzte Spende", "Gesamtvolumen", "Monate Erste Spende", "Spendentakt")], use="complete.obs")
df_melt   <- melt(cor_mat)
p_corr    <- ggplot(df_melt, aes(Var1, Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(label=round(value,2))) +
  scale_fill_gradient2(limits=c(-1,1)) +
  theme_minimal() +
  labs(x="", y="", fill="Correlation")

# Titelbalken
title_bg   <- rectGrob(gp=gpar(fill="#7F3FBF", col=NA))
title_txt  <- textGrob(
  "Trainningsdaten - Beziehung der Variablen",
  gp = gpar(fontsize=28, col="white", fontface="bold")
)
title_panel<- grobTree(title_bg, title_txt)

# Kommentarbox
comment_bg   <- rectGrob(gp=gpar(fill=rgb(1,1,1,0.2), col=NA))
comment_text <- textGrob(
  "Erkenntnisse:\n\n• 'Anzahl Spenden' und 'Gesamtvolumen' haben Korrelation 1\n• Korrelation neuer Prädiktor 'Spendetakt' hoch zu 'Anzahl Spenden' & 'Gesamtvolumen'\n\nSchlussfolgerung:\n• 'Anzahl Spenden' beibehalten und 'Gesamtvolumen' weglassen",
  x=0.05, y=0.95, just=c("left","top"),
  gp=gpar(col="black", fontsize=12)
)
comment_panel<- grobTree(comment_bg, comment_text)

# Anordnung Folie 6
grid.arrange(
  title_panel, p_pairs, p_corr, comment_panel,
  layout_matrix = rbind(
    c(1,1,1),
    c(2,2,3),
    c(4,4,4)
  ),
  heights = c(0.8, 3, 2)
)
