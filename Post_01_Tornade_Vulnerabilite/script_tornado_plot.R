# SCRIPT R : GENERATION D'UN GRAPHIQUE EN TORNADE POUR REGRESSION
# Auteur : Dr. Gazali SANNI, PhD | +229 01 97 21 19 48

# Ce script permet de tracer les coefficients d'un modèle de régression 
# (Logit, Probit, Bêta, etc.) sous forme de graphique en tornade.
# Les facteurs ayant un effet négatif (protecteur) apparaissent à gauche, 
# et les facteurs ayant un effet positif (aggravant) apparaissent à droite.

library(ggplot2)
library(dplyr)

# 1. SAISIE DES DONNEES
df_reg <- data.frame(
  Variable = c("Facteur de securite A", "Facteur de protection B", "Facteur protecteur C", 
               "Facteur aggravant D", "Facteur d'exposition E", "Facteur de risque F"),
  Coefficient = c(-0.520, -0.310, -0.050, 0.080, 0.290, 0.410),
  Effet = c("Protecteur (-)", "Protecteur (-)", "Protecteur (-)", 
            "Aggravant (+)", "Aggravant (+)", "Aggravant (+)")
)

# 2. TRAITEMENT ET TRI DES DONNEES
df_reg$Variable <- factor(df_reg$Variable, levels = df_reg$Variable[order(df_reg$Coefficient)])

# 3. DEFINITION DU THEME ESTHETIQUE
theme_tornade <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5, margin = margin(b = 15)),
      plot.caption = element_text(size = 10, face = "italic", color = "grey40"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#e0e0e0")
    )
}

# 4. CONSTRUCTION DU GRAPHIQUE
graph_tornade <- ggplot(df_reg, aes(x = Variable, y = Coefficient, fill = Effet)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(Coefficient, 3), 
                hjust = ifelse(Coefficient > 0, -0.2, 1.2)), 
            fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("Protecteur (-)" = "#388E3C", "Aggravant (+)" = "#D32F2F")) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  labs(
    title = "Impact des differents facteurs (Coefficients du modele)",
    x = "",
    y = "Coefficients de regression",
    fill = "Effet du facteur :",
    caption = "Conception de graphique par Dr. Gazali SANNI, PhD | +229 01 97 21 19 48"
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(-0.7, 0.7)) +
  theme_tornade()

# 5. AFFICHAGE ET EXPORTATION
print(graph_tornade)

ggsave("Post_01_Tornade_Vulnerabilite.png", plot = graph_tornade, width = 10, height = 5, dpi = 300)
print("Graphique genere et exporte avec succes sous le nom 'Post_01_Tornade_Vulnerabilite.png'.")
