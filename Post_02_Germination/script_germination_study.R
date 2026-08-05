# SCRIPT R : ANALYSE ET VISUALISATION D'UN ESSAI DE GERMINATION
# Auteur : Dr. Gazali SANNI, PhD | +229 01 97 21 19 48

# Ce script calcule les indices classiques de germination à partir de données 
# de suivi quotidien (sur 30 jours) et génère des graphiques professionnels.
# Il intègre une simulation de données pour être directement opérationnel.

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# 1. GENERATION DE DONNEES FICTIVES (SIMULATION)
set.seed(123)

traitements <- c("Ageratum", "Chromolaena", "Eau distillee", "Eau_chaude", "Hyptis")
concentrations <- c("0", "50", "100")
temps_trempage <- c("0.16", "12", "24")
replications <- c("R1", "R2", "R3")

df_sim <- expand.grid(
  Specie = "Espece Modele",
  `Weeds/Solutions` = traitements,
  Concentration = concentrations,
  Soaking_time = temps_trempage,
  Replications = replications
)

jours <- paste0("D", 1:30, "AS")
for(j in jours) {
  df_sim[[j]] <- sample(c(0, 1), nrow(df_sim), replace = TRUE, prob = c(0.9, 0.1))
}

# 2. CALCUL DES PARAMETRES DE GERMINATION
df_sim$Total <- rowSums(df_sim[, jours], na.rm = TRUE)

N <- 5
df_sim$GP <- (df_sim$Total / N) * 100

df_sim$GI <- apply(df_sim[, jours], 1, function(x) {
  sum(x / (1:30), na.rm = TRUE)
})

df_sim$MGT <- apply(df_sim[, jours], 1, function(x) {
  total <- sum(x, na.rm = TRUE)
  if(total == 0) return(NA)
  sum(x * (1:30), na.rm = TRUE) / total
})

df_sim$GR <- 1 / df_sim$MGT

# 3. SYNTHESE DES DONNEES PAR TRAITEMENT
tableau_germination <- df_sim %>%
  group_by(Specie, `Weeds/Solutions`, Concentration, Soaking_time) %>%
  summarise(
    n = n(),
    GP_moy = round(mean(GP, na.rm = TRUE), 2),
    GI_moy = round(mean(GI, na.rm = TRUE), 2),
    MGT_moy = round(mean(MGT, na.rm = TRUE), 2),
    GR_moy = round(mean(GR, na.rm = TRUE), 2),
    .groups = "drop"
  )

tableau_germination$Concentration <- factor(tableau_germination$Concentration, levels = c("0", "50", "100"))
tableau_germination$Soaking_time <- factor(tableau_germination$Soaking_time, levels = c("0.16", "12", "24"))

# 4. REPRESENTATION GRAPHIQUE (FORMAT PUBLICATION ACADEMIQUE)
graph_germination <- ggplot(tableau_germination, 
                            aes(x = `Weeds/Solutions`, 
                                y = GP_moy, 
                                fill = Concentration)) +
  geom_col(position = position_dodge(0.8), 
           colour = "black", 
           linewidth = 0.3) +
  facet_wrap(~Soaking_time) +
  scale_fill_viridis_d(option = "D") +
  labs(
    title = "Effet des extraits vegetaux sur le taux de germination",
    x = "Traitement",
    y = "Germination (%)",
    fill = "Concentration",
    caption = "Dr. Gazali SANNI, PhD | +229 01 97 21 19 48"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(color = "black", fill = "white", linewidth = 0.8),
    strip.text = element_text(face = "bold")
  )

print(graph_germination)

ggsave("Post_02_Germination.png", graph_germination, width = 10, height = 6, dpi = 300)
print("Traitement termine. Graphique exporte sous le nom 'Post_02_Germination.png'.")
