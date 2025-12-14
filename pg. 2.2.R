##########################################################
# OPGAVE 2.2 – DE 3 VIGTIGSTE SPØRGSMÅL (PCA, PC1)
# Vi finder de spørgsmål, der vægter højest på første
# hovedkomponent (PC1) fra pca_fit.
##########################################################

library(dplyr)

##########################################################
# 1. Træk vægte på PC1 ud for alle F-spørgsmål
##########################################################

# pca_fit$rotation:
#   - rækker = F-spørgsmål (F2–F13)
#   - kolonner = PC1, PC2 osv
vægt_pc1 <- pca_fit$rotation[, "PC1"]

# Lav en tabel med:
#   - F-kode
#   - vægt på PC1
#   - absolut værdi af vægten 
pc1_tabel <- tibble(
  Fkode                 = names(vægt_pc1),
  Vægt_PC1              = as.numeric(vægt_pc1),
  Absolut_vægt_vigtighed = abs(Vægt_PC1)
)

##########################################################
# 2. Find de 3 spørgsmål med størst (absolut) vægt
##########################################################

top3_pc1 <- pc1_tabel %>%
  arrange(desc(Absolut_vægt_vigtighed)) %>%  # højest vægt først
  slice(1:3)

top3_pc1   # kort tjek i konsollen

##########################################################
# 3. Tilføj spørgsmåls-tekst fra ftidata
##########################################################

# ftidata indeholder fuld spørgsmålstekst i INDIKATOR.
# Vi laver en lille oversigt med F-kode + tekst.
spg_oversigt <- ftidata %>%
  distinct(Fkode = extract_fkode(INDIKATOR), Spørgsmål = INDIKATOR)

# Join top-3 vægte med den fulde tekst
top3_pc1_med_tekst <- top3_pc1 %>%
  left_join(spg_oversigt, by = "Fkode") %>%
  # afrund vægte så de er pæne i tabeller
  mutate(
    Vægt_PC1              = round(Vægt_PC1, 3),
    Absolut_vægt_vigtighed = round(Absolut_vægt_vigtighed, 3)
  ) %>%
  arrange(desc(Absolut_vægt_vigtighed))

top3_pc1_med_tekst