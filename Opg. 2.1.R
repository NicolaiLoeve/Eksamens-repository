############################################################
# PCA-REGRESSION PÅ FORBRUGERTILLIDSSPØRGSMÅL (F2–F13)
# Y = realvækst, X = alle 12 F-spørgsmål
############################################################

library(dplyr)

############################################################
# 1. Vælg variabler: F2–F13 som X og realvækst som Y
############################################################

# Find alle kolonnenavne, der ligner "F" + tal (F2, F3, ..., F13)
spg_koder <- sort(grep("^F[0-9]+$", names(fti_all_q_wide), value = TRUE))

spg_koder         # tjek i konsollen: skal være 12 F-koder
length(spg_koder) # tjek: bør være 12

############################################################
# 2. Byg datasæt til PCA
#    - Kun kvartaler med OBSERVERET realvækst
#    - Kun kvartaler hvor ALLE 12 F-spørgsmål er udfyldt
############################################################

pca_data <- fti_all_q_wide %>%
  filter(!is.na(realvækst)) %>%                               # Y må ikke være NA
  filter(dplyr::if_all(all_of(spg_koder), ~ !is.na(.))) %>%   # alle X skal være udfyldt
  select(realvækst, all_of(spg_koder))                        # behold kun det, vi skal bruge

# Evt. hurtigt tjek:
# anyNA(pca_data)   # bør være FALSE
# nrow(pca_data)    # hvor mange kvartaler indgår i PCA?

############################################################
# 3. PCA på de 12 spørgsmål (X-delen)
############################################################

# X-matrix: kun F-variablerne
X <- pca_data %>%
  select(all_of(spg_koder))

# PCA med centrerede og skalerede variabler
pca_fit <- prcomp(
  X,
  center = TRUE,   # træk middelværdien fra
  scale. = TRUE    # skaler til samme varians
)

# Hvor meget forklarer hver komponent?
summary(pca_fit)      # Proportion of Variance, Cumulative Proportion

# Hvilke spørgsmål "laster" på hvilke komponenter?
pca_fit$rotation      # loadings for F2–F13 på PC1, PC2, ...

############################################################
# 4. PCA-scores (PC1, PC2, ..., PC12) og regressionsdata
############################################################

# PCA-scorerne er de nye "X-variabler" (ukorrelerede)
pca_scores <- as.data.frame(pca_fit$x)   # én kolonne pr. PC

# Bind scores sammen med realvækst
data_pca_reg <- pca_data %>%
  bind_cols(pca_scores)

# Tjek evt.:
# names(data_pca_reg)
# head(data_pca_reg)

############################################################
# 5. PCA-REGRESSION: realvækst ~ alle 12 hovedkomponenter
############################################################

pca_model_full <- lm(
  realvækst ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +
    PC7 + PC8 + PC9 + PC10 + PC11 + PC12,
  data = data_pca_reg
)

summary(pca_model_full)   # R², signifikans for PC'erne mv.

############################################################
# 6. Evt. enklere model: kun de første 3 komponenter
#    (de forklarer typisk langt det meste af variationen)
############################################################

pca_model_3 <- lm(
  realvækst ~ PC1 + PC2 + PC3,
  data = data_pca_reg
)

summary(pca_model_3)      # brugbar til sammenligning i rapporten