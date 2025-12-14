############################################################
# OPGAVE 2.3 – PCA-PROGNOSE FOR 2025K4 UDEN NA
############################################################

library(dplyr)
library(ggplot2)

mål_kvartal <- "2025K4"

############################################################
# 1. Definér F-variabler og byg datasæt til PCA (som i 2.1)
############################################################

# Alle F-koder (F2–F13) som bruges i PCA
spg_koder <- sort(grep("^F[0-9]+$", names(fti_all_q_wide), value = TRUE))
spg_koder    # tjek: bør være 12 stk.

# Træningsdatasæt til PCA og regression:
# - kun kvartaler med observeret realvækst
# - ingen NA i F-variablerne
pca_data <- fti_all_q_wide %>%
  filter(!is.na(realvækst)) %>%
  filter(dplyr::if_all(all_of(spg_koder), ~ !is.na(.))) %>%
  select(realvækst, all_of(spg_koder))

# X-matrix til PCA
X <- pca_data %>%
  select(all_of(spg_koder))

# PCA (samme som i opgave 2.1)
pca_fit <- prcomp(
  X,
  center = TRUE,
  scale. = TRUE
)

# PCA-scorer (PC1–PC12) til regressionsdata
pca_scores <- as.data.frame(pca_fit$x)

data_pca_reg <- pca_data %>%
  bind_cols(pca_scores)

# PCA-regression med alle 12 komponenter
model_pca <- lm(
  realvækst ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 +
    PC7 + PC8 + PC9 + PC10 + PC11 + PC12,
  data = data_pca_reg
)

summary(model_pca)   # valgfrit: kig på R² mv.

############################################################
# 2. Forbered 2025K4: udfyld manglende F11 med gennemsnit
############################################################

# Træk F2–F13 for 2025K4
X_2025K4 <- fti_all_q_wide %>%
  filter(KVARTAL == mål_kvartal) %>%
  select(all_of(spg_koder))

X_2025K4   # tjek: én række med F2–F13, F11 er NA

# Beregn gennemsnit for hver F-variabel i træningsdata
kolonne_middel <- pca_data %>%
  summarise(across(all_of(spg_koder), ~ mean(.x, na.rm = TRUE)))

# Erstat eventuelle NA i 2025K4 med kolonne-gennemsnit
X_2025K4_imputeret <- X_2025K4 %>%
  mutate(
    across(
      all_of(spg_koder),
      ~ ifelse(
        is.na(.x),
        kolonne_middel[[cur_column()]],  # indsæt middelværdi for den kolonne
        .x
      )
    )
  )

X_2025K4_imputeret   # nu bør der ikke være NA i F11

############################################################
# 3. Beregn PCA-scorer og forudsig realvækst for 2025K4
############################################################

# PCA-scorer for 2025K4 (på de imputerede værdier)
pc_scores_2025K4 <- predict(
  pca_fit,
  newdata = X_2025K4_imputeret
)

# Forudsagt realvækst ud fra PCA-regressionsmodellen
realvækst_hat_2025K4 <- predict(
  model_pca,
  newdata = as.data.frame(pc_scores_2025K4)
)

# Læg det i en lille, pæn tabel
pca_prognose_2025K4 <- tibble(
  KVARTAL                     = mål_kvartal,
  PCA_forudsagt_realvækst_pct = round(realvækst_hat_2025K4, 2)
)

pca_prognose_2025K4


############################################################
# 1. Saml historik + PCA-prognose i ét datasæt
############################################################

# Antag at pca_prognose_2025K4 allerede er beregnet:
# pca_prognose_2025K4$PCA_forudsagt_realvækst_pct == 0.29

pca_plot_data <- fti_all_q_wide %>%
  # kun historiske observationer med kendt realvækst
  filter(!is.na(realvækst)) %>%
  select(KVARTAL, realvækst) %>%
  mutate(type = "Historik") %>%
  
  # læg PCA-prognosen for 2025K4 oveni
  bind_rows(
    tibble(
      KVARTAL    = pca_prognose_2025K4$KVARTAL,
      realvækst  = pca_prognose_2025K4$PCA_forudsagt_realvækst_pct,
      type       = "Prognose"
    )
  ) %>%
  # konverter KVARTAL til dato for pæn x-akse
  mutate(
    KVARTAL_DATE = kvartal_to_date(KVARTAL)
  )

# hvis du kun vil vise de seneste fx 10 år:
pca_plot_data_recent <- pca_plot_data %>%
  filter(KVARTAL_DATE >= max(KVARTAL_DATE) - lubridate::years(10))

############################################################
# 2. Plot: søjler for realvækst (historik + PCA-prognose)
############################################################

ggplot(pca_plot_data_recent,
       aes(x = KVARTAL_DATE, y = realvækst, fill = type)) +
  geom_col() +
  scale_fill_manual(values = c("Historik" = "grey70", "Prognose" = "red")) +
  labs(
    title    = "Årlig realvækst i husholdningernes forbrug\nHistorik og PCA-prognose for 2025K4",
    x        = "Kvartal",
    y        = "Realvækst i forbrug (pct.)",
    fill     = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold")
  )
