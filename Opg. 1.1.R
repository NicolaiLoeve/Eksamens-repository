##########################################################
# PAKKER (bruges i hele scriptet)
##########################################################

library(dkstat)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(tibble)    # til tibble()
library(ggplot2)   # til plot af fordeling af kombinationer


##########################################################
# 8. KOMBINATIONER AF DE 12 SPØRGSMÅL (DST-UNDERSØGELSEN)
##########################################################
# Formål:
#  - Få alle 12 spørgsmål (F1–F12) i wide-format pr. kvartal
#  - Lave ALLE ikke-tomme kombinationer af spørgsmålene
#  - Lave en oversigt over kombinationer
#  - Se fordelingen af, hvor mange spørgsmål der indgår i kombinationerne
#  - (Eksempel) beregne et gennemsnitsindeks for hver kombination
#  - Lave et lille plot af fordelingen


##########################################################
# 8.1 Kvartalsdata i wide-format med alle 12 F-koder (F1–F12)
##########################################################

fti_all_q_wide <- df_spg %>%
  # Tilføj variabel med F-kode (F1, F2, ..., F12)
  mutate(Fkode = extract_fkode(INDIKATOR)) %>%
  # Gennemsnit pr. kvartal og F-kode (fra månedsdata til kvartal)
  group_by(KVARTAL, Fkode) %>%
  summarise(
    value = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Wide-format: én kolonne pr. spørgsmål (F1, F2, ..., F12)
  pivot_wider(
    names_from  = Fkode,
    values_from = value
  ) %>%
  # Join med forbruget
  left_join(df_forbrug, by = "KVARTAL") %>%
  arrange(KVARTAL) %>%
  # Beregn realvækst i forbrug (år-til-år, lag 4 kvartaler)
  mutate(
    realvækst = (Forbrug / lag(Forbrug, 4) - 1) * 100
  )


##########################################################
# 8.2 Vektor med spørgsmål (F1–F12)
##########################################################
# Vi finder alle kolonnenavne, der matcher F + tal, fx "F1", "F2", ..., "F12"

spg_vektor <- sort(grep("^F[0-9]+$", names(fti_all_q_wide), value = TRUE))

# Tjek (valgfrit): hvor mange spørgsmål har vi?
length(spg_vektor)   # bør være 12
spg_vektor           # fx "F1" "F2" ... "F12"


##########################################################
# 8.3 Funktion: lav alle ikke-tomme kombinationer af spørgsmål
##########################################################
# Idé: for k = 1, 2, ..., 12 laver vi alle kombinationer af længde k
# og samler dem i én stor liste.

lav_kombinationer <- function(vars) {
  # vars er fx c("F1", "F2", ..., "F12")
  out <- lapply(seq_along(vars), function(k) {
    combn(vars, k, simplify = FALSE)   # alle kombinationer med længde k
  })
  # Unlist -> én flad liste med alle kombinationer
  unlist(out, recursive = FALSE)
}

alle_kombinationer <- lav_kombinationer(spg_vektor)

# Kort tjek (valgfrit): antal kombinationer (2^12 - 1 = 4095)
length(alle_kombinationer)


##########################################################
# 8.4 Oversigtstabel: én række pr. kombination
##########################################################
# Vi laver en tabel der beskriver hver kombination:
#  - komb_id   : løbenummer for kombinationen
#  - n_spg     : hvor mange spørgsmål indgår i kombinationen
#  - spg_kombi : tekststreng, fx "F2+F4+F9"

kombinationsoversigt <- tibble(
  komb_id   = seq_along(alle_kombinationer),
  n_spg     = vapply(alle_kombinationer, length, integer(1)),
  spg_kombi = vapply(
    alle_kombinationer,
    function(x) paste(x, collapse = "+"),
    character(1)
  )
)

# kig evt. på de første rækker:
# head(kombinationsoversigt)


##########################################################
# 8.5 Fordeling af kombinationsstørrelser
#     (hvor mange kombinationer har 1, 2, ..., 12 spørgsmål)
##########################################################

fordeling_kombinationer <- kombinationsoversigt %>%
  count(n_spg, name = "antal_kombinationer") %>%
  arrange(n_spg)

fordeling_kombinationer
# Her kan man fx se:
#  - hvor mange kombinationer består af 1 spørgsmål
#  - hvor mange af 2 spørgsmål
#  - ...
#  - hvor mange af 12 spørgsmål


##########################################################
# 8.6 Eksempel: beregn et indeks (gennemsnit) for hver kombination
#     og bind alt sammen til ét stort datasæt
##########################################################
# For hver kombination:
#  - beregner vi indeks = gennemsnit af de F-variabler,
#    der indgår i den pågældende kombination.

liste_med_data <- lapply(seq_along(alle_kombinationer), function(i) {
  vars_i <- alle_kombinationer[[i]]   # fx c("F2", "F4", "F9")
  
  fti_all_q_wide %>%
    mutate(
      komb_id   = i,
      n_spg     = length(vars_i),
      spg_kombi = paste(vars_i, collapse = "+"),
      indeks    = rowMeans(across(all_of(vars_i)), na.rm = TRUE)
    )
})

fti_kombi_alle <- bind_rows(liste_med_data)
# fti_kombi_alle indeholder:
#  - én række pr. kvartal pr. kombination
#  - indeks = gennemsnit af de valgte F-spørgsmål
#  - plus Forbrug, realvækst mv. fra fti_all_q_wide


##########################################################
# 8.7 Summary-tabel for de 12 spørgsmål (valgfrit)
##########################################################
# Hurtig oversigt over niveauet for hvert spørgsmål (F1–F12)

summary_tabel <- df_spg %>%
  mutate(Fkode = extract_fkode(INDIKATOR)) %>%
  group_by(Fkode) %>%
  summarise(
    Min     = min(value, na.rm = TRUE),
    Q1      = quantile(value, 0.25, na.rm = TRUE),
    Median  = median(value, na.rm = TRUE),
    Mean    = mean(value, na.rm = TRUE),
    Q3      = quantile(value, 0.75, na.rm = TRUE),
    Max     = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Fkode)

summary_tabel


##########################################################
# 8.8 Plot: fordeling af kombinationsstørrelser
##########################################################
# Lille søjlediagram der viser, hvor mange kombinationer
# der har hhv. 1, 2, ..., 12 spørgsmål.

ggplot(fordeling_kombinationer, aes(x = n_spg, y = antal_kombinationer)) +
  geom_col() +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Fordeling af kombinationer efter antal spørgsmål",
    x     = "Antal spørgsmål i kombinationen",
    y     = "Antal kombinationer"
  ) +
  theme_minimal()