##########################################################
# PAKKER
##########################################################

# Kun første gang:
# install.packages(
#   "dkstat",
#   repos = c(
#     ropengov = "https://ropengov.r-universe.dev",
#     getOption("repos")
#   )
# )
# install.packages("devtools")
# devtools::install_github("rOpenGov/dkstat")

library(dkstat)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

##########################################################
# 0. PARAMETRE FOR PERIODE
##########################################################
# Ændr disse to linjer til eksamen, hvis de spørger efter
# en anden periode – resten af koden kører uændret.

start_år <- 1996
slut_år  <- 2025


##########################################################
# 1. FORBRUG (NKN1)
##########################################################

# Metadata til API-kald
forbrug_meta_filters <- list(
  TRANSAKT  = "P.31 Privatforbrug",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON     = "Sæsonkorrigeret",
  TID       = "*"   # henter hele serien – vi filtrerer efter år bagefter
)

# Hent forbrugsdata
forbrugdata <- dst_get_data(
  table = "NKN1",
  query = forbrug_meta_filters,
  lang  = "da"
)

# Omdan til kvartalsserie og filtrér på år
df_forbrug <- forbrugdata %>%
  mutate(
    # "1996K1" eller "1996M01" -> dato. FORV1/NKN1 bruger ofte M, men vi
    # sikrer os ved at trække år+måned ud.
    TID_DATE = as.Date(paste0(substr(TID, 1, 4), "-",
                              substr(TID, 6, 7), "-01")),
    Year     = year(TID_DATE),
    Quarter  = quarter(TID_DATE),
    KVARTAL  = paste0(Year, "K", Quarter)
  ) %>%
  filter(Year >= start_år, Year <= slut_år) %>%
  select(KVARTAL, Year, Quarter, Forbrug = value)


##########################################################
# 2. FORBRUGERTILLID (FORV1)
##########################################################

fti_meta_filters <- list(
  INDIKATOR = c(
    "Familiens økonomiske situation i dag, sammenlignet med for et år siden",
    "Familiens økonomiske  situation om et år, sammenlignet med i dag",
    "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
    "Danmarks økonomiske situation om et år, sammenlignet med i dag",
    "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
    "Priser i dag, sammenlignet med for et år siden",
    "Priser om et år, sammenlignet med i dag",
    "Arbejdsløsheden om et år, sammenlignet med i dag",
    "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.",
    "Anser det som fornuftigt at spare op i den nuværende økonomiske situation",
    "Regner med at kunne spare op i de kommende 12 måneder",
    "Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener"
  ),
  TID = "*"
)

ftidata <- dst_get_data(
  table = "FORV1",
  query = fti_meta_filters,
  lang  = "da"
)

# Omdan til kvartal og filtrér på år
df_spg <- ftidata %>%
  arrange(TID) %>%
  mutate(
    Year    = year(TID),
    Quarter = quarter(TID),
    KVARTAL = paste0(Year, "K", Quarter)
  ) %>%
  filter(Year >= start_år, Year <= slut_år)


##########################################################
# 3. OPDELING I DI- OG DST-INDIKATORER
##########################################################

ftidi_koder  <- c("F2", "F4", "F9", "F10")
ftidst_koder <- c("F2", "F3", "F4", "F5", "F9")

# Hjælpestreng: træk F-koden ud af INDIKATOR-teksten
extract_fkode <- function(x) {
  str_extract(x, "F[0-9]{1,2}")
}

ftidi <- df_spg %>%
  filter(extract_fkode(INDIKATOR) %in% ftidi_koder)

ftidst <- df_spg %>%
  filter(extract_fkode(INDIKATOR) %in% ftidst_koder)


##########################################################
# 4. AGGREGERING TIL KVARTAL OG BEREGNING AF INDIKATOR
##########################################################

# DI
ftidiq <- ftidi %>%
  group_by(KVARTAL, INDIKATOR) %>%
  summarise(
    value = mean(value, na.rm = TRUE),
    n_mdr = n(),
    .groups = "drop"
  ) %>%
  group_by(KVARTAL) %>%
  mutate(Forbrugertillidsindikator = mean(value, na.rm = TRUE)) %>%
  ungroup()

# DST
ftidstq <- ftidst %>%
  group_by(KVARTAL, INDIKATOR) %>%
  summarise(
    value = mean(value, na.rm = TRUE),
    n_mdr = n(),
    .groups = "drop"
  ) %>%
  group_by(KVARTAL) %>%
  mutate(Forbrugertillidsindikator = mean(value, na.rm = TRUE)) %>%
  ungroup()


##########################################################
# 5. WIDE FORMAT (ÉN RÆKKE PR. KVARTAL)
##########################################################

ftidiq_wide <- ftidiq %>%
  select(KVARTAL, INDIKATOR, value, Forbrugertillidsindikator) %>%
  pivot_wider(names_from = INDIKATOR, values_from = value) %>%
  distinct()

ftidstq_wide <- ftidstq %>%
  select(KVARTAL, INDIKATOR, value, Forbrugertillidsindikator) %>%
  pivot_wider(names_from = INDIKATOR, values_from = value) %>%
  distinct()


##########################################################
# 6. JOIN MED FORBRUG OG BEREGNING AF REALVÆKST
##########################################################

# DST
ftidstq_wide <- ftidstq_wide %>%
  left_join(df_forbrug, by = "KVARTAL") %>%
  arrange(KVARTAL) %>%
  mutate(
    # årlig realvækst: sammenlign med samme kvartal året før (lag 4)
    realvækst = (Forbrug / lag(Forbrug, 4) - 1) * 100
  )

# DI
ftidiq_wide <- ftidiq_wide %>%
  left_join(df_forbrug, by = "KVARTAL") %>%
  arrange(KVARTAL) %>%
  mutate(
    realvækst = (Forbrug / lag(Forbrug, 4) - 1) * 100
  )


##########################################################
# 7. GEM SOM CSV
##########################################################

write.csv(ftidstq_wide, "ftidstq_wide.csv", row.names = FALSE)
write.csv(ftidiq_wide, "ftidiq_wide.csv", row.names = FALSE)
write.csv(df_forbrug,   "df_forbrug.csv",   row.names = FALSE)