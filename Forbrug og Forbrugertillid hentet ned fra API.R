#Kun hvis ikke man har installeret det før

#install.packages(
  "dkstat",
  repos = c(
    ropengov = "https://ropengov.r-universe.dev",
    getOption("repos")
  )
)
# install.packages("devtools")
#devtools::install_github("rOpenGov/dkstat")

library(tidyr)
library(dkstat)
library(dplyr)
library(stringr)
library(lubridate)

##########################################################
# 1. FORBRUG (NKN1)
##########################################################

# Metadata
forbrug_meta_filters <- list(
  TRANSAKT  = "P.31 Privatforbrug",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON     = "Sæsonkorrigeret",
  TID       = "*"
)

# Hent forbrugsdata
forbrugdata <- dst_get_data(table = "NKN1", query = forbrug_meta_filters, lang = "da")

# Fjern de 24 første rækker + lav korrekt kvartalskode
df_forbrug <- forbrugdata %>%
  slice(-(1:24)) %>%
  mutate(
    # Lav "1996M01" om til en dato: "1996-01-01"
    TID_DATE = as.Date(paste0(substr(TID, 1, 4), "-", 
                              substr(TID, 6, 7), "-01")),
    
    Year    = year(TID_DATE),
    Quarter = quarter(TID_DATE),
    KVARTAL = paste0(Year, "K", Quarter)
  ) %>%
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

ftidata <- dst_get_data(table = "FORV1", query = fti_meta_filters, lang = "da")

# Sortér & fjern gamle rækker som i dit script
df_spg <- ftidata %>%
  arrange(TID) %>%
  slice(-(1:3060))

# Inddel i DI og DST
ftidi_koder  <- c("F2", "F4", "F9", "F10")
ftidst_koder <- c("F2", "F3", "F4", "F5", "F9")

ftidi <- df_spg %>%
  filter(str_extract(INDIKATOR, "F[0-9]{1,2}") %in% ftidi_koder)

ftidst <- df_spg %>%
  filter(str_extract(INDIKATOR, "F[0-9]{1,2}") %in% ftidst_koder)

##########################################################
# 3. AGGREGERING TIL KVARTAL + KVARTALSKODE
##########################################################

# DI
ftidiq <- ftidi %>%
  mutate(
    Year    = year(TID),
    Quarter = quarter(TID),
    KVARTAL = paste0(Year, "K", Quarter)
  ) %>%
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
  mutate(
    Year    = year(TID),
    Quarter = quarter(TID),
    KVARTAL = paste0(Year, "K", Quarter)
  ) %>%
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
# 4. WIDE FORMAT
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
# 5. JOIN MED FORBRUG + REALVÆKST
##########################################################

# DST
ftidstq_wide <- ftidstq_wide %>%
  left_join(df_forbrug, by = "KVARTAL") %>%
  arrange(KVARTAL) %>%
  mutate(
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
# 6. GEM CSV
##########################################################

write.csv(ftidstq_wide, "ftidstq_wide.csv", row.names = FALSE)
write.csv(ftidiq_wide, "ftidiq_wide.csv", row.names = FALSE)
write.csv(df_forbrug,   "df_forbrug.csv",   row.names = FALSE)
