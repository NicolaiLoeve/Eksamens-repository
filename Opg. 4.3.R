library(dkstat)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)

# --------------------------------------------------------
# OPGAVE 4.3 – De 15 grupper af forbrug, via API
# + KLARGØR KVARTALSDATA (forbrug15_q) TIL OPGAVE 4.4
# --------------------------------------------------------

# 1) De 15 forbrugsgrupper (som i NKHC021)
formaal_15 <- c(
  "Fødevarer mv.",
  "Drikkevarer og tobak mv.",
  "Beklædning og fodtøj",
  "Boligbenyttelse",
  "Elektricitet, fjernvarme og andet brændsel",
  "Boligudstyr, husholdningstjenester mv.",
  "Medicin, lægeudgifter o.l.",
  "Køb af køretøjer",
  "Drift af køretøjer og transporttjenester",
  "Information og kommunikation",
  "Fritid, sport og kultur",
  "Undervisning",
  "Restauranter og hoteller",
  "Forsikring og finansielle tjenester",
  "Andre varer og tjenester"
)

# 2) Hent metadata
forbrug_meta <- dst_meta("NKHC021", lang = "da")

# 3) Definér filtre til API-kald
forbrug_filters <- list(
  FORMAAAL  = formaal_15,
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON     = "Sæsonkorrigeret",
  TID       = "*"        # alle kvartaler
)

# 4) Hent data via API (kvartalsdata)
forbrug_raw <- dst_get_data(
  table = "NKHC021",
  query = forbrug_filters,
  lang  = "da",
  meta  = forbrug_meta
)

# --------------------------------------------------------
# A) KVARTALSDATA TIL OPGAVE 4.4  (forbrug15_q)
# --------------------------------------------------------



forbrug15_q <- forbrug_raw %>%
  mutate(
    Year    = as.integer(substr(TID, 1, 4)),
    # Træk kvartalet ud som sidste ciffer i TID (1–4)
    Quarter = as.integer(str_extract(TID, "[1-4]$")),
    KVARTAL = paste0(Year, "K", Quarter)
  ) %>%
  # Behold kun rigtige kvartaler (1–4)
  filter(!is.na(Quarter), Quarter %in% 1:4) %>%
  # Saml evt. flere observationer pr. kvartal/gruppe til én værdi
  group_by(KVARTAL, FORMAAAL) %>%
  summarise(
    value = mean(value, na.rm = TRUE),   # eller sum(value, na.rm = TRUE) hvis du vil summere
    .groups = "drop"
  ) %>%
  # Gør data brede: én kolonne pr. forbrugsgruppe
  pivot_wider(
    names_from  = FORMAAAL,
    values_from = value
  ) %>%
  arrange(KVARTAL) %>%
  # Rens kolonnenavne: fjern koder som "CPD "
  rename_with(
    ~ gsub("^[A-Z]{3} ", "", .x),
    -KVARTAL
  )
# Nu har du: forbrug15_q  (KVARTAL + 15 forbrugsgrupper, kvartalsvis)


# --------------------------------------------------------
# B) ÅRSDATA TIL OPGAVE 4.3  (df_long)
# --------------------------------------------------------

# 5) Lav år og læg kvartalerne sammen til årligt "forbrug"
df_long <- forbrug_raw %>%
  mutate(
    År = as.integer(substr(TID, 1, 4))
  ) %>%
  filter(År <= 2024) %>%  # opgave til og med 2024
  group_by(År, Gruppe = FORMAAAL) %>%
  summarise(
    Forbrug = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# Fjern koderne (CPD, CPM osv.) fra gruppenavnene
df_long$Gruppe <- gsub("^[A-Z]{3} ", "", df_long$Gruppe)

# -------------------------------
# SPØRGSMÅL 1: mest forbrug i 2024
# -------------------------------
# Filtrer til 2024 og sorter efter forbrug
df_2024 <- df_long %>%
  filter(År == 2024) %>%
  arrange(desc(Forbrug))

# Søjlediagram over forbruget i 2024
ggplot(df_2024, aes(x = reorder(Gruppe, Forbrug), y = Forbrug)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +   # <-- VIGTIGT: normale talformater
  labs(
    title = "Boligbenyttelse var den klart største forbrugsgruppe i 2024",
    subtitle = "Kvartaler lagt sammen til år, 2020-priser (kædede værdier)",
    x = "Forbrugsgruppe",
    y = "Samlet forbrug i 2024",
    caption = "Kilde: Danmarks Statistik, NKHC021"
  ) +
  theme_minimal(base_size = 13)

# -------------------------------
# SPØRGSMÅL 2: største stigning 2020–2024
# -------------------------------
årstart <- 2020
årslut  <- 2024

ændring_pct <- df_long %>%
  filter(År %in% c(årstart, årslut)) %>%
  pivot_wider(
    names_from  = År,
    values_from = Forbrug
  ) %>%
  mutate(
    forskel     = .data[[as.character(årslut)]] - .data[[as.character(årstart)]],
    forskel_pct = (forskel / .data[[as.character(årstart)]]) * 100
  ) %>%
  arrange(desc(forskel))

ændring_pct

# Visualisering af absolut ændring 2020–2024
ggplot(ændring_pct, aes(x = reorder(Gruppe, forskel), y = forskel)) +
  geom_col() +
  coord_flip() +
  labs(
    title    = "Ændring i husholdningernes forbrug efter formål",
    subtitle = paste("Fra", årstart, "til", årslut, " (2020-priser, kædede værdier)"),
    x        = "Forbrugsgruppe",
    y        = "Ændring (samme enhed som NKHC021)",
    caption  = "Kilde: Danmarks Statistik, NKHC021"
  ) +
  theme_minimal(base_size = 13)

