############################################## opgave 3.1
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---------------------------------------------------------
# 1. Læs data
# ---------------------------------------------------------
virk <- read_excel("regnskaber_industri_transport_byg_5_25000_ansatte_anonym(in).xlsx")

# Fjern "Ved ikke"
virk <- virk %>% filter(laane_svar != "Ved ikke")

names(virk)

virk <- virk %>%
  filter(`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` != "Ved ikke")
table(virk$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)

virk <- virk %>%
  rename(laane_svar =
           `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`
  )

table(virk$laane_svar)


# ---------------------------------------------------------
# 2. Opret lånekategorier
# ---------------------------------------------------------

laan_svar <- virk[[1]]

virk$laane_kategori <- case_when(
  laan_svar %in% c("Gode", "Meget gode") ~ "Positiv",
  laan_svar == "Neutrale" ~ "Neutral",
  laan_svar %in% c("Dårlige", "Dårlig", "Meget dårlige") ~ "Negativ",
  TRUE ~ NA_character_
)

# ---------------------------------------------------------
# 3. Find kolonner automatisk
# ---------------------------------------------------------

solid_cols   <- grep("Soliditetsgrad", colnames(virk), value = TRUE)
afkast_cols  <- grep("Afkast", colnames(virk), value = TRUE)
balance_cols <- grep("Balance", colnames(virk), value = TRUE)


# FIX: vælg kun den tal-baserede kolonne (ikke "kilde")
ansatte_cols <- grep("Antal ansatte Cvr-nr\\.(?! kilde)", colnames(virk), value = TRUE, perl = TRUE)

branche_cols <- grep("Branchebetegnelse primær", colnames(virk), value = TRUE)


# ---------------------------------------------------------
# 4. Long-format (soliditet, afkast, balance)
# ---------------------------------------------------------

# Soliditet
soliditet_long <- virk %>%
  select(laane_kategori, all_of(solid_cols)) %>%
  pivot_longer(
    cols = all_of(solid_cols),
    names_to = "year",
    values_to = "soliditet"
  ) %>%
  mutate(
    year = substr(gsub("[^0-9]", "", year), 1, 4),
    soliditet = as.numeric(gsub(",", ".", soliditet))
  )

# Afkast
afkast_long <- virk %>%
  select(laane_kategori, all_of(afkast_cols)) %>%
  pivot_longer(
    cols = all_of(afkast_cols),
    names_to = "year",
    values_to = "afkast"
  ) %>%
  mutate(
    year = substr(gsub("[^0-9]", "", year), 1, 4),
    afkast = as.numeric(gsub(",", ".", afkast))
  )

# Balance
balance_long <- virk %>%
  select(laane_kategori, all_of(balance_cols)) %>%
  pivot_longer(
    cols = all_of(balance_cols),
    names_to = "year",
    values_to = "balance"
  ) %>%
  mutate(
    year = substr(gsub("[^0-9]", "", year), 1, 4),
    balance = as.numeric(gsub(",", ".", balance))
  )


# ---------------------------------------------------------
# 5. Antal ansatte (uden år)
# ---------------------------------------------------------

antal_ansatte_long <- virk %>%
  select(laane_kategori, all_of(ansatte_cols)) %>%
  rename(antal_ansatte = all_of(ansatte_cols)) %>%
  mutate(antal_ansatte = as.numeric(antal_ansatte))


# ---------------------------------------------------------
# 6. Branchebetegnelse primær
# ---------------------------------------------------------

branche_long <- virk %>%
  select(laane_kategori, all_of(branche_cols)) %>%
  pivot_longer(
    cols = all_of(branche_cols),
    names_to = "variable",
    values_to = "branche"
  ) %>%
  mutate(branche = as.character(branche))


# ---------------------------------------------------------
# 7. SOLIDITETSGRAD (KUN 2020–2016)
# ---------------------------------------------------------

plot_soliditet <- soliditet_long %>%
  filter(
    year %in% c("2020", "2019", "2018", "2017", "2016"),
    !is.na(laane_kategori)
  ) %>%
  group_by(year, laane_kategori) %>%
  summarise(mean_soliditet = mean(soliditet, na.rm = TRUE))

ggplot(plot_soliditet, aes(x = year, y = mean_soliditet, fill = laane_kategori)) +
  geom_col(position = "dodge", width = 0.7) +
  labs(
    title = "Soliditetsgrad fordelt på lånekategorie",
    x = "",
    y = "Gennemsnitlig soliditetsgrad (%)"
  ) +
  scale_fill_manual(
    values = c("Positiv" = "#009DE0", "Neutral" = "#666666", "Negativ" = "#BFBFBF"),
    labels = c(
      "Positiv" = "Positiv = Gode / Meget gode",
      "Neutral" = "Neutral",
      "Negativ" = "Negativ = Dårlige / Meget dårlige"
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))


# ---------------------------------------------------------
# 8. AFKAST (KUN 2020–2016)
# ---------------------------------------------------------

plot_afkast <- afkast_long %>%
  filter(
    year %in% c("2020", "2019", "2018", "2017", "2016"),
    !is.na(laane_kategori)
  ) %>%
  group_by(year, laane_kategori) %>%
  summarise(mean_afkast = mean(afkast, na.rm = TRUE))

ggplot(plot_afkast, aes(x = year, y = mean_afkast, fill = laane_kategori)) +
  geom_col(position = "dodge", width = 0.7) +
  labs(
    title = "Afkastningsgrad fordelt på lånekategorier",
    x = "",
    y = "Gennemsnitligt afkast (%)"
  ) +
  scale_fill_manual(
    values = c("Positiv" = "#009DE0", "Neutral" = "#666666", "Negativ" = "#BFBFBF")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))


# ---------------------------------------------------------
# 9. BALANCE (KUN 2020–2016) – log af gennemsnitlig balance
# ---------------------------------------------------------

plot_balance <- balance_long %>%
  filter(
    year %in% c("2020", "2019", "2018", "2017", "2016"),
    !is.na(laane_kategori)
  ) %>%
  group_by(year, laane_kategori) %>%
  summarise(
    mean_balance = mean(balance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_mean_balance = log(mean_balance + 1)   # her tager vi log EFTER gennemsnit
  )

ggplot(plot_balance,
       aes(x = year, y = log_mean_balance, fill = laane_kategori)) +
  geom_col(position = "dodge", width = 0.7) +
  labs(
    title = "Virksomheder med større balance har mere positive lånevurderinger",
    subtitle = "Virksomheder, der vurderer deres lånemuligheder positivt, har generelt større balance end andre",
    x = "",
    y = "log(balance)"
  ) +
  scale_fill_manual(
    values = c("Positiv" = "#009DE0", "Neutral" = "#666666", "Negativ" = "#BFBFBF")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))
