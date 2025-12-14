#####################################
# ML: Forbrugertillid -> julehandel
#####################################

library(dplyr)
library(tidyr)
library(lubridate)

## 1) Forbrugertillid: ftidstq_wide -> kvartalsdata (År, kvartal, fti)

fti_raw <- ftidstq_wide

# Antagelse:
# - ftidstq_wide har en kolonne "KVARTAL" med fx "2015K1", "2015K2", ...
# - der findes mindst én numerisk kolonne med selve indikatoren

fti <- fti_raw %>%
  mutate(KVARTAL = as.character(KVARTAL)) %>%
  separate(KVARTAL, into = c("År", "Kvar"), sep = "K", remove = FALSE) %>%
  mutate(
    År = as.numeric(År),
    kvartal = paste0("Q", Kvar)
  )

# vælg første numeriske kolonne som forbrugertillidsindikator
num_cols <- fti %>% select(where(is.numeric))

if (ncol(num_cols) == 0) {
  stop("Fandt ingen numeriske kolonner i ftidstq_wide. Angiv selv indikator-kolonnen.")
}

fti <- fti %>%
  mutate(fti = num_cols[[1]]) %>%     # hvis du vil være helt præcis: rename(KOLONNAVN = fti)
  select(År, kvartal, fti)

# tag kun Q4 pr. år (forbrugertillid i julekvartalet)
fti_q4 <- fti %>%
  filter(kvartal == "Q4") %>%
  rename(year = År, fti_q4 = fti)


## 2) Detailhandel: DETA11 -> månedlige data -> julehandel (Q4) pr. år

# DETA11 antages at have:
# - første kolonne = branchekode/tekst (...1)
# - øvrige kolonner = måneder, fx "2015M01", "2015M02", ...

retail <- DETA11 %>%
  pivot_longer(
    cols = -...1,
    names_to = "Tid",
    values_to = "Omsætning"
  ) %>%
  mutate(
    År = as.numeric(substr(Tid, 1, 4)),
    Måned = as.numeric(substr(Tid, 6, 7)),
    Dato = as.Date(paste0(År, "-", Måned, "-01"))
  ) %>%
  select(Branche = ...1, Dato, År, Måned, Omsætning)

# vælg "Detailhandel i alt" (4700)
detail_total <- retail %>%
  filter(Branche == "4700 Detailhandel i alt")

# julehandel = sum af Q4 (oktober–december) pr. år
julehandel <- detail_total %>%
  filter(Måned %in% c(10, 11, 12)) %>%
  group_by(År) %>%
  summarise(
    juleoms = sum(Omsætning, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(År) %>%
  mutate(
    juleoms_lag = lag(juleoms),
    y = ifelse(juleoms > juleoms_lag, 1, 0)   # 1 = julehandel større end året før
  ) %>%
  filter(!is.na(juleoms_lag)) %>%            # fjern første år
  rename(year = År)


## 3) Samlet ML-datasæt: julehandel + forbrugertillid

data_ml <- julehandel %>%
  left_join(fti_q4, by = "year") %>%
  filter(!is.na(fti_q4))

# tjek hurtigt:
# View(data_ml)


## 4) Træn ML-model (logistisk regression)

# brug kun år, hvor vi har fuld julehandel (dvs. uden 2025)
data_train <- data_ml %>%
  filter(year <= 2024)

data_train$y <- factor(data_train$y, levels = c(0, 1))

ml_model <- glm(
  y ~ fti_q4,
  data   = data_train,
  family = binomial(link = "logit")
)

summary(ml_model)


## 5) Forudsigelse for 2025 (julehandel 2025 > 2024?)

fti_2025 <- fti_q4 %>%
  filter(year == 2025)

if (nrow(fti_2025) == 0) {
  stop("Ingen FTI-værdi for 2025Q4 i ftidstq_wide.")
}

p_2025 <- predict(ml_model, newdata = fti_2025, type = "response")
klasse_2025 <- ifelse(p_2025 > 0.5, 1, 0)

cat("Forudsigelse: julehandel 2025 vs. 2024\n")
cat(" - Sandsynlighed for, at 2025 > 2024 (y = 1):", round(100 * p_2025, 1), "%\n")
cat(" - Klassifikation (0 = ikke større, 1 = større):", klasse_2025, "\n")

#### Opg. 3.2

set.seed(123)

# brug kun år <= 2024 som før
data_all <- data_ml %>%
  filter(year <= 2024) %>%
  mutate(y = factor(y, levels = c(0, 1)))

# simpel 80/20-split
n <- nrow(data_all)
idx_train <- sample(seq_len(n), size = floor(0.8 * n))

train <- data_all[idx_train, ]
test  <- data_all[-idx_train, ]

# fit model kun på train
ml_model <- glm(
  y ~ fti_q4,
  data   = train,
  family = binomial(link = "logit")
)

summary(ml_model)  # giver dig koefficienter, p-værdier osv.
