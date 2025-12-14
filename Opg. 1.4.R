library(dplyr)
library(tibble)

##########################################################
# 1. Vælg kvartaler der skal forudsiges
##########################################################

mål_kvartaler <- c("2025K4")

##########################################################
# 2. BYG INDEKS FOR DEN BEDSTE INDIKATOR
##########################################################

# Hent de variable (F-koder), der indgår i den bedste kombination
best_vars <- strsplit(bedste$spg_kombi, "\\+")[[1]]

# Byg indeks for alle kvartaler
fti_best <- fti_all_q_wide %>%
  mutate(
    indeks_bedste = rowMeans(dplyr::across(all_of(best_vars)), na.rm = TRUE)
  )

# Hent indeksværdier i de kvartaler vi vil forudsige
best_K <- fti_best %>%
  filter(KVARTAL %in% mål_kvartaler) %>%
  select(KVARTAL, indeks_bedste)

##########################################################
# 3. Hent DI-indikator i de samme kvartaler
##########################################################

di_K <- ftidiq_wide %>%
  filter(KVARTAL %in% mål_kvartaler) %>%
  select(KVARTAL, Forbrugertillidsindikator)

##########################################################
# 4. ESTIMÉR MODELLER (SAMME TYPE SOM I DIT SCRIPT)
##########################################################

## 4.1 DI-modeller: realvækst og forbrug
model_DI_real <- lm(realvækst ~ Forbrugertillidsindikator, data = ftidiq_wide)
model_DI_forb <- lm(Forbrug   ~ Forbrugertillidsindikator, data = ftidiq_wide)

## 4.2 NY indikator-modeller: realvækst og forbrug
model_BEST_real <- lm(realvækst ~ indeks_bedste, data = fti_best)
model_BEST_forb <- lm(Forbrug   ~ indeks_bedste, data = fti_best)

##########################################################
# 5. FORUDSIG REALVÆKST OG FORBRUG I 2025K3 OG 2025K4
##########################################################

## 5.1 DI – prognose
pred_DI <- di_K %>%
  mutate(
    Forudset_realvækst_pct  = predict(model_DI_real,
                                      newdata = data.frame(
                                        Forbrugertillidsindikator = Forbrugertillidsindikator
                                      )),
    Forudset_forbrug_niveau = predict(model_DI_forb,
                                      newdata = data.frame(
                                        Forbrugertillidsindikator = Forbrugertillidsindikator
                                      ))
  ) %>%
  mutate(Model = "DI-indikator") %>%
  select(Model, KVARTAL,
         Forudset_realvækst_pct, Forudset_forbrug_niveau)

## 5.2 BEDSTE indikator – prognose
pred_BEST <- best_K %>%
  mutate(
    Forudset_realvækst_pct  = predict(model_BEST_real,
                                      newdata = data.frame(
                                        indeks_bedste = indeks_bedste
                                      )),
    Forudset_forbrug_niveau = predict(model_BEST_forb,
                                      newdata = data.frame(
                                        indeks_bedste = indeks_bedste
                                      ))
  ) %>%
  mutate(Model = "Bedste indikator") %>%
  select(Model, KVARTAL,
         Forudset_realvækst_pct, Forudset_forbrug_niveau)

##########################################################
# 6. SAMLET OVERSIGT
##########################################################

forudsigelser_2025 <- bind_rows(pred_DI, pred_BEST) %>%
  arrange(KVARTAL, Model)

forudsigelser_2025
