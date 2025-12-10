library(dplyr)
library(tibble)
library(ggplot2)

############################################################
# 0. KLARGØR DI-DATA TIL LOGITMODEL
############################################################

# Tjek hvilke variabelnavne der ligger i datasættet
names(ftidiq_wide)

# Omdøb indikatorvariablerne til korte, klare navne
# (Meget lettere at arbejde med i modelformuleringen)
ftidiq_clean <- ftidiq_wide %>%
  rename(
    F2  = `F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden`,
    F4  = `F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden`,
    F9  = `F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`,
    F10 = `F10 Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`
  )

############################################################
# 1. LAV AFHÆNGIG VARIABEL: OM REALVÆKSTEN STIGER
############################################################

df_logit <- ftidiq_clean %>%
  # Vælg analysetidsrum
  filter(KVARTAL >= "1998K1", KVARTAL <= "2025K3") %>%
  arrange(KVARTAL) %>%
  
  # Beregn ændringen i realvækst
  mutate(
    delta_realvækst = realvækst - lag(realvækst),
    
    # Dummy-variabel: 1 = realvæksten stiger, 0 = falder/uændret
    stiger = ifelse(delta_realvækst > 0, 1, 0)
  ) %>%
  
  # Første kvartal har ingen "lag", så fjern NA
  filter(!is.na(stiger))

############################################################
# 2. ESTIMÉR LOGIT-MODEL
############################################################

model_logit <- glm(
  stiger ~ F2 + F4 + F9 + F10,
  data   = df_logit,
  family = binomial(link = "logit")
)

summary(model_logit)

############################################################
# 3. FORUDSIG OM FORBRUGSVÆKSTEN STIGER I 2025K3
############################################################

# Hent indikatorværdierne for 2025K3
di_2025K3 <- ftidiq_clean %>%
  filter(KVARTAL == "2025K3") %>%
  select(F2, F4, F9, F10)

# Modellens sandsynlighed for stigning
pred_2025K3 <- predict(model_logit, 
                       newdata = di_2025K3, 
                       type = "response")

# Fortolkning ved 50%-grænse
retning_2025K3 <- ifelse(pred_2025K3 > 0.5, "Stiger", "Falder")

pred_2025K3
retning_2025K3

# Visualisering af forudsigelse

df_pred_2025K3 <- tibble(
  kategori      = c("Stiger", "Falder/ikke stiger"),
  sandsynlighed = c(pred_2025K3, 1 - pred_2025K3)
)

ggplot(df_pred_2025K3, aes(x = kategori, y = sandsynlighed, fill = kategori)) +
  geom_col() +
  geom_text(aes(label = scales::percent(sandsynlighed, accuracy = 0.1)),
            vjust = -0.3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    title    = "Modellens prognose for 2025K3",
    subtitle = "Større sandsynlighed for stigning end for fald",
    x        = "",
    y        = "Sandsynlighed",
    caption  = "Logitmodel: stiger ~ F2 + F4 + F9 + F10"
  ) +
  theme_minimal()
