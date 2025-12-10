# PAKKER
##########################################################
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)

##########################################################
# 1. BASELINE-MODEL (FTI_t -> vækst_t)
##########################################################

# Datasæt til baseline-logit
df_logit <- ftidiq_clean %>%
  filter(KVARTAL >= "1998K1", KVARTAL <= "2025K4") %>%
  arrange(KVARTAL) %>%
  mutate(
    # Ændring i realvækst fra t-1 til t
    delta_realvækst = realvækst - lag(realvækst),
    # Dummy afhængig variabel: 1 = stiger, 0 = falder/uændret
    dummy_stiger    = ifelse(delta_realvækst > 0, 1, 0)
  ) %>%
  filter(!is.na(dummy_stiger))

# Baseline logitmodel
model_logit <- glm(
  dummy_stiger ~ F2 + F4 + F9 + F10,
  data   = df_logit,
  family = binomial(link = "logit")
)

# Forudsigelser og klassifikation til baseline
df_validation <- df_logit %>%
  mutate(
    pred_prob  = predict(model_logit, newdata = ., type = "response"),
    pred_class = ifelse(pred_prob > 0.5, 1, 0)
  )

# Konfusionsmatrix for baseline
tab_base <- table(
  Forudsagt = df_validation$pred_class,
  Faktisk   = df_validation$dummy_stiger
)

##########################################################
# 2. SCENARIE 1: LAG-MODEL (FTI_{t-1} -> vækst_t)
##########################################################

df_logit_lag <- ftidiq_clean %>%
  filter(KVARTAL >= "1998K1", KVARTAL <= "2025K4") %>%
  arrange(KVARTAL) %>%
  mutate(
    # Ændring i realvækst fra t-1 til t
    delta_realvækst = realvækst - lag(realvækst),
    dummy_stiger    = ifelse(delta_realvækst > 0, 1, 0),
    # Laggede forklarende variable (t-1)
    F2_lag  = lag(F2),
    F4_lag  = lag(F4),
    F9_lag  = lag(F9),
    F10_lag = lag(F10)
  ) %>%
  # Fjern rækker hvor lag-værdier eller dummy ikke kan beregnes
  filter(
    !is.na(dummy_stiger),
    !is.na(F2_lag), !is.na(F4_lag),
    !is.na(F9_lag), !is.na(F10_lag)
  )

# Logitmodel med laggede indikatorer
model_logit_lag <- glm(
  dummy_stiger ~ F2_lag + F4_lag + F9_lag + F10_lag,
  data   = df_logit_lag,
  family = binomial(link = "logit")
)

# Forudsigelser og klassifikation for lag-modellen
df_val_lag <- df_logit_lag %>%
  mutate(
    pred_prob  = predict(model_logit_lag, newdata = ., type = "response"),
    pred_class = ifelse(pred_prob > 0.5, 1, 0)
  )

# Konfusionsmatrix for lag-modellen
tab_lag <- table(
  Forudsagt = df_val_lag$pred_class,
  Faktisk   = df_val_lag$dummy_stiger
)

##########################################################
# 3. SCENARIE 2: CUTOFF-MODEL (MERE KONSERVATIV GRÆNSE)
##########################################################

cutoff <- 0.60  # Konservativ grænse for "stiger"

df_val_cut <- df_logit %>%
  mutate(
    pred_prob  = predict(model_logit, newdata = ., type = "response"),
    pred_class = ifelse(pred_prob > cutoff, 1, 0)
  )

tab_cut <- table(
  Forudsagt = df_val_cut$pred_class,
  Faktisk   = df_val_cut$dummy_stiger
)

# (valgfrit) Tjek flere cutoffs og se, hvordan precision ændrer sig
eval_cutoff <- function(cut) {
  tmp <- df_logit %>%
    mutate(
      pred_prob  = predict(model_logit, newdata = ., type = "response"),
      pred_class = ifelse(pred_prob > cut, 1, 0)
    )
  
  TP <- sum(tmp$pred_class == 1 & tmp$dummy_stiger == 1)
  FP <- sum(tmp$pred_class == 1 & tmp$dummy_stiger == 0)
  forudsagt_1 <- sum(tmp$pred_class == 1)
  
  tibble(
    cutoff      = cut,
    TP          = TP,
    FP          = FP,
    forudsagt_1 = forudsagt_1,
    precision   = ifelse(forudsagt_1 > 0, TP / forudsagt_1, NA_real_)
  )
}

result_cutoffs <- bind_rows(
  lapply(seq(0.50, 0.70, by = 0.05), eval_cutoff)
)

##########################################################
# 4. KONFUSIONSMATRIXER SOM HEATMAPS
##########################################################

plot_confusion <- function(tab, title) {
  df <- as.data.frame(tab)
  colnames(df) <- c("Forudsagt", "Faktisk", "Freq")
  
  df <- df %>%
    mutate(
      Forudsagt = factor(
        Forudsagt,
        levels = c(0, 1),
        labels = c("Model: Ned/falder", "Model: Op/stiger")
      ),
      Faktisk = factor(
        Faktisk,
        levels = c(0, 1),
        labels = c("Faktisk: Ned/falder", "Faktisk: Op/stiger")
      )
    )
  
  ggplot(df, aes(x = Faktisk, y = Forudsagt, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), size = 6) +
    scale_fill_gradient(low = "lightblue", high = "steelblue") +
    labs(
      title = title,
      x     = "Faktisk retning",
      y     = "Modellens forudsigelse",
      fill  = "Antal kvartaler"
    ) +
    theme_minimal(base_size = 14)
}

# Plot for de tre modeller
plot_confusion(tab_base, "Baseline-model: Konfusionsmatrix")
plot_confusion(tab_lag,  "Lag-model: Konfusionsmatrix")
plot_confusion(tab_cut,  "Cutoff-model (0,60): Konfusionsmatrix")

##########################################################
# 5. PERFORMANCE-MÅL (ACCURACY, PRECISION, RECALL)
##########################################################

compute_metrics <- function(tab) {
  # tab: 2x2-tabel med Forudsagt (rækker), Faktisk (kolonner)
  TP <- tab["1", "1"]
  FP <- tab["1", "0"]
  FN <- tab["0", "1"]
  TN <- tab["0", "0"]
  
  accuracy  <- (TP + TN) / sum(tab)
  precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA_real_)
  recall    <- ifelse((TP + FN) > 0, TP / (TP + FN), NA_real_)
  
  tibble(
    Accuracy  = accuracy,
    Precision = precision,
    Recall    = recall
  )
}

metrics <- bind_rows(
  compute_metrics(tab_base) %>% mutate(Model = "Baseline"),
  compute_metrics(tab_lag)  %>% mutate(Model = "Lag-model"),
  compute_metrics(tab_cut)  %>% mutate(Model = "Cutoff-model (0,60)")
)

metrics_long <- metrics %>%
  pivot_longer(
    cols      = c(Accuracy, Precision, Recall),
    names_to  = "Metric",
    values_to = "Value"
  )

ggplot(metrics_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = "dodge") +
  labs(
    title = "Sammenligning af modellernes performance",
    x     = "Model",
    y     = "Værdi",
    fill  = "Mål"
  ) +
  theme_minimal(base_size = 14)