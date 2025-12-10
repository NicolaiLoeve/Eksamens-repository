library(dplyr)
library(ggplot2)

##########################################################
# 1. Læg modelens forudsigelser oven på data
##########################################################

df_validation <- df_logit %>%
  mutate(
    # Modellens forudsagte sandsynlighed for stigning
    pred_prob  = predict(model_logit, newdata = ., type = "response"),
    
    # Modellens klassifikation: 1 = stigning, 0 = fald/uændret
    pred_class = ifelse(pred_prob > 0.5, 1, 0)
  )

##########################################################
# 2. Kort, samlet oversigt over modellens træfsikkerhed
##########################################################

# Faktisk udfald (stiger = 1 eller 0)
antal_faktisk_stigning <- sum(df_validation$stiger == 1)
antal_faktisk_fald     <- sum(df_validation$stiger == 0)

# Modellens forudsigelser (pred_class)
antal_forudsagt_stigning <- sum(df_validation$pred_class == 1)
antal_forudsagt_fald     <- sum(df_validation$pred_class == 0)

# Hvor ofte rammer modellen rigtigt?
antal_korrekte <- sum(df_validation$pred_class == df_validation$stiger)
total_obs      <- nrow(df_validation)
accuracy       <- antal_korrekte / total_obs

# Saml i én tabel, så det er nemt at forklare i studiegruppen
model_overblik <- tibble::tibble(
  antal_faktisk_stigning    = antal_faktisk_stigning,
  antal_faktisk_fald        = antal_faktisk_fald,
  antal_forudsagt_stigning  = antal_forudsagt_stigning,
  antal_forudsagt_fald      = antal_forudsagt_fald,
  antal_korrekte            = antal_korrekte,
  total_obs                 = total_obs,
  accuracy                  = accuracy
)

model_overblik

##########################################################
# 3. Konfusionsmatrix som heatmap
##########################################################

# 2x2-konfusionsmatrix
tab <- table(
  Faktisk   = df_validation$stiger,
  Forudsagt = df_validation$pred_class
)

# Gør tabellen til et data frame og sæt pæne labels på
df_heat <- as.data.frame(tab) %>%
  mutate(
    Faktisk = factor(
      Faktisk,
      levels = c(0, 1),
      labels = c("Faktisk: Ned/falder", "Faktisk: Op/stiger")
    ),
    Forudsagt = factor(
      Forudsagt,
      levels = c(0, 1),
      labels = c("Model: Ned/falder", "Model: Op/stiger")
    )
  )

# Plot konfusionsmatrix som heatmap
ggplot(df_heat, aes(x = Faktisk, y = Forudsagt, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title    = "Konfusionsmatrix for logitmodellen",
    subtitle = "Forudsigelse af om realvæksten stiger (1) eller ej (0)",
    x        = "Faktisk retning",
    y        = "Modellens forudsigelse",
    fill     = "Antal kvartaler"
  ) +
  theme_minimal(base_size = 14)
