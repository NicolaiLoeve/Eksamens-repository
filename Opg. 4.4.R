library(dplyr)
library(stringr)

# --------------------------------------------------------
# Opgave 4.4 – 30 simple lineære regressioner
# 15 forbrugsgrupper (y) mod DST og DI forbrugertillid
# Periode: 2000K1–2025K3
# Forudsætter: forbrug15_q, ftidstq_wide, ftidiq_wide er allerede lavet
# --------------------------------------------------------

# 1. Afgræns FTI-data til 2000K1–2025K3 -------------------

dst_fti <- ftidstq_wide %>%
  mutate(
    Year    = as.integer(substr(KVARTAL, 1, 4)),
    Quarter = as.integer(substr(KVARTAL, 6, 6))
  ) %>%
  filter(
    Year >= 2000,
    Year < 2025 | (Year == 2025 & Quarter <= 3)
  ) %>%
  select(KVARTAL, fti_dst = Forbrugertillidsindikator)

di_fti <- ftidiq_wide %>%
  mutate(
    Year    = as.integer(substr(KVARTAL, 1, 4)),
    Quarter = as.integer(substr(KVARTAL, 6, 6))
  ) %>%
  filter(
    Year >= 2000,
    Year < 2025 | (Year == 2025 & Quarter <= 3)
  ) %>%
  select(KVARTAL, fti_di = Forbrugertillidsindikator)

# 2. Match forbrugsgrupper med begge FTI-kilder ------------

regdata <- forbrug15_q %>%
  mutate(
    Year    = as.integer(substr(KVARTAL, 1, 4)),
    Quarter = as.integer(substr(KVARTAL, 6, 6))
  ) %>%
  filter(
    Year >= 2000,
    Year < 2025 | (Year == 2025 & Quarter <= 3)
  ) %>%
  left_join(dst_fti, by = "KVARTAL") %>%
  left_join(di_fti,  by = "KVARTAL") %>%
  # hvis du vil være helt sikker på at undgå NA i FTI:
  filter(!is.na(fti_dst), !is.na(fti_di))

# 3. Identificér de 15 forbrugsgrupper ---------------------

not_groups <- c("KVARTAL", "Year", "Quarter", "fti_dst", "fti_di")
forbrugsgrupper <- setdiff(names(regdata), not_groups)

length(forbrugsgrupper)   # skal være 15

# 4. 15 regressioner: y ~ fti_dst (DST) --------------------

reg_dst_list <- lapply(forbrugsgrupper, function(grp) {
  # backticks rundt om grp pga. mellemrum/punktummer i navnet
  formel <- as.formula(paste0("`", grp, "` ~ fti_dst"))
  summary(lm(formel, data = regdata))
})
names(reg_dst_list) <- paste0("DST_", forbrugsgrupper)

# 5. 15 regressioner: y ~ fti_di (DI) ----------------------

reg_di_list <- lapply(forbrugsgrupper, function(grp) {
  formel <- as.formula(paste0("`", grp, "` ~ fti_di"))
  summary(lm(formel, data = regdata))
})
names(reg_di_list) <- paste0("DI_", forbrugsgrupper)

# 6. Saml alle 30 modeller i én liste ----------------------

reg_all <- c(reg_dst_list, reg_di_list)

length(reg_all)  # skal være 30

# Eksempler på at tilgå modeller (tilpas til dine faktiske kolonnenavne):
# reg_all[["DST_Fødevarer mv."]]
# reg_all[["DI_Fødevarer mv."]]

# 7. Træk Intercept, Beta, p-værdi og R² ud i én tabel ------

extract_stats <- function(model_summary) {
  data.frame(
    Intercept = model_summary$coefficients[1, 1],
    Beta      = model_summary$coefficients[2, 1],
    p_value   = model_summary$coefficients[2, 4],
    R2        = model_summary$r.squared
  )
}

reg_results <- lapply(reg_all, extract_stats) %>%
  bind_rows(.id = "Model") %>%
  arrange(desc(R2))

reg_results

# For at visualisere forklaringsgraderne, plotter vi det - først laver vi et plot med de 5 højeste forklaringsgrader

library(dplyr)
library(ggplot2)

top5 <- reg_results %>%
  arrange(desc(R2)) %>%
  slice_head(n = 5)

ggplot(top5, aes(x = reorder(Model, R2), y = R2, fill = R2)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 5 højeste forklaringsgrader (R²)",
    subtitle = "De forbrugsgrupper der bedst forklares af forbrugertillid",
    x = "Model",
    y = "R²"
  ) +
  scale_fill_gradient(low = "skyblue", high = "steelblue") +
  theme_minimal()

# Og så plotter vi de 5 laveste forklaringsgrader

bottom5 <- reg_results %>%
  arrange(R2) %>%
  slice_head(n = 5)

ggplot(bottom5, aes(x = reorder(Model, R2), y = R2, fill = R2)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 5 laveste forklaringsgrader (R²)",
    subtitle = "De forbrugsgrupper der næsten ikke påvirkes af forbrugertillid",
    x = "Model",
    y = "R²"
  ) +
  scale_fill_gradient(low = "mistyrose", high = "red") +
  theme_minimal()

