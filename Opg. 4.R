##############################
# Opgave 4.1 – Stabilitet af bedste indikator (indeks)
##############################

library(dplyr)
library(ggplot2)

# 0) DATA: vælg bedste indikator og klargør
# antager: fti_regdata og bedste findes fra opgave 1

ftiq <- fti_regdata %>%
  filter(komb_id == bedste$komb_id) %>%    # kun bedste kombination
  mutate(tid = KVARTAL) %>%                # tidsmarkør, fx "2000K1"
  select(tid, realvækst, indeks)           # mål + indikator

dat <- ftiq %>%
  drop_na()

##############################
# 1) BASISMODEL (hele perioden)
##############################

model_full <- lm(realvækst ~ indeks, data = dat)
sum_full   <- summary(model_full)

cat("=== Basis-model: realvækst ~ indeks (hele perioden) ===\n")
cat("R²     :", round(sum_full$r.squared,     3), "\n")
cat("Adj. R²:", round(sum_full$adj.r.squared, 3), "\n")
cat("Beta(indeks):", round(sum_full$coefficients["indeks", "Estimate"], 3),
    "  p-værdi:", round(sum_full$coefficients["indeks", "Pr(>|t|)"], 3), "\n\n")

##############################
# 2) SPLIT-SAMPLE (før/efter 2013K1)
##############################

cut_tid <- "2013K1"        # kan ændres hvis I vil teste et andet brud

pre  <- dat %>% filter(tid <  cut_tid)
post <- dat %>% filter(tid >= cut_tid)

mod_pre  <- lm(realvækst ~ indeks, data = pre)
mod_post <- lm(realvækst ~ indeks, data = post)

sum_pre  <- summary(mod_pre)
sum_post <- summary(mod_post)

cat("=== Split-sample: realvækst ~ indeks ===\n")
cat("PRE-periode  :", first(pre$tid),  "til", last(pre$tid), "\n")
cat("  Adj. R²    :", round(sum_pre$adj.r.squared, 3),
    "  Beta(indeks):", round(sum_pre$coefficients["indeks", "Estimate"], 3),
    "  p-værdi:", round(sum_pre$coefficients["indeks", "Pr(>|t|)"], 3), "\n")
cat("POST-periode :", first(post$tid), "til", last(post$tid), "\n")
cat("  Adj. R²    :", round(sum_post$adj.r.squared, 3),
    "  Beta(indeks):", round(sum_post$coefficients["indeks", "Estimate"], 3),
    "  p-værdi:", round(sum_post$coefficients["indeks", "Pr(>|t|)"], 3), "\n\n")

##############################
# 3) RULLENDE adj. R² (vindue = 40 kvartaler ≈ 10 år)
##############################

W <- 40                          # vindueslængde
n <- nrow(dat)
roll_adjR2 <- numeric(n)

for (i in seq_len(n)) {
  i1 <- max(1, i - W + 1)
  d  <- dat[i1:i, ]
  
  # kræv mindst fx 10 observationer for at estimere
  if (nrow(d) >= 10) {
    m  <- lm(realvækst ~ indeks, data = d)
    roll_adjR2[i] <- summary(m)$adj.r.squared
  } else {
    roll_adjR2[i] <- NA
  }
}


# Udtræk årstal fra tid-variablen, fx "2005K3" → 2005
roll_df$year <- as.numeric(substr(roll_df$tid, 1, 4))

cat("=== Rullende adj. R² (vindue = 40 kvartaler) ===\n")
cat("Gennemsnitlig adj. R²:", round(mean(roll_df$adjR2, na.rm = TRUE), 3), "\n\n")

# Vælg kun hvert 3. år som label
year_breaks <- unique(roll_df$year)[seq(1, length(unique(roll_df$year)), by = 3)]

ggplot(roll_df, aes(x = year, y = adjR2, group = 1)) +
  geom_line() +
  geom_hline(yintercept = mean(roll_df$adjR2, na.rm = TRUE),
             linetype = "dotted") +
  scale_x_continuous(breaks = year_breaks) +
  labs(
    title = "Rullende justeret R² for bedste indikator",
    subtitle = "Vindue = 40 kvartaler (~10 år)",
    x = "År",
    y = "Adj. R²"
  ) +
  theme_minimal(base_size = 12)

basis_mod <- lm(realvækst ~ indeks, data = dat)
basis_sum <- summary(basis_mod)

basis_sum$adj.r.squared
basis_sum$coefficients
