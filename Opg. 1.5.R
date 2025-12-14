##########################################################
# MIKRO-INDIKATORER: FIND DEN BEDSTE KOMBINATION
##########################################################
# Forudsætning: fti_all_q_wide indeholder F2–F13 + realvækst
# Pakker: dplyr, tibble (og gerne stringr/ggplot2 i resten af scriptet)
##########################################################

#########################
# 1. Vælg mikro-spørgsmål
#########################

# Vi definerer her, hvilke F-koder vi betragter som "mikro-økonomiske":
mikro_koder <- c("F2", "F3", "F9", "F10", "F11", "F12", "F13")

# Tjek hurtigt at de faktisk findes som kolonner i datasættet
intersect(mikro_koder, names(fti_all_q_wide))


##############################################
# 2. Funktion: lav alle kombinationer af spørgsmål
##############################################

lav_kombinationer <- function(spg_vektor) {
  # spg_vektor er fx c("F2","F3",...,"F13")
  liste_med_k <- lapply(seq_along(spg_vektor), function(k) {
    combn(spg_vektor, k, simplify = FALSE)   # alle kombinationer med længde k
  })
  unlist(liste_med_k, recursive = FALSE)      # saml til én lang liste
}

# Alle (ikke-tomme) kombinationer af vores mikrospørgsmål
mikro_kombinationer <- lav_kombinationer(mikro_koder)

length(mikro_kombinationer)  # bør være 2^7 - 1 = 127


##############################################
# 3. Hjælpefunktion: beregn R² for én model
##############################################

beregn_r2 <- function(df) {
  # df skal indeholde variablerne: realvækst og indeks
  model <- lm(realvækst ~ indeks, data = df)
  summary(model)$r.squared
}


##########################################################
# 4. BEREGN INDEKS OG R² FOR ALLE MIKRO-KOMBINATIONER
##########################################################

# Vi bruger fti_all_q_wide, men fjerner observationer uden realvækst
fti_regdata_mikro <- fti_all_q_wide %>%
  filter(!is.na(realvækst))

# Loop hen over alle kombinationer og lav én række pr. kombination
r2_mikro <- lapply(seq_along(mikro_kombinationer), function(i) {
  
  vars_i <- mikro_kombinationer[[i]]  # fx c("F2","F10","F13")
  
  # Lav et indeks = gennemsnit af de valgte spørgsmål
  tmp <- fti_regdata_mikro %>%
    mutate(
      indeks = rowMeans(across(all_of(vars_i)), na.rm = TRUE)
    )
  
  # Gem info om kombinationen + dens R²
  tibble(
    komb_id   = i,
    n_spg     = length(vars_i),
    spg_kombi = paste(vars_i, collapse = "+"),
    R2        = beregn_r2(tmp)
  )
  
}) %>%
  bind_rows() %>%             # saml alle kombinationer i én tabel
  arrange(desc(R2))           # sorter så bedste (højeste R²) står øverst

# Kig evt. på de første rækker
head(r2_mikro)


##########################################################
# 5. UDPEGE "BEDSTE" MIKRO-INDIKATORER
##########################################################

# 5.1 Absolut bedste mikroindikator (kan godt være ét spørgsmål)
bedste_mikro_1 <- r2_mikro %>%
  slice(1)

bedste_mikro_1   # fx F10 alene

# 5.2 Bedste mikroindikator med MINIMUM 2 spørgsmål
bedste_mikro_flere <- r2_mikro %>%
  filter(n_spg >= 2) %>%   # kræv mindst 2 spørgsmål i indikatoren
  slice(1)

bedste_mikro_flere
