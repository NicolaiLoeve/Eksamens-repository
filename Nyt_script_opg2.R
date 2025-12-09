###########################################################
# Forbedret R-kode til Opgave 2
# Korrelation og simpel lineær regression (villaer)
###########################################################

library(tidyverse)
library(ggplot2)
library(corrplot)
# (valgfrit) library(broom)  # hvis du vil lave flotte modeltabeller senere

#----------------------------------------------------------
# 0. Hjælpefunktion til at rense tal fra tekst
#----------------------------------------------------------

clean_num <- function(x) {
  as.numeric(gsub("[^0-9]", "", x))
}

#----------------------------------------------------------
# 1. Indlæs data og grundrens (fjerner kun vej)
#----------------------------------------------------------

boliger <- read.csv("newhomes (3).csv")

boliger2 <- boliger %>%
  select(-vej)

#----------------------------------------------------------
# 2. Rensning + udvælgelse af villaer
#   (alt samlet ét sted – i stedet for at rense flere gange)
#----------------------------------------------------------

villa_clean <- boliger2 %>%
  # Kun villaer
  filter(type == "Villa") %>%
  
  # Fjern helt skøre rækker, hvor kvm indeholder "ejerudg."
  filter(!grepl("ejerudg.", kvm, ignore.case = TRUE)) %>%
  
  # Rens tekst -> tal på én gang
  mutate(
    pris    = clean_num(pris),
    kvm     = clean_num(kvm),
    vaer    = clean_num(vaer),
    ejerudg = clean_num(ejerudg),
    alder   = clean_num(alder)
  ) %>%
  
  # Drop NA efter rensning
  drop_na(pris, kvm, vaer, ejerudg, alder) %>%
  
  # Fjern mærkelige postnumre med "%"
  filter(!grepl("%", zip)) %>%
  
  # Opret afledte variable
  mutate(
    priskvm = pris / kvm,         # pris pr. m2
    alder2  = 2025 - alder        # hvor gammel boligen er
  ) %>%
  
  drop_na(priskvm, alder2)

# (Valgfrit) Kig hurtigt på hvor mange der er tilbage
nrow(villa_clean)

#----------------------------------------------------------
# 2.1 – Beskrivende statistik
#----------------------------------------------------------

# Simpel deskriptiv tabel
deskriptiv_stats <- villa_clean %>%
  summarise(
    across(
      .cols = c(pris, kvm, ejerudg, vaer, priskvm, alder2),
      .fns  = list(
        mean   = ~mean(.x, na.rm = TRUE),
        sd     = ~sd(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        min    = ~min(.x, na.rm = TRUE),
        max    = ~max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

deskriptiv_stats

# Boxplots til hurtigt overblik (bruges i opgaven)
boxplot(villa_clean$pris, main = "Pris – villaer")
boxplot(villa_clean$kvm, main = "Kvm – villaer")
boxplot(villa_clean$ejerudg, main = "Ejerudgift – villaer")

#----------------------------------------------------------
# 2.2 – Korrelation mellem m2 og pris
#----------------------------------------------------------

# Scatterplot m2 vs pris
ggplot(villa_clean, aes(x = kvm, y = pris)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Sammenhæng mellem boligstørrelse (m²) og pris",
    x = "Boligareal (m²)",
    y = "Pris (kr.)"
  )

# Lineær regression: pris ~ kvm
pris_model <- lm(pris ~ kvm, data = villa_clean)
summary(pris_model)

# Korrelation (det konkrete tal, opgaven spørger efter)
kor_kvm_pris <- cor(villa_clean$kvm, villa_clean$pris, use = "complete.obs")
kor_kvm_pris

#----------------------------------------------------------
# 2.3 – 5 simple regressioner med Y = pris pr. m² (priskvm)
#----------------------------------------------------------

# 1) priskvm ~ kvm
ggplot(villa_clean, aes(x = kvm, y = priskvm)) +
  geom_point(alpha = 0.4) +
  labs(title = "Pris pr. m² som funktion af boligareal")

kvm_model <- lm(priskvm ~ kvm, data = villa_clean)
summary(kvm_model)

# 2) priskvm ~ ejerudg
ggplot(villa_clean, aes(x = ejerudg, y = priskvm)) +
  geom_point(alpha = 0.4) +
  labs(title = "Pris pr. m² som funktion af ejerudgift")

ejerudg_model <- lm(priskvm ~ ejerudg, data = villa_clean)
summary(ejerudg_model)

# 3) priskvm ~ pris
ggplot(villa_clean, aes(x = pris, y = priskvm)) +
  geom_point(alpha = 0.4) +
  labs(title = "Pris pr. m² som funktion af totalpris")

pris_model2 <- lm(priskvm ~ pris, data = villa_clean)
summary(pris_model2)

# 4) priskvm ~ vaer
ggplot(villa_clean, aes(x = vaer, y = priskvm)) +
  geom_point(alpha = 0.4) +
  labs(title = "Pris pr. m² som funktion af antal værelser")

vaer_model <- lm(priskvm ~ vaer, data = villa_clean)
summary(vaer_model)

# 5) priskvm ~ alder2 (boligens alder)
ggplot(villa_clean, aes(x = alder2, y = priskvm)) +
  geom_point(alpha = 0.4) +
  labs(title = "Pris pr. m² som funktion af boligens alder")

alder_model <- lm(priskvm ~ alder2, data = villa_clean)
summary(alder_model)

#----------------------------------------------------------
# 2.3 – Korrelationsmatrix til at understøtte modellerne
#----------------------------------------------------------

kormatrix_df <- villa_clean %>%
  select(pris, kvm, ejerudg, vaer, priskvm, alder2)

cor_matrix <- cor(kormatrix_df, use = "complete.obs")

corrplot(
  cor_matrix,
  method = "color",
  addCoef.col = "black",
  tl.col = "black",
  number.cex = 0.7
)

#----------------------------------------------------------
# 2.4 – (ingen kodekrav)
# Teorien om sammenhængen mellem korrelation og simpel regression
# skriver vi i tekst – men vi kan bruge R² og cor() til at illustrere:
# fx:
summary(ejerudg_model)$r.squared
cor(villa_clean$priskvm, villa_clean$ejerudg)^2
# -> de to værdier vil (næsten) være ens i en simpel regression
#----------------------------------------------------------
