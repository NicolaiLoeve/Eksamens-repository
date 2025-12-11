###########################################################
# Forbedret R-kode til Opgave 2
# Korrelation og simpel lineær regression (villaer)
###########################################################

library(tidyverse)
library(ggplot2)
library(corrplot)
# (valgfrit) library(broom)  # hvis man vil lave flotte modeltabeller senere

#----------------------------------------------------------
# 0. Hjælpefunktion til at rense tal fra tekst
#----------------------------------------------------------

clean_num <- function(x) {
  as.numeric(gsub("[^0-9]", "", x))
}

#----------------------------------------------------------
# 1. Indlæser data og grundrens (fjerner kun vej)
#----------------------------------------------------------

boliger <- read.csv("newhomes (3).csv")

boliger2 <- boliger %>%
  select(-vej)

#----------------------------------------------------------
# 2. Rensning + udvælgelse af villaer
#   (alt samlet ét sted – i stedet for at vi renser flere gange)
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


####### Fjerner outliers
villa_no_outliers <- villa_clean %>%
  filter(
    alder >= 1795,      # behold kun boliger fra 1795 og nyere
    kvm <= 600,         # fjern gigantvillaer
    pris <= 50000000    # fjern ultra-dyre outliers
  )
###Efter at fjerne outliers, får vi et datasæt med 6.548 villaer. Men når vi kigger i datasættet er 
###der enkelte NA-værdier, dem skal vi have fjernet

### Fjerner NA-værdier.
villa_no_outliers <- villa_no_outliers %>% 
  drop_na()


####Efter at vi har fjernet NA-Værdierne så får vi 6.488


#----------------------------------------------------------
# 2.1 – Beskrivende statistik
#----------------------------------------------------------

# Simpel deskriptiv tabel
deskriptiv_stats <- villa_no_outliers %>%
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
boxplot(villa_no_outliers$pris, main = "Pris – villaer")
###
ggplot(villa_no_outliers, aes(x = 1, y = pris)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, color = "black") +
  scale_y_continuous(
    labels = function(x) paste0(x / 1e6, " mio. kr.")
  ) +
  labs(
    title = "Pris – villaer",
    x = "",
    y = "Pris (i mio. kr.)"
  ) +
  theme_minimal()
###
boxplot(villa_no_outliers$kvm, main = "Kvm – villaer")
###
ggplot(villa_no_outliers, aes(x = 1, y = kvm)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, color = "black") +
  scale_y_continuous(
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ","), " m²")
  ) +
  labs(
    title = "Kvm – villaer",
    x = "",
    y = "Boligareal (m²)"
  ) +
  theme_minimal()

###
boxplot(villa_no_outliers$ejerudg, main = "Ejerudgift – villaer")
###
ggplot(villa_no_outliers, aes(x = 1, y = ejerudg)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.4, color = "black") +
  scale_y_continuous(
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ","), " kr.")
  ) +
  labs(
    title = "Ejerudgift – villaer",
    x = "",
    y = "Ejerudgift (kr.)"
  ) +
  theme_minimal()

###

#----------------------------------------------------------
# 2.2 – Korrelation mellem m2 og pris
#----------------------------------------------------------

# Scatterplot m2 vs pris
ggplot(villa_no_outliers, aes(x = kvm, y = pris)) +
  geom_point(alpha = 0.4) +
  scale_y_continuous(
    labels = function(x) paste0(format(round(x / 1e6, 2),
                                       big.mark = ".", decimal.mark = ","), " mio. kr.")
  ) +
  labs(
    title = "Sammenhæng mellem boligstørrelse (m²) og pris",
    x = "Boligareal (m²)",
    y = "Pris (mio. kr.)"
  ) +
  theme_minimal()


######
# Lineær regression: pris ~ kvm
pris_model <- lm(pris ~ kvm, data = villa_no_outliers)
summary(pris_model)

# Korrelation (det konkrete tal, opgaven spørger efter)
kor_kvm_pris <- cor(villa_no_outliers$kvm, villa_no_outliers$pris, use = "complete.obs")
kor_kvm_pris

#----------------------------------------------------------
# 2.3 – 5 simple regressioner med Y = pris pr. m² (priskvm)
#----------------------------------------------------------

# 1) priskvm ~ kvm
ggplot(villa_no_outliers, aes(x = kvm, y = priskvm)) +
  geom_point(alpha = 0.25, color = "black") +
  scale_y_continuous(
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ","), " kr./m²")
  ) +
  labs(
    title = "Pris pr. m² som funktion af boligareal",
    x = "Boligareal (m²)",
    y = "Pris pr. m² (kr.)"
  ) +
  theme_minimal()

kvm_model <- lm(priskvm ~ kvm, data = villa_no_outliers)
summary(kvm_model)

# 2) priskvm ~ ejerudg
ggplot(villa_no_outliers, aes(x = ejerudg, y = priskvm)) +
  geom_point(alpha = 0.25, color = "black") +
  scale_y_continuous(
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ","), " kr./m²")
  ) +
  scale_x_continuous(
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ","), " kr.")
  ) +
  labs(
    title = "Pris pr. m² som funktion af ejerudgift",
    x = "Ejerudgift (kr. pr. måned)",
    y = "Pris pr. m² (kr.)"
  ) +
  theme_minimal()

ejerudg_model <- lm(priskvm ~ ejerudg, data = villa_no_outliers)
summary(ejerudg_model)

# 3) priskvm ~ pris

ggplot(villa_no_outliers, aes(x = pris, y = priskvm)) +
  geom_point(alpha = 0.25, color = "black") +
  scale_y_continuous(
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ","), " kr./m²")
  ) +
  scale_x_continuous(
    labels = function(x) paste0(format(round(x / 1e6, 2),
                                       big.mark = ".", decimal.mark = ","), " mio. kr.")
  ) +
  labs(
    title = "Pris pr. m² som funktion af totalpris",
    x = "Totalpris (mio. kr.)",
    y = "Pris pr. m² (kr.)"
  ) +
  theme_minimal()

pris_model2 <- lm(priskvm ~ pris, data = villa_no_outliers)
summary(pris_model2)

# 4) priskvm ~ vaer
ggplot(villa_no_outliers, aes(x = vaer, y = priskvm)) +
  geom_point(alpha = 0.25, color = "black") +
  scale_y_continuous(
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ","), " kr./m²")
  ) +
  scale_x_continuous(
    breaks = pretty(villa_no_outliers$vaer),
    labels = function(x) format(x, big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Pris pr. m² som funktion af antal værelser",
    x = "Antal værelser",
    y = "Pris pr. m² (kr.)"
  ) +
  theme_minimal()

vaer_model <- lm(priskvm ~ vaer, data = villa_no_outliers)
summary(vaer_model)

# 5) priskvm ~ alder2 (boligens alder)
ggplot(villa_no_outliers, aes(x = alder2, y = priskvm)) +
  geom_point(alpha = 0.25, color = "black") +
  scale_y_continuous(
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ","), " kr./m²")
  ) +
  scale_x_continuous(
    labels = function(x) paste0(format(x, big.mark = ".", decimal.mark = ","), " år")
  ) +
  labs(
    title = "Pris pr. m² som funktion af boligens alder",
    x = "Boligens alder (år)",
    y = "Pris pr. m² (kr.)"
  ) +
  theme_minimal()

alder_model <- lm(priskvm ~ alder2, data = villa_no_outliers)
summary(alder_model)

#----------------------------------------------------------
# 2.3 – Korrelationsmatrix til at understøtte modellerne
#----------------------------------------------------------

kormatrix_df <- villa_no_outliers %>%
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
cor(villa_no_outliers$priskvm, villa_no_outliers$ejerudg)^2
# -> de to værdier vil (næsten) være ens i en simpel regression
#----------------------------------------------------------
