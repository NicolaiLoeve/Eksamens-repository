library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

##########################################################
# 1. Udvælg F5-spørgsmålet i de månedlige data
##########################################################

spg_forbrugsgoder <- df_spg %>%
  filter(str_detect(
    INDIKATOR,
    "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"
  ))

##########################################################
# 2. Aggregér til kvartaler og vælg perioden 2018K1–2025K3
##########################################################

spg_forbrugsgoder_q <- spg_forbrugsgoder %>%
  mutate(
    Year    = year(TID),
    Quarter = quarter(TID),
    # Første måned i kvartalet (1, 4, 7, 10)
    Month   = (Quarter - 1) * 3 + 1,
    # Dato: første dag i kvartalet
    kvartalsdato = as.Date(sprintf("%d-%02d-01", Year, Month)),
    KVARTAL      = paste0(Year, "K", Quarter)
  ) %>%
  # Filtrér til den ønskede periode
  filter(KVARTAL >= "2018K1", KVARTAL <= "2025K3") %>%
  group_by(kvartalsdato) %>%
  summarise(
    indeks = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

##########################################################
# 3. Beregn gennemsnit for hele perioden
##########################################################

gennemsnit_forbrugsgoder <- mean(spg_forbrugsgoder_q$indeks, na.rm = TRUE)
gennemsnit_forbrugsgoder

##########################################################
# 4. Visualisering: tidsserie af F5-indekset + gennemsnit
##########################################################

ggplot(spg_forbrugsgoder_q, aes(x = kvartalsdato, y = indeks)) +
  geom_line() +
  
  # Nul-linje (balance mellem optimister/pessimister)
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # Gennemsnitslinje for hele perioden
  geom_hline(
    yintercept = gennemsnit_forbrugsgoder,
    color      = "red",
    linewidth  = 0.8
  ) +
  
  # Label til gennemsnittet
  annotate(
    "text",
    x     = as.Date("2018-06-01"),   # placering kan justeres
    y     = gennemsnit_forbrugsgoder + 1,
    label = paste0("Gennemsnit: ", round(gennemsnit_forbrugsgoder, 1)),
    color = "red",
    size  = 3
  ) +
  
  scale_x_date(
    date_breaks = "3 years",
    date_labels = "%Y",
    limits      = c(as.Date("2018-01-01"), as.Date("2025-09-30"))
  ) +
  
  labs(
    title    = "Konstant negativitet omkring anskaffelse af større forbrugsgoder",
    subtitle = "Kvartalsvist gennemsnit, 2018K1–2025K3",
    x        = "År",
    y        = "Balancetal (indeks)",
    caption  = "Kilde: Danmarks Statistik (FORV1)"
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, size = 9)
  )
