###Opg 4.1
library(ggplot2)
library(lubridate)


#Vi skal i opgaven kun bruge fra 2000 til 2022 (Da det er det data som er til rådighed)
#Da vi hentede alt data (1994-2022), så bruger vi lige noget dplyr, til at rette lidt i datasættet.
alko_df_2000_2022 <- alkodata %>%
  filter(TID >= "2000-01-01", TID <= "2022-01-01")


### Nu skal vi prøve at plotte en 
### 'illustration af udviklingen i de enkelte grupper'
############ Ny pakke til nyt plot 
### Jeg googlede mig frem til at pakken "ggh4x" kan gå ind og 'klemme' ting ind på grafer
### når der er mange ting som skal være på samme side.
install.packages("ggh4x")
library(ggh4x)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

plotdata <- alko_df_2000_2022 %>%
  mutate(
    ÅR = year(TID),
    KONSUM_NAVN = str_replace(KONSUMGRP, "^[0-9. ]+ ", "")
  )

ggplot(plotdata, aes(x = ÅR, y = value)) +
  geom_line() +
  ggh4x::facet_wrap2(
    ~ KONSUM_NAVN,
    scales = "free_y",
    axes = "all"        # << her er rettelsen
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Udvikling i forbruget for alkoholiske varegrupper (2000–2022)",
    x = "År",
    y = "Kr. pr. husstand"
  )

###Okay, så det vi har fået nu, er en graf over "udviklingen i forbruget for forskellige 
###alkoholiske grupper.

#Det vi så gør nu er:
# Vi tager fat i vores dataframe "alkodata" som er den vi har fået fra API kaldet
# Så filtrere vi, så vi har 1999-2022 og laver indeks (2000 = 100)

# 1) Filtrer til 1999–2022
alko_df_1999_2022 <- alkodata %>% 
  filter(TID >= "1999-01-01", TID <= "2022-01-01")

# 2) Lav pæne navne + indeks (2000 = 100)
plotdata_index <- alko_df_1999_2022 %>%
  mutate(
    ÅR = year(TID),
    KONSUM_NAVN = str_replace(KONSUMGRP, "^[0-9. ]+ ", "")
  ) %>%
  group_by(KONSUM_NAVN) %>%
  mutate(
    base_2000 = value[ÅR == 2000],          # niveau i år 2000
    indeks    = value / base_2000 * 100     # 2000 = 100
  ) %>%
  ungroup()

# Vi starter med at gruppere de alkoholiske grupper, så det giver mening

plotdata_grupperet <- plotdata_index %>%
  mutate(
    GRUPPE = case_when(
      # 1. Spiritus
      KONSUM_NAVN == "Spiritus og likør" ~ "Spiritus og likør",
      
      # 2. Vin samlet
      KONSUM_NAVN %in% c("Vin af druer",
                         "Vin af andre frugter",
                         "Hedvin") ~ "Vin (druer, andre frugter, hedvin)",
      
      # 3. Vinbaseret + lav/alkoholfri øl
      KONSUM_NAVN %in% c("Vinbaserede drikkevarer og alkoholfri vin",
                         "Øl med lavt alkoholindhold og alkoholfri øl") ~
        "Vinbaserede drikkevarer & lav/alkoholfri øl",
      
      # 4. Øl-gruppen
      KONSUM_NAVN %in% c("Pilsnerøl, guldøl",
                         "Andre alkoholholdige øl",
                         "Øl-baserede drikkevarer") ~
        "Øl (pilsner, andre, øl-baserede)",
      
      # 5. Alkoholiske læskedrikke
      KONSUM_NAVN == "Alkoholiske læskedrikke" ~ "Alkoholiske læskedrikke",
      
      TRUE ~ NA_character_
    )
  )
##################
### Her har vi så fået oprettet 5 grupper ud fra vores 10 forskellige forbrugsgrupper.
###Nu summer vi værdierne inden for hver samlet gruppe pr. år.
plotdata_gruppe_sum <- plotdata_grupperet %>%
  group_by(GRUPPE, ÅR) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

#################
### Nu laver vi indeks (2000=100) for de 5 nye grupper.
#Indeks per samlet gruppe, baseret på deres samlede niveau i år 2000.
plotdata_gruppe_index <- plotdata_gruppe_sum %>%
  group_by(GRUPPE) %>%
  mutate(
    base_2000 = value[ÅR == 2000],
    indeks = value / base_2000 * 100
  ) %>%
  ungroup()


#################
### Her prøver vi at plotte vores 5 nye grupper!

ggplot(plotdata_gruppe_index, aes(x = ÅR, y = indeks)) +
  geom_line() +
  ggh4x::facet_wrap2(
    ~ GRUPPE,
    axes   = "all",
    scales = "fixed"
  ) +
  scale_x_continuous(breaks = seq(1999, 2022, by = 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Indekseret udvikling i samlet forbrug (2000 = 100)",
    x = "År",
    y = "Indeks (2000 = 100)"
  )



##### Problemer med alkoholiske læskedrikke, så vi prøver lige en kode for at få den til at virke.
plotdata_gruppe_index <- plotdata_gruppe_sum %>%
  group_by(GRUPPE) %>%
  mutate(
    base_value = value[ÅR == 2000],
    # hvis base = 0, find første positive værdi i gruppen
    base_value = ifelse(base_value == 0,
                        value[which(value > 0)][1],
                        base_value),
    indeks = value / base_value * 100
  ) %>%
  ungroup()

######
###Prøver nu et nyt plot. med Log-Skala
ggplot(plotdata_gruppe_index, aes(x = ÅR, y = indeks)) +
  geom_line() +
  ggh4x::facet_wrap2(
    ~ GRUPPE,
    axes = "all",
    scales = "fixed"
  ) +
  scale_y_log10() +   # << log-skala!
  scale_x_continuous(breaks = seq(1999, 2022, by = 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Indekseret udvikling i samlet forbrug (log-skala, 2000 = 100)",
    x = "År",
    y = "Indeks (log-skala)"
  )
# Log-skalaen anvendes for at gøre udviklingen mere sammenlignelig på tværs af grupper.
# En logaritmisk akse viser procentvise ændringer frem for absolutte niveauer.
# Det betyder, at produktgrupper med små relative ændringer (fx vin og spiritus)
# fremstår mere stabile, mens grupper med kraftig vækst (fx alkoholiske læskedrikke)
# vises tydeligere uden at dominere figuren. Log-skalaen giver derfor et mere retvisende
# billede af forskelle i vækstrater mellem de samlede alkoholgrupper.



#Nu har vi styr på den del...
## Nu laver vi så et plot, hvor vi får lidt farve på! Samtidig får vi alle graferne i et.

ggplot(plotdata_gruppe_index,
       aes(x = ÅR, y = indeks, color = GRUPPE)) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(1999, 2022, by = 2)) +
  labs(
    title = "Indekseret udvikling i samlet alkoholforbrug efter hovedgrupper (2000 = 100)",
    x = "År",
    y = "Indeks (log-skala, 2000 = 100)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.box   = "horizontal"
  )



