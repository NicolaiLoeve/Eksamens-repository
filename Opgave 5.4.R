############## Opgave 5.4 #############

# finder gennemsnit for hele perioden (2000Q1–2025Q2)
gns_alle <- colMeans(df_EUwide[,-1], na.rm = TRUE)

# Laver ny df med kun til og med 2020Q1
df_pre_covid <- df_EUwide[df_EUwide$time <= "2020-Q1", ]

# finder gennemsnit for perioden 2000Q1–2020Q1
gns_før <- colMeans(df_pre_covid[,-1], na.rm = TRUE)

# sammenligner de to 
sammenligning2 <- rbind(
  alle_år = gns_alle,
  før_covid = gns_før,
  difference = gns_før - gns_alle
)

# 5. Find landet med den største negative difference
min_land <- names(which.min(sammenligning2["difference", ]))
min_value   <- sammenligning2["difference", min_land]

min_land
min_value

# > min_land
# [1] "ES"
# > min_value
# [1] -0.1773829

########### KONKLUSION ####################
# spanien har mærket den største effekt af corona på forbruget



##### Den feedback vi fik i opgave 5.4 var, at vi skulle tage alle EU lande med.
library(dplyr)
library(tidyr)

# Liste over europæiske lande (ISO2-koder)
europaeiske_lande <- c(
  "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE",
  "EL","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL",
  "PT","RO","SK","SI","ES","SE",
  "NO","IS","CH","UK"
)

df_all <- dfnamq %>% 
  filter(unit == "CLV20_MEUR",
         s_adj == "SCA",
         na_item == "P31_S14",
         geo %in% europaeiske_lande) %>%
  mutate(time = as.character(time)) %>% 
  filter(time >= "2000-Q1", time <= "2025-Q3")

df_EUR_all <- df_all %>%
  arrange(geo, time) %>%
  group_by(geo) %>%
  mutate(realvaekst = (values / lag(values, 4) - 1) * 100) %>%
  ungroup() %>%
  filter(!is.na(realvaekst))

# Wide-format som i 5.2/5.3
df_EUwide_all <- df_EUR_all %>%
  select(geo, time, realvaekst) %>%
  pivot_wider(
    names_from  = geo,
    values_from = realvaekst
  )
#########
########### Opgave 5.4 – Effekt af corona ###############

# 1) Data før corona
df_pre_covid <- df_EUwide_all %>% 
  filter(time >= "2000-Q1", time <= "2019-Q4")

gns_pre <- colMeans(df_pre_covid[ , -1], na.rm = TRUE)

# 2) Data fra og med corona (2020Q1–2025Q3)
df_covid_og_efter <- df_EUwide_all %>% 
  filter(time >= "2020-Q1", time <= "2025-Q3")

gns_covid <- colMeans(df_covid_og_efter[ , -1], na.rm = TRUE)

# 3) Sammenlign – hvor meget ændrer gennemsnittet sig?
sammenligning2 <- rbind(
  foer_covid = gns_pre,
  covid_efter = gns_covid,
  difference = gns_covid - gns_pre   # negativ = fald i vækst
)

# 4) Landet med den største NEGATIVE forskel
min_land  <- names(which.min(sammenligning2["difference", ]))
min_value <- sammenligning2["difference", min_land]

min_land
min_value


################ Okay, vi prøver at plotte det.
library(dplyr)
library(ggplot2)
library(tidyr)

########### 1) Data før og efter corona ###########

df_pre_covid <- df_EUwide_all %>% 
  filter(time >= "2000-Q1", time <= "2019-Q4")

df_covid_efter <- df_EUwide_all %>% 
  filter(time >= "2020-Q1", time <= "2025-Q3")

gns_pre   <- colMeans(df_pre_covid[ , -1], na.rm = TRUE)
gns_after <- colMeans(df_covid_efter[ , -1], na.rm = TRUE)

########### 2) Sammenligning ###########

sammenligning_all <- rbind(
  foer_covid = gns_pre,
  covid_efter = gns_after,
  difference = gns_after - gns_pre
)

forskel_df <- data.frame(
  geo = names(sammenligning_all["difference", ]),
  difference = as.numeric(sammenligning_all["difference", ])
)

forskel_df$difference <- round(forskel_df$difference, 3)

########### 3) Find landet med største fald ###########

max_fall_land <- names(which.min(forskel_df$difference))
max_fall_val  <- min(forskel_df$difference)

########### 4) Plot ###########

ggplot(forskel_df,
       aes(x = reorder(geo, -difference),
           y = difference,
           fill = geo)) +
  geom_col() +
  labs(
    title = "Ændring i gennemsnitlig årlig realvækst når coronaperioden fjernes",
    subtitle = paste0("2000Q1–2025Q3: ", max_fall_land,
                      " har det største fald i gennemsnit (", max_fall_val, " procentpoint)"),
    x = "Land",
    y = "Ændring i gennemsnit (%)",
    caption = "Kilde: Eurostat (namq_10_fcs)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

