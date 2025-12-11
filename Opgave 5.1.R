# Opgave 5
## 5.1 Kvartalsvis årlig realvækst for en række Eurolande

# Pakker
install.packages("eurostat")
install.packages("restatapi")
install.packages("tidyverse")

library(restatapi)
library(eurostat)
library(tidyverse)

# Henter Eurostats “table of contents” 
#(metadata-katalog over tilgængelige datasæt) som en tabel/data frame med bl.a. title, code, type m.m.
toc <- get_eurostat_toc()

myseachES <- function(searchstr){
  tmpdf <- toc %>% filter(str_detect(title,searchstr)) %>% 
    filter(type=="dataset") %>% 
    select(title,code)
  return(tmpdf)
}
resdf <- myseachES("ousehold") %>% 
  filter(str_detect(title,"expen"))

#############################################################
# Efter at have kigget i forskellige sæt har vi valgt at arbejde med namq_10_fcs
## jaaaaaa

dfnamq <- get_eurostat_data("namq_10_fcs") # loader selve datasættet i dfnamq
namMeta <- get_eurostat_dsd("namq_10_fcs") # får metadata ned så vi kan undersøge variable når vi skal til at sortere

# vi vil gerne kigge nærmere på variable og fjerne dem vi ikke skal bruge 
unique(namMeta$concept)
# "freq"    "unit"    "s_adj"   "na_item" "geo"   

unique(dfnamq$unit)
dfnamqSub <- dfnamq %>% filter(unit=="CLV20_MEUR") #vælger denne unit som den eneste

unique(dfnamq$s_adj)
dfnamqSub <- dfnamqSub %>% filter(s_adj=="SCA") #vælger SCA da den er korrigere for både kalender og sæsons støj

unique(dfnamq$na_item)
dfnamqSub <- dfnamqSub %>% filter(na_item =="P31_S14")

unique(dfnamq$geo)
dfnamqSub <- dfnamqSub %>% 
  filter(geo %in% c("DK","BE","SE","NL","AT","DE","FR","IT","ES"))

unique(dfnamq$time)
dfnamqSub <- dfnamqSub %>% 
  mutate(time = as.character(time)) %>% 
  filter(time >= "1999-Q1", time <= "2025-Q3") ### HER KAN MAN VÆLGE DEN PERIODE MAN VIL HAVE

###### Opretter ny df med kun relevante kolonner #############

df_EU <- dfnamqSub %>%
  select(geo, time, values)

df_EUR <- df_EU %>%
  arrange(geo, time) %>%   # sørg for at hvert land er i rigtig rækkefølge
  group_by(geo) %>%        # regn for hvert land for sig
  mutate(realvaekst = (values / lag(values, 4) - 1) * 100) %>%
  ungroup()

######################## Nu prøver vi at plotte vores data ##################

df_EUR <- df_EUR %>%
  filter(!is.na(realvaekst))

library(ggplot2)
ggplot(df_EUR, aes(x = time, y = realvaekst, color = geo, group = geo)) +
  geom_line() +
  scale_x_discrete(
    breaks = df_EUR$time[grepl("Q1", df_EUR$time)],    # kun Q1 hvert år
    labels = substr(df_EUR$time[grepl("Q1", df_EUR$time)], 1, 4) # kun år
  ) +
  labs(x = "År", y = "Realvækst (%)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))


############### Nu opdaterer vi vores plot, så det det æstetisk ser lidt pænere ud ########

ggplot(df_EUR, aes(x = time, y = realvaekst, color = geo, group = geo)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_discrete(
    breaks = df_EUR$time[grepl("Q1", df_EUR$time)],
    labels = substr(df_EUR$time[grepl("Q1", df_EUR$time)], 1, 4),
  ) +
  labs(
    title = "Stabil realvækst i husholdningernes forbrug – afbrudt af finanskrisen og COVID-19",
    subtitle = "Fra 2000Q1–2025Q3, baseret på 9 EU lande",
    x = "År",
    y = "Årlig realvækst (%)",
    color = "Land",
    caption = "Kilde: Eurostat (namq_10_fcs) – Beregnet ud fra CLV20_MEUR, SCA, P31_S14. Data hentet via Eurostat API.",
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))

################################ Nyt plot med kun 5 lande, så vi har et bedre overblik over grafen
###### Udvælger 5 lande og perioden til og med 2025Q3 ######
dfnamqSub <- dfnamq %>% 
  filter(unit == "CLV20_MEUR",
         s_adj == "SCA",
         na_item == "P31_S14",
         geo %in% c("DE","DK","ES","IT","SE")) %>% 
  mutate(time = as.character(time)) %>% 
  filter(time >= "1999-Q1", time <= "2025-Q3")

df_EU <- dfnamqSub %>%
  select(geo, time, values)

df_EUR <- df_EU %>%
  arrange(geo, time) %>%           # korrekt rækkefølge
  group_by(geo) %>%                # årlig vækst for hvert land
  mutate(realvaekst = (values / lag(values, 4) - 1) * 100) %>%
  ungroup() %>% 
  filter(!is.na(realvaekst))

###### Plot som i dit eksempel – nu til 2025Q3 og med 5 lande ######
library(ggplot2)

ggplot(df_EUR, aes(x = time, y = realvaekst, color = geo, group = geo)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_discrete(
    breaks = df_EUR$time[grepl("Q1", df_EUR$time)],
    labels = substr(df_EUR$time[grepl("Q1", df_EUR$time)], 1, 4)
  ) +
  labs(
    title    = "Stabil realvækst i husholdningernes forbrug – afbrudt af finanskrisen og COVID-19",
    subtitle = "Fra 2000Q1–2025Q3, baseret på 5 EU lande",
    x        = "År",
    y        = "Årlig realvækst (%)",
    color    = "Land",
    caption  = "Kilde: Eurostat (namq_10_fcs) – Beregnet ud fra CLV20_MEUR, SCA, P31_S14. Data hentet via Eurostat API."
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

