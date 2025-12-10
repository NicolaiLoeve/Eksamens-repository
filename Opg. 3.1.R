library(dplyr)
library(ggplot2)
library(tibble)

##########################################################
# 1. Forbered data: ændringer i realvækst (DI)
##########################################################

di_changes <- ftidiq_wide %>%
  # Afgræns den periode, vi vil kigge på
  filter(KVARTAL >= "1998K1", KVARTAL <= "2025K3") %>%
  arrange(KVARTAL) %>%
  
  # Ændring i årlig realvækst fra kvartal t-1 til t
  mutate(
    delta_realvækst = realvækst - lag(realvækst),
    
    # Logisk variabel: TRUE = realvæksten stiger
    stiger = delta_realvækst > 0
  ) %>%
  
  # Første observation har ingen lag-værdi, så den filtrerer vi fra
  filter(!is.na(delta_realvækst))

##########################################################
# 2. Optæl hvor ofte realvæksten stiger/falder
##########################################################

di_summary <- di_changes %>%
  summarise(
    antal_stiger = sum(stiger),
    antal_falder = sum(!stiger),
    total_obs    = n()
  ) %>%
  mutate(
    andel_stiger = antal_stiger / total_obs,
    andel_falder = antal_falder / total_obs
  )

di_summary
#> giver dig både antal og andele i én tabel

##########################################################
# 3. Data til cirkeldiagram
##########################################################

df_pie <- tibble(
  kategori = c("Stiger", "Falder/uændret"),
  procent  = c(di_summary$andel_stiger, di_summary$andel_falder) * 100
) %>%
  mutate(
    label = sprintf("%s: %.1f%%", kategori, procent)
  )

##########################################################
# 4. Plot cirkeldiagram
##########################################################

ggplot(df_pie, aes(x = "", y = procent, fill = kategori)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  geom_label(
    aes(label = label),
    position    = position_stack(vjust = 0.5),
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("Stiger"          = "steelblue",
                               "Falder/uændret" = "red")) +
  labs(
    title    = "Fordeling af ændringer i årlig realvækst (DI)",
    subtitle = "Stigning vs. fald/uændret fra ét kvartal til det næste",
    caption  = "Egen beregning på baggrund af DI-baseret realvækst"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption  = element_text(hjust = 0)
  )