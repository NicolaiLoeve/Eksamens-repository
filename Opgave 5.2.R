##### 5.2 ##########################
# lægger tabellen ned 
df_EUwide <- df_EUR %>%
  select(geo, time, realvaekst) %>%   # behold kun det nødvendige
  pivot_wider(
    names_from = geo,             # kolonnenavne = lande
    values_from = realvaekst          # celler = forbrugsværdier
  )

# laver summary 
summary(df_EUwide)

# Sverige har det højeste gennemsnit i realvækst 

#En lille tabel der viser hvem der har det højeste gennemsnit i rækkefølge.
df_mean <- df_EUR %>% 
  filter(time >= "2000-Q1", time <= "2024-Q2") %>%   # sikrer korrekt periode
  group_by(geo) %>% 
  summarise(mean_realvaekst = mean(realvaekst, na.rm = TRUE)) %>% 
  arrange(desc(mean_realvaekst))

df_mean


#####Vi laver et søjlediagram for at visualisere det.
ggplot(df_mean, aes(x = reorder(geo, -mean_realvaekst),
                    y = mean_realvaekst,
                    fill = geo)) +
  geom_col() +
  labs(
    title    = "Sverige har den højest gennemsnitlige årlige realvækst i husholdningernes forbrug",
    subtitle = "2000Q1–2025Q3",
    x        = "Land",
    y        = "Gennemsnitlig vækst (%)",
    fill     = "Land",
    caption  = "Kilde: Eurostat (namq_10_fcs)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )
