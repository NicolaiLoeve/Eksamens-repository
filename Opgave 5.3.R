########### Opgave 5.3 ###############
# laver en ny vektor kun med gennemsnittet for hvert land 
gns_alle <- colMeans(df_EUwide[,-1], na.rm = TRUE)

# definerer coronaperioden
corona_periode <- c("2020-Q2","2020-Q3","2020-Q4","2021-Q1","2021-Q2","2021-Q3","2021-Q4")

# laver en ny df uden coronaperioden 
df_no_covid <- df_EUwide[!(df_EUwide$time %in% corona_periode), ]

# laver en ny vektor med gennemsnittet for hvert land uden corona 
gns_no_covid <- colMeans(df_no_covid[,-1], na.rm = TRUE)

# sammenligner de to med og uden corona 
sammenligning <- rbind(
  alle = gns_alle,
  no_covid = gns_no_covid,
  difference = gns_no_covid - gns_alle
)

max_country <- names(which.max(abs(sammenligning["difference", ])))
max_country

##### forklaring af overstående kode:
# comparison["difference", ]: henter rækken "difference" = vektor af lande
# abs: laver absolutværdier (ignorerer minus)
# which.max: finder positionen for det største tal
# names: finder navnet for posititionen med det største tal


##### HER PLOTTER VI! #####
# 8) Gør difference-rækken til data frame til ggplot
forskel_df <- data.frame(
  geo       = names(sammenligning["difference", ]),
  difference = as.numeric(sammenligning["difference", ])
)

# runder forskellen lidt af til flot aksetal
forskel_df$difference <- round(forskel_df$difference, 3)

# dansk navn i undertitel
land_dk <- recode(max_country,
                  "DK" = "Danmark",
                  "SE" = "Sverige",
                  "NO" = "Norge",
                  "FI" = "Finland",
                  "DE" = "Tyskland",
                  "ES" = "Spanien",
                  "FR" = "Frankrig",
                  "IT" = "Italien",
                  "NL" = "Nederlandene",
                  "AT" = "Østrig")

# 9) Søjlediagram nu til 2025Q3
ggplot(forskel_df,
       aes(x = reorder(geo, -difference),
           y = difference,
           fill = geo)) +
  geom_col() +
  labs(
    title    = "Ændring i gennemsnitlig årlig realvækst når coronaperioden fjernes",
    subtitle = paste0("2000Q1–2025Q3: ", land_dk, " (", max_country, ") har den største difference"),
    x        = "Land",
    y        = "Forskel i gennemsnit (%)",
    caption  = "Kilde: Eurostat (namq_10_fcs)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title   = element_text(face = "bold", size = 14),
    plot.subtitle= element_text(size = 11)
  )

