library(dplyr)
library(stringr)
library(tidyr)
library(readr)

## --- Opgave 1.3 – Simuleret ny scraping ---

## Trin 1 - lav ny df til simulation af ændringer i annoncer
nyscrape1 <- colldf

## Trin 2 - sæt scrapedate en dag frem
nyscrape1 <- nyscrape1 %>%
  mutate(
    scrapedate = as.Date(scrapdate) + 1
  )

## Trin 3 - samme udvælgelse hver gang
set.seed(123)

## Trin 4 - formater pris som numerisk
nyscrape1 <- nyscrape1 %>%
  mutate(
    price = parse_number(
      as.character(price),
      locale = locale(grouping_mark = ".", decimal_mark = ",")
    )
  )

## --- SÆLG 5 BILER ---
sold_ids <- sample(nyscrape1$carid, 5) # vælg tilfældige 5 bil-id'er der “bliver solgt”

nyscrape1_sold_removed <- nyscrape1 %>%        # fjern de solgte biler fra dag-2-datasættet
  filter(!carid %in% sold_ids)

## --- ÆNDRE PRIS PÅ 3 BILER MED -5% ---
change_ids <- sample(nyscrape1_sold_removed$carid, 3) # Vælg 3 tilfældige biler til prisændring

nyscrape1_changed <- nyscrape1_sold_removed %>%
  mutate(
    price = ifelse(
      carid %in% change_ids,
      round(price * 0.95),   # 5% prisfald
      price
    )
  )

## --- TILFØJ 2 NYE BILER ---
new_rows <- nyscrape1_changed %>%
  slice(1:2) %>%                          # Tager udgangspunkt i de 2 første rækker og bruger dem som skabelon til de 2 nye biler vi laver
  mutate(
    carid = sprintf("%07d", sample(1e6:9e6, n(), replace = FALSE)),  # Laver 2 nye carid
    price = price,          # pris som tal
    scrapedate = scrapedate # stadig +1 dag
  )

## --- SAMLE NYE BILER MED RESTEN ---
nyscrape1 <- bind_rows(nyscrape1_changed, new_rows)  

## Trin 6 - split details
split_details <- function(df) {           # Splitter details/properties så vi får nye variable for hver istedet for én samlet
  df %>%
    mutate(
      reg_month = str_extract(details, "^[0-9]{1,2}(?=/)") %>% as.integer(),
      reg_year  = str_extract(details, "(?<=/)[0-9]{4}")   %>% as.integer(),
      km        = str_extract(details, "[0-9.]+(?= km)") %>%
        gsub("\\.", "", .) %>% as.integer(),
      km_per_l  = str_extract(details, "[0-9,]+(?= km/l)") %>%
        gsub(",", ".", .) %>% as.numeric()
    )
}

## (Du kan nu gøre:)
nyscrape1 <- split_details(nyscrape1)

######################

# Solgte biler
sold2 <- setdiff(colldf$carid, nyscrape1$carid) 

# Nye biler
new <- setdiff(nyscrape1$carid, colldf$carid)

# Prisændringer (kun på biler der findes i begge dataframes)
price_changes <- colldf %>%
  inner_join(nyscrape1, by = "carid", suffix = c("_day1", "_day2")) %>%
  filter(price_day1 != price_day2) %>%
  pull(carid)

# Uændrede biler
unchanged <- setdiff(intersect(colldf$carid, nyscrape1$carid), price_changes)

status_df <- data.frame(
  status = c("Solgte biler", "Nye biler", "Prisændrede biler", "Uændrede biler"),
  count  = c(length(sold2), length(new), length(price_changes), length(unchanged))
)
status_df

library(ggplot2)

ggplot(status_df, aes(x = status, y = count, fill = status)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0, max(status_df$count), by = 10)) + 
  labs(
    title = "Statusoversigt: Ændringer fra dag 1 til dag 2",
    x = "",
    y = "Antal biler",
    caption = "Kilde: Bilbasen.dk"   # <-- kildehenvisning
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")  # fin stil på kilden
  )
