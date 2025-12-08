## Opg. 2

install.packages("RMariaDB")
library(DBI)
library(RMariaDB)
library(dplyr)
library(lubridate)


## forbindelse
con <- dbConnect(MariaDB(),
                 host="localhost",
                 db="cars",
                 user="root",
                 password=Sys.getenv("localpw")
)

###############################################################

library(dplyr)

# Ikke-variable

bildf <- colldf %>%
  select(
    carid,
    makemodel,
    reg_year,
    km_km,
    km_per_l
  )

######### Ind i Workbench

dbWriteTable(con, "Cars table", bildf, overwrite=T)

######### Variable

library(dplyr)
library(readr)

dfscrapeinfo <- colldf %>%
  transmute(
    carid,
    price = parse_number(
      as.character(price),
      locale = locale(grouping_mark = ".", decimal_mark = ",")
    ),
    dato = as.Date("2025-11-20")
  )

## Laver den simulerede
dfscrapesim <- nyscrape1 %>%
  transmute(
    carid,
    price = parse_number(
      as.character(price),
      locale = locale(grouping_mark = ".", decimal_mark = ",")
    ),
    dato = as.Date("2025-11-21")
  )

##### Ind i Workbench med variable

dbWriteTable(con, "Scrape table", dfscrapeinfo, overwrite=T)

##### Laver simuleringer

sold <- dfscrapeinfo %>% 
  anti_join(dfscrapesim, by = "carid") %>%
  mutate(change_type = "Sold")

new_cars <- dfscrapesim %>% 
  anti_join(dfscrapeinfo, by = "carid") %>%
  mutate(change_type = "New listing")

price_changes <- dfscrapeinfo %>%
  inner_join(dfscrapesim, by = "carid", suffix = c("_old", "_new")) %>%
  filter(price_old != price_new) %>%
  mutate(change_type = "Price changed")

diff_df <- bind_rows(
  sold,
  new_cars,
  price_changes
)

dbWriteTable(con, "Diff table", diff_df, overwrite=T)
