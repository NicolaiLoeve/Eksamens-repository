#### Downloader dkstat

install.packages(
  "dkstat",
  repos = c(
    ropengov = "https://ropengov.r-universe.dev",
    getOption("repos")
  )
)
library(dkstat)
library(dplyr)

## Henter Meta Data med henblik på filtrering.
alko_meta <- dst_meta(table = "FU02", lang = "da")

#Kontruer liste med filtre

alko_meta_filters <- list(
  KONSUMGRP = c("02.1.1.1 Spiritus og likør",
                "02.1.1.2 Alkoholiske læskedrikke",
                "02.1.2.1 Vin af druer",
                "02.1.2.2 Vin af andre frugter",
                "02.1.2.3 Hedvin",
                "02.1.2.4 Vinbaserede drikkevarer og alkoholfri vin",
                "02.1.3.1 Pilsnerøl, guldøl",
                "02.1.3.2 Andre alkoholholdige øl",
                "02.1.3.3 Øl med lavt alkoholindhold og alkoholfri øl",
                "02.1.3.4 Øl-baserede drikkevarer"),
  PRISENHED = "Faste priser",
  Tid = "*"
)

#Hente data via filtres
alkodata <- dst_get_data(table = "FU02", query = alko_meta_filters, lang = "da")
