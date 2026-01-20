## Opg. 1.1 – Scrape Mercedes E-klasse fra Bilbasen

## Trin 1 – Load pakker
library(rvest)
library(dplyr)
library(httr)
library(stringr)
library(readr)


## Trin 2 – Definér første side og headers
startlink <- "https://www.bilbasen.dk/brugt/bil/mercedes/ms-e-klasse?includeengroscvr=true&includeleasing=false&pagesize=100"

# Samme headers genbruges til alle requests
std_headers <- add_headers(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36",
  "Cookie" = 'GdprConsent-Custom=eyJ2ZW5kb3JJZHMiOnt9fQ==; _cmp_analytics=0; _cmp_personalisation=0; _cmp_marketing=0; _cmp_advertising=0; bbsession=id=2e9ffa10-f707-4f02-a051-01a3b6d1598d; consentUUID=8ffcb244-b354-492b-9fc0-cf574921de1b_47_49_50; consentDate=2025-11-17T07:47:34.716Z; GdprConsent={"version":"IAB TCF 2.0","consentString":"CQbB_cAQbB_cAAGABBENCCFgAAAAAAAAAAZQAAAAAAAA.YAAAAAAAAAAA"}; bbtracker=id=8e5fe7c8-c6c6-4537-affe-253034c06de1; __eoi=ID=f43ab5922e642a8a:T=1763365654:RT=1763367050:S=AA-AfjagrlFlxB3-cqO3mtc0WVpq; _pulse2data=ad23e785-2b09-4d94-89ae-6d8ae3def12d%2Cv%2C%2C1763971989000%2CeyJpc3N1ZWRBdCI6IjIwMjUtMDgtMjRUMTI6Mjc6NDhaIiwiZW5jIjoiQTEyOENCQy1IUzI1NiIsInJlSXNzdWVkQXQiOiIyMDI1LTExLTE3VDA4OjEzOjA5WiIsImFsZyI6ImRpciIsImtpZCI6IjMifQ..Kx8bCr_P4EhoyvwKgdjfhQ.vj5A5iQVs2_E3gZrPyerVwlBRt3xCS7Reu-tv6mgMwaQtLP2nNV4G_Kn51-L-f03YYtZPrvT3JpFx_RPPDy0VPtoL9wQmJIutOnYW5U1AnjY6Ed0IYb_ikFmGx3U3cJwsH_NlcRqBQ1qKrMwPUu82FPxA_xhiDpyGZOShJ5oxwdWM4THf9FQGEHRCwVqbWHy8HOGure2isO048cF7QdzXA.Nf8uHPj0cc_tDxsHx5T9uQ%2C%2C0%2Ctrue%2C%2C; _pulsesession=%5B%22sdrn%3Aschibsted%3Asession%3A89f0631d-3275-4659-be41-a98e6a6e1737%22%2C1763365611572%2C1763367290133%5D'
)

rawres <- GET(url = startlink, std_headers)            # Hent første side
rawres$status_code                                     # Tjek at vi får 200    
rawcontent <- httr::content(rawres, as = "text")       # Konverter raw HTML til tekst

## Trin 3 – Læs HTML og find bilkortene
page    <- read_html(rawcontent)
carlist <- page %>% html_elements("article")           # # Hvert bilkort ligger i <article>
 
## Trin 4 – Definér CSS-tags til forsiden og bilsiderne

# tags på forsiden
ptag          <- ".Listing_price__6B3kE"
proptag       <- "[class^='Listing_properties']"
mmtag         <- "[class^='Listing_makeModel']"
dettag        <- "[class^='Listing_details']"
dettagogitem  <- "[class^='ListingDetails_listItem']"
desctag       <- "[class^='Listing_description']"
loctag        <- "[class^='Listing_location']"
linktag       <- "[class^='Listing_link']"

# Forhandler-tags (inde på bilsiden)
Seller_tag         <- "div[aria-label='bil sælger']"
Seller_address_tag <- "[data-e2e='seller-address']"
Seller_cvr_tag     <- "[class^='bas-MuiSellerInfoComponent-cvr']"

## Trin 5 – Opret tom dataframe til opsamling
cn <- c(
  "price","makemodel","details","properties","description",
  "location","link","carid","seller_name","seller_address","seller_cvr","scrapdate"
)

colldf <- data.frame(
  matrix(NA, nrow = 0, ncol = length(cn)),
  stringsAsFactors = FALSE
)
colnames(colldf) <- cn

## Trin 6 – Loop over alle biler, hent data + rens description (Opgave 1.2)
for (car in carlist) {
  # --- data fra listen (forsiden) ---
  price     <- car %>% html_element(ptag)    %>% html_text()
  props     <- car %>% html_element(proptag) %>% html_text()
  makemodel <- car %>% html_element(mmtag)   %>% html_text()
  details   <- car %>% html_elements(dettagogitem) %>% 
    html_text() %>% paste0(collapse = "_")
  
  # rå salgstekst (kan være flere elementer)
  raw_description <- car %>% html_elements(desctag) %>% html_text()
  description <- paste(raw_description, collapse = " ")
  
  ## Rens salgsteksten:
  # 1) newline -> ". "
  description <- gsub("[\r\n]+", ". ", description)
  # 2) fjern alle “mærkelige” tegn – behold bogstaver (inkl. æøå), tal, punktum, komma og mellemrum
  description <- gsub("[^0-9A-Za-zÆØÅæøå., ]", " ", description)
  # 3) mange mellemrum -> ét mellemrum
  description <- gsub(" {2,}", " ", description)
  # 4) trim for og bag
  description <- trimws(description)
  
  location <- car %>% html_elements(loctag) %>% html_text()
  link     <- car %>% html_element("a")     %>% html_attr("href")
  carid    <- link %>% str_extract("[0-9]{7}")
  
  # --- hent sælgerdata inde på bilens egen side ---
  rawres_bil <- GET(link, std_headers)
  bilside    <- read_html(content(rawres_bil, as = "text"))
  
  seller_name <- bilside %>%
    html_elements(Seller_tag) %>%
    html_element("h2") %>%
    html_text(trim = TRUE)
  
  seller_address <- bilside %>%
    html_element(Seller_address_tag) %>%
    html_text(trim = TRUE)
  
  seller_cvr <- bilside %>%
    html_element(Seller_cvr_tag) %>%
    html_text(trim = TRUE)
  
  ## Samler alle bilens oplysninger i én række, så den kan tilføjes korrekt til hoved-dataframe 'colldf'

  tmpdf <- data.frame(
    price          = price,
    makemodel      = makemodel,
    details        = details,
    properties     = props,
    description    = description,
    location       = location,
    link           = link,
    carid          = carid,
    seller_name    = seller_name,
    seller_address = seller_address,
    seller_cvr     = seller_cvr,
    scrapdate      = Sys.time(),
    stringsAsFactors = FALSE
  )
  
  colldf <- rbind(colldf, tmpdf)
}

## Trin 7 – Split ‘details’ op i årgang, kilometer og km/l (Opgave 1.2 fortsat)
colldf <- colldf %>%
  mutate(
    # f.eks. "11/2016_176.000 km_18,2 km/l_Automatisk gear_Diesel"
    reg_month = str_extract(details, "^[0-9]{1,2}(?=/)"),
    reg_year  = str_extract(details, "(?<=/)[0-9]{4}"),
    
    km_raw = str_extract(details, "[0-9.]+(?= km)"),
    km_km  = km_raw %>% str_replace_all("\\.", "") %>% as.integer(),
    
    kmpl_raw = str_extract(details, "[0-9]+,[0-9]+(?= km/l)"),
    km_per_l = kmpl_raw %>% str_replace(",", ".") %>% as.numeric()
  ) %>%
  # fjern hjælpekolonner
  select(-km_raw, -kmpl_raw)

## Trin 8 – Vi er ligeglade med registreringsmåned, beholder kun år
colldf <- colldf %>% 
  select(-reg_month) %>%
  mutate(
    # HER gør vi prisen til "rå tal"
    price = readr::parse_number(
      as.character(price),
      locale = locale(grouping_mark = ".", decimal_mark = ",")
    )
  )

## Slutresultat: colldf
colldf

