# --- AFW API COLLECT SCRIPT (VERSION 2: API + LOG + DB-CONNECT TEST) ---

library(httr)
library(jsonlite)
library(DBI)
library(RMySQL)

# Din API key - HUSK Bearer foran
api_key <- "Bearer SG_APIM_616QEBX83TYEECB5XRTWH5G5GRXRV949BWG54HBKAJTK6FHJ1VW0"

# Logfil
log_file <- "C:/afw_project/logs/afw_collect.log"

log_msg <- function(msg) {
  line <- paste(Sys.time(), "-", msg)
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}

# Hent data for ÉT postnummer (2300 Amager)
zipcode <- "2300"
url <- paste0("https://api.sallinggroup.com/v1/food-waste/?zip=", zipcode)

log_msg(paste("Starter API-kald for postnummer", zipcode))

res <- GET(url, add_headers(Authorization = api_key))
code <- status_code(res)
log_msg(paste("HTTP status:", code))

if (code != 200) {
  log_msg("FEJL: API svarede ikke med 200 OK")
} else {
  txt <- content(res, as = "text", encoding = "UTF-8")
  data <- fromJSON(txt, flatten = TRUE)

  # Stamdata om butikken fra API'et
  store_id_api   <- data$`store.id`
  store_name     <- data$`store.name`
  store_street   <- data$`store.address.street`
  store_zip      <- data$`store.address.zip`
  store_city     <- data$`store.address.city`
  store_brand    <- data$`store.brand`

  log_msg(paste(
    "Butik fra API:",
    store_name, "|", store_street, store_zip, store_city, "| brand:", store_brand, "| id:", store_id_api
  ))


  if (!is.null(data$clearances)) {
    n_offers <- length(data$clearances)
    log_msg(paste("Antal tilbud hentet:", n_offers))
  } else {
    log_msg("Ingen 'clearances' felt i svar")
  }
}
# Test af DB-forbindelse
log_msg("Forsøger at forbinde til MySQL")

con <- NULL
try({
  con <- dbConnect(
    RMySQL::MySQL(),
    user = "root",
    password = "Tfx58kzd.EK.Stud.dk",
    host = "127.0.0.1",
    dbname = "salling_afw",
    port = 3306
  )
}, silent = TRUE)

if (is.null(con)) {
  log_msg("FEJL: Kunne ikke forbinde til MySQL")
} else {
  log_msg("MySQL-forbindelse OK")

  # Loop over alle butikker fra API'et
  n_stores <- length(store_id_api)
  log_msg(paste("Antal butikker fra API:", n_stores))

  for (i in seq_len(n_stores)) {
    s_id    <- store_id_api[i]
    s_name  <- store_name[i]
    s_addr  <- store_street[i]
    s_zip   <- store_zip[i]
    s_city  <- store_city[i]
    s_brand <- store_brand[i]

    # Lidt simpel beskyttelse mod ' i tekst (SQL)
    s_name  <- gsub("'", "''", s_name,  fixed = TRUE)
    s_addr  <- gsub("'", "''", s_addr,  fixed = TRUE)
    s_city  <- gsub("'", "''", s_city,  fixed = TRUE)
    s_brand <- gsub("'", "''", s_brand, fixed = TRUE)

    sql <- sprintf(
      "INSERT INTO stores (salling_store_id, name, address, zipcode, city, chain)
       VALUES ('%s', '%s', '%s', '%s', '%s', '%s')
       ON DUPLICATE KEY UPDATE
         name = VALUES(name),
         address = VALUES(address),
         zipcode = VALUES(zipcode),
         city = VALUES(city),
         chain = VALUES(chain);",
      s_id, s_name, s_addr, s_zip, s_city, s_brand
    )

    dbExecute(con, sql)
    log_msg(paste("Indsat/opf. butik i DB:", s_name, "|", s_zip, s_city))
  }

  dbDisconnect(con)
}

log_msg("Script færdigt")
