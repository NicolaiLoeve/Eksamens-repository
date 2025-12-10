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

###
# --- LOOP OVER ZIPCODES ---
zipcodes <- c("2300", "2200", "2100", "2000")

for (zipcode in zipcodes) {

  log_msg(paste("=== Starter API-kald for postnummer:", zipcode, "==="))

  url <- paste0("https://api.sallinggroup.com/v1/food-waste/?zip=", zipcode)

  res <- GET(url, add_headers(Authorization = api_key))
  code <- status_code(res)
  log_msg(paste("HTTP status for", zipcode, ":", code))

  if (code != 200) {
    log_msg(paste("FEJL: API svarede ikke korrekt for", zipcode))
    next
  }

  # PARSE DATA
  txt  <- content(res, as = "text", encoding = "UTF-8")
  data <- fromJSON(txt, flatten = TRUE)

  # Butiksdata
  store_id_api  <- data$`store.id`
  store_name    <- data$`store.name`
  store_street  <- data$`store.address.street`
  store_zip     <- data$`store.address.zip`
  store_city    <- data$`store.address.city`
  store_brand   <- data$`store.brand`

  if (!is.null(data$clearances)) {
     n_offers <- length(data$clearances)
     log_msg(paste("Tilbud fundet i", zipcode, ":", n_offers))
  } else {
     log_msg(paste("Ingen tilbud fundet i", zipcode))
     next
  }

# Test af DB-forbindelse
  # -------- DATABASE-FORBINDELSE --------
  con <- NULL
  try({
    con <- dbConnect(
      RMySQL::MySQL(),
      user = "root",
      password = "Tfx58kzd.EK.Stud.dk",   # <- brug dit korrekte password
      host = "127.0.0.1",
      dbname = "salling_afw",
      port = 3306
    )
    log_msg("DB-forbindelse OK")
  }, silent = TRUE)

  if (is.null(con)) {
    log_msg("Kunne ikke forbinde til MySQL - springer postnummer over")
    next
  }
  # -------- INDSÆT ALLE BUTIKKER --------
  n_stores <- length(store_id_api)
  log_msg(paste("Butikker fundet i", zipcode, ":", n_stores))

  for (i in seq_len(n_stores)) {

    s_id    <- store_id_api[i]
    s_name  <- gsub("'", "''", store_name[i])
    s_addr  <- gsub("'", "''", store_street[i])
    s_zip   <- store_zip[i]
    s_city  <- gsub("'", "''", store_city[i])
    s_brand <- gsub("'", "''", store_brand[i])

    sql_store <- sprintf("
      INSERT INTO stores (salling_store_id, name, address, zipcode, city, chain)
      VALUES ('%s','%s','%s','%s','%s','%s')
      ON DUPLICATE KEY UPDATE
        name=VALUES(name),
        address=VALUES(address),
        zipcode=VALUES(zipcode),
        city=VALUES(city),
        chain=VALUES(chain);
    ", s_id, s_name, s_addr, s_zip, s_city, s_brand)

    dbExecute(con, sql_store)
    log_msg(paste("Gemt butik:", s_name, "|", s_zip, s_city))
  }
  # -------- PRODUKTER OG OFFERS --------

    # -------- PRODUKTER OG OFFERS --------

  offers <- data$clearances

  for (j in seq_len(n_offers)) {

    off <- offers[[j]]

    # Hjælpefunktioner til at undgå NULL
    safe_str <- function(x) {
      if (is.null(x) || length(x) == 0) return("")
      y <- as.character(x)[1]
      if (is.na(y)) y <- ""
      gsub("'", "''", y, fixed = TRUE)
    }

    safe_num <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA)
      as.numeric(x)[1]
    }

        safe_time <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA)
      s <- as.character(x)[1]
      if (is.na(s) || s == "") return(NA)

      # API-format:  2025-12-04T06:36:31.000Z
      # Vi laver det om til: 2025-12-04 06:36:31
      s <- sub("Z$", "", s)          # fjern evt. Z til sidst
      s <- sub("\\.000$", "", s)     # fjern .000 hvis det findes
      s <- sub("T", " ", s)          # skift T til mellemrum

      return(s)
    }


    # PRODUKT
    prod_ean  <- safe_str(off$product.ean)
    prod_desc <- safe_str(off$product.description)

    sql_prod <- sprintf("
      INSERT INTO products (salling_product_id, name)
      VALUES ('%s', '%s')
      ON DUPLICATE KEY UPDATE
        name = VALUES(name);
    ",
      prod_ean, prod_desc
    )
    dbExecute(con, sql_prod)

    # OFFER-felter (alle via safe_*)
    store_id_api_val <- safe_str(store_id_api[j])
    new_price     <- safe_num(off$offer.newPrice)
    orig_price    <- safe_num(off$offer.originalPrice)
    disc_pct      <- safe_num(off$offer.percentDiscount)
    stock_qty     <- safe_num(off$offer.stock)
    start_time    <- safe_time(off$offer.startTime)
    end_time      <- safe_time(off$offer.endTime)

    sql_offer <- sprintf("
      INSERT INTO offers (
        store_id,
        product_id,
        price,
        original_price,
        discount_pct,
        quantity,
        valid_from,
        valid_to,
        observed_at
      )
      VALUES (
        (SELECT store_id FROM stores WHERE salling_store_id = '%s'),
        (SELECT product_id FROM products WHERE salling_product_id = '%s'),
        %s,
        %s,
        %s,
        %s,
        %s,
        %s,
        NOW()
      );
    ",
      store_id_api_val,
      prod_ean,
      ifelse(is.na(new_price), "NULL", as.character(new_price)),
      ifelse(is.na(orig_price), "NULL", as.character(orig_price)),
      ifelse(is.na(disc_pct), "NULL", as.character(disc_pct)),
      ifelse(is.na(stock_qty), "NULL", as.character(stock_qty)),
      ifelse(is.na(start_time), "NULL", sprintf("'%s'", start_time)),
      ifelse(is.na(end_time), "NULL", sprintf("'%s'", end_time))
    )

    dbExecute(con, sql_offer)
  }

  log_msg(paste("Indsat", n_offers, "offers og produkter for", zipcode))


  dbDisconnect(con)
  log_msg(paste("Færdig med postnummer:", zipcode))
}  # <-- afslutning på zipcode-loopet

