##### OPGAVE 3.3 – MOVE FAST - ALLE FLY #####

library(httr)
library(jsonlite)

# 0) Henter token-funktion mv. fra util.R
source("util.R")

# 1) Opsætter OpenSky endpoint til tracks
baseurl   <- "https://opensky-network.org/api"
endpointT <- "/tracks/all"


# 2) Laver vektor med alle friske ICAO'er fra statedfpr -----

if ("icao24" %in% names(statedfpr)) {
  icaov <- statedfpr$icao24
} else {
  icaov <- statedfpr$V1
}
icaov <- unique(icaov)


# 3) Funktion til at hente track for ét fly ----------------

getTrack <- function(icao) {
  token <- getToken()
  turl  <- paste0(baseurl, endpointT, "?icao24=", icao, "&time=0")
  
  res <- httr::GET(turl, add_headers(Authorization = paste("Bearer", token)))
  
  # Hvis ikke 200 OK -> ingen track
  if (res$status_code != 200) {
    message("Ingen track (status ", res$status_code, ") for: ", icao)
    return(NULL)
  }
  
  result <- jsonlite::fromJSON(httr::content(res, as = "text"))
  df <- as.data.frame(result)
  
  # Hvis ingen rækker -> ingen track
  if (nrow(df) == 0) {
    message("Tomt track for: ", icao)
    return(NULL)
  }
  
  colnames(df) <- c("icao24","callsign","startTime","endTime",
                    "time","lat","lng","alt","crs","on_ground")
  
  return(df)
}


# 4) Funktion til at beregne metrics for ét track ---------

calcMetrics <- function(df) {
  # Fjern rækker uden lat/lng
  df2 <- subset(df, !is.na(lat) & !is.na(lng))
  
  # For få punkter -> ingen mening
  if (nrow(df2) < 2) {
    return(list(
      sdcourse = NA_real_,
      rsq      = NA_real_
    ))
  }
  
  # Standardafvigelse af kurs
  sdcourse <- sd(df2$crs, na.rm = TRUE)
  
  # R^2 fra lm(lat ~ lng)
  rsq <- tryCatch(
    {
      model <- lm(lat ~ lng, data = df2)
      summary(model)$r.squared
    },
    error = function(e) {
      NA_real_
    }
  )
  
  # Tving til længde 1 numerisk
  sdcourse <- as.numeric(sdcourse[1])
  rsq      <- as.numeric(rsq[1])
  
  return(list(
    sdcourse = sdcourse,
    rsq      = rsq
  ))
}


# 5) LOOP: beregner metrics for FRISKE fly -------------------

results_fresh <- data.frame(
  icao24   = character(),
  sdcourse = numeric(),
  rsq      = numeric(),
  isOff    = integer(),
  stringsAsFactors = FALSE
)

max_fresh <- min(length(icaov), 80)   # fx maks 80 fly (kan sættes op senere)

for (i in seq_len(max_fresh)) {
  icao <- icaov[i]
  message("Frisk fly nr. ", i, " (icao = ", icao, ")")
  
  track_df <- getTrack(icao)
  
  # Hvis ingen track -> spring over
  if (is.null(track_df)) next
  
  # Beregn metrics
  m <- calcMetrics(track_df)
  
  # Hvis metrics er helt ubrugelige -> spring over
  if (is.null(m) ||
      length(m$sdcourse) != 1 ||
      length(m$rsq)      != 1 ||
      (is.na(m$sdcourse) && is.na(m$rsq))) {
    
    message("  -> skipper ", icao, " pga. manglende/ugyldige metrics")
    next
  }
  
  # Læg række på results_fresh
  results_fresh <- rbind(
    results_fresh,
    data.frame(
      icao24   = as.character(icao),
      sdcourse = as.numeric(m$sdcourse),
      rsq      = as.numeric(m$rsq),
      isOff    = 0L,      # 0 = frisk fly
      stringsAsFactors = FALSE
    )
  )
}

# Tjekker hvordan det ser ud
print(dim(results_fresh))
print(head(results_fresh))



# 6) LOOP: beregner metrics for TRÆNINGS-fly -----------------

# Find alle rds-filer
fl_train <- list.files(path = ".", pattern = "^circ.*\\.rds$", full.names = TRUE)

train_list <- lapply(fl_train, readRDS)

results_train <- data.frame(
  icao24   = character(),
  sdcourse = numeric(),
  rsq      = numeric(),
  isOff    = integer(),
  stringsAsFactors = FALSE
)

for (j in seq_along(train_list)) {
  df <- train_list[[j]]
  
  this_icao <- as.character(df$icao24[1])
  message("Træningsfly nr. ", j, " (", this_icao, ")")
  
  m <- calcMetrics(df)
  
  if (is.null(m) ||
      length(m$sdcourse) != 1 ||
      length(m$rsq)      != 1 ||
      (is.na(m$sdcourse) && is.na(m$rsq))) {
    
    message("  -> skipper træningsfly nr. ", j, " pga. manglende/ugyldige metrics")
    next
  }
  
  results_train <- rbind(
    results_train,
    data.frame(
      icao24   = this_icao,
      sdcourse = as.numeric(m$sdcourse),
      rsq      = as.numeric(m$rsq),
      isOff    = 1L,        # 1 = cirkelfly (træning)
      stringsAsFactors = FALSE
    )
  )
}

print(dim(results_train))
print(head(results_train))



# 7) Kombinére FRISKE + TRÆNINGS-fly ------------------------

results_all <- rbind(results_fresh, results_train)

print(dim(results_all))
print(head(results_all))
table(results_all$isOff)

