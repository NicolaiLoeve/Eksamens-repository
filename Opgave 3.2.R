##########Opgave 3.2 #######

#I tager udgangspunkt i ét fly og henter nu tracks for det givne fly.
##Gør det samme for ét af de cirklende fly. Plot nu begge og kommenter.

# Finder den rigtige icao-kolonne 
if ("icao24" %in% names(statedfpr)) {
  icaov <- statedfpr$icao24
} else {
  icaov <- statedfpr$V1
}

# Kigger på de første par værdier
head(icaov)
length(icaov)   # hvor mange fly har vi?


getTrack <- function(icao) {
  # 1. Hent token
  token <- getToken()
  
  # 2. Byg URL til OpenSky tracks/all endpoint
  turl <- paste0(baseurl, endpointT, "?icao24=", icao, "&time=0")
  
  # 3. Send HTTP GET-request med token som Authorization header
  res <- httr::GET(turl, add_headers(Authorization = paste("Bearer", token)))
  
  # 4. Tjekker om det gik godt (status 200 = OK)
  if (res$status_code != 200) {
    message("Ingen track (status ", res$status_code, ") for: ", icao)
    return(NULL)
  }
  
  # 5. Parse JSON-svar til R-objekt
  result <- jsonlite::fromJSON(httr::content(res, as = "text"))
  
  # 6. Lav data.frame ud af resultatet
  df <- as.data.frame(result)
  
  # Hvis der ikke er nogen rækker, er der ingen track
  if (nrow(df) == 0) {
    message("Tomt track for: ", icao)
    return(NULL)
  }
  
  # 7. Sætter pæne kolonnenavne på
  colnames(df) <- c("icao24","callsign","startTime","endTime",
                    "time","lat","lng","alt","crs","on_ground")
  
  return(df)
}

####
# Funktion der finder det første fly i icaov med et gyldigt track
getFirstWorkingTrack <- function(icaov, max_try = 20) {
  n <- min(length(icaov), max_try)
  
  for (i in seq_len(n)) {
    icao <- icaov[[i]]
    message("Prøver fly nr. ", i, " med icao: ", icao)
    
    df <- getTrack(icao)
    
    if (!is.null(df) && nrow(df) > 0) {
      message("✅ Fandt track for: ", icao)
      return(df)
    }
  }
  
  stop("Ingen gyldige tracks blandt de første ", n, " fly.")
}

# Brug den til at få vores 'friske' fly-data:
fresh_df <- getFirstWorkingTrack(icaov)

# Hvilket fly blev det?
unique(fresh_df$icao24)
head(fresh_df)


#####
# Læs træningsfly (cirkelfly) fra rds-fil
circle_df <- readRDS("circjet2.rds")

# Tjek strukturen (frivilligt)
head(circle_df)



# Kort-plot af ruten
myMapPlot <- function(df) {
  leaflet(df) %>% 
    addTiles() %>%                    # baggrundskort
    addPolylines(lat = ~lat, lng = ~lng)  # tegner ruten som en streg
}

# Enkel EDA: vis tre aspekter
myEDAPlots <- function(df) {
  par(mfcol = c(1,3), ann = FALSE)
  
  # 1) Rute: latitude vs longitude
  plot(df$lng, df$lat, main = "Rute (lat/lng)")
  
  # 2) Kurs over tid
  plot(df$time, df$crs, main = "Kurs (crs) over tid")
  
  # 3) Højde over tid
  plot(df$time, df$alt, main = "Højde (alt) over tid")
}


####Plotter begge fly - Først frisk fly
# Plots for det friske fly
myMapPlot(fresh_df)
myEDAPlots(fresh_df)

####Nu plotter vi cirklerende fly
# Plots for cirkelflyet
myMapPlot(circle_df)
myEDAPlots(circle_df)

