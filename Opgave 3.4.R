##### OPGAVE 3.4 – EGEN ALGORITME TIL CIRKLENDE FLY #####

# Forudsætter:
# - results_all findes fra opgave 3.3 (med kolonnerne icao24, sdcourse, rsq, isOff)
# - getTrack() er defineret (samme som i opgave 3.3)
# Du skal IKKE source Opgave_3.3.R her.

############################
# 1. Funktion: krydser flyet sit eget spor?
############################

hasSelfCrossing <- function(df, dist_thr = 0.02, min_step = 5L) {
  # df skal have kolonnerne lat og lng
  lat <- df$lat
  lng <- df$lng
  n   <- length(lat)
  
  # hvis for få punkter, kan vi ikke sige noget
  if (n < (min_step + 2)) return(FALSE)
  
  # gennemløb ruten
  for (i in 1:(n - min_step - 1)) {
    # sammenlign kun med punkter et stykke fremme i tiden
    j_idx <- (i + min_step):n
    
    dlat <- lat[j_idx] - lat[i]
    dlng <- lng[j_idx] - lng[i]
    
    # “afstand” i grad-rum
    d2 <- dlat^2 + dlng^2
    
    # hvis vi kommer tættere på end dist_thr → krydser eget spor
    if (any(d2 < dist_thr^2, na.rm = TRUE)) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

############################
# 2. Beregn self-cross flag for alle fly i results_all
############################

# start med 0 for alle
results_all$crossFlag <- 0L

for (i in seq_len(nrow(results_all))) {
  icao <- results_all$icao24[i]
  message("Tjekker self-cross for: ", icao)
  
  tr <- getTrack(icao)
  if (is.null(tr)) {
    # hvis vi ikke kan hente track, lader vi crossFlag være 0
    next
  }
  
  results_all$crossFlag[i] <- as.integer(hasSelfCrossing(tr))
}

############################
# 3. Vælg simple thresholds til sdcourse og rsq
############################

# Brug træningsfly (isOff==1) til at finde typiske værdier
sd_thr  <- quantile(results_all$sdcourse[results_all$isOff == 1],
                    0.25, na.rm = TRUE)
rsq_thr <- quantile(results_all$rsq[results_all$isOff == 1],
                    0.75, na.rm = TRUE)

sd_thr
rsq_thr

# Hvis I hellere vil have runde tal, kan I fx gøre:
# sd_thr  <- 60
# rsq_thr <- 0.90

############################
# 4. Definér jeres egen algoritme: myOwnAlg
############################

# Et fly er cirklende hvis:
# - det krydser sit eget spor (crossFlag == 1)
# - kursen varierer meget (sdcourse >= sd_thr)
# - ruten ikke er særligt lineær (rsq <= rsq_thr)

results_all$myOwnAlg <- ifelse(
  results_all$crossFlag == 1 &
    results_all$sdcourse >= sd_thr &
    results_all$rsq      <= rsq_thr,
  1L,
  0L
)

############################
# 5. Byg den endelige dataframe som i opgaveeksemplet
############################

final_df <- data.frame(
  icao     = results_all$icao24,
  sdcourse = results_all$sdcourse,
  lmres    = results_all$rsq,
  myOwnAlg = results_all$myOwnAlg,
  isOff    = results_all$isOff
)

View(final_df)

############################
# 6. Lille evaluering (valgfrit, men godt til rapporten)
############################

# Hvor godt rammer algoritmen de kendte cirkelfly?
table(
  Facit_isOff        = final_df$isOff,
  Algoritme_myOwnAlg = final_df$myOwnAlg
)

