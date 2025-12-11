## Stormen okt 2023 – vindstyrke + vindretning med pile og labels
#######################

# FORMÅL MED opgaven:
# 1) Hente vinddata (vindhastighed og vindretning) fra DMI's API for 2 stationer
#    i perioden omkring stormen i oktober 2023.
# 2) Samle data i ét datasæt.
# 3) Konvertere tid og omkode vindretning til kompas-retninger (N, Ø, S, V osv.).
# 4) Udvælge enkelte tidspunkter til pile (så plottet ikke bliver overfyldt).
# 5) Lave et tidsserie-plot med:
#    - vindhastighed som linje
#    - pile, der illustrerer vindretning
#    - labels med kompas-retning
#    - annotering af den maksimale vindhastighed.


# === NØDVENDIGE PAKKER ===
library(httr)      # til at kalde API'et (HTTP GET)
library(jsonlite)  # til at parse JSON-svar til R-lister/data.frames
library(dplyr)     # til data-manipulation (select, mutate, group_by osv.)
library(tidyr)     # (ikke brugt så meget her, men god til reshaping)
library(ggplot2)   # til at lave plottet
library(scales)    # til evt. skalering/formatteringer (kan bruges i ggplot)

## === 0) Opsætning: API, stationer og periode ===

ak      <- "9253d782-9484-4918-87db-619d73b96f30"   # <- din API-nøgle til DMI (skal normalt holdes hemmelig)
baseurl <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?"

# to stationer 
# navnet i vektoren er stationens ID (som bruges i API-kaldet),
# værdien er det "pæne" navn, vi viser i plottet.
stations <- c(
  "06068" = "Aarhus Havn",
  "06080" = "Anholt"
)

# periode: stormen i oktober 2023
# (fra og med 19/10 kl. 00 til 22/10 kl. 00, angivet i UTC)
from_dt <- "2023-10-19T00:00:00Z"
to_dt   <- "2023-10-22T00:00:00Z"

# parametre (vi henter én ad gangen)
# p_speed: time-middel vindhastighed
# p_dir:   vindretning for samme tidsvindue
p_speed <- "wind_speed_past1h"   # time-middel vindhastighed
p_dir   <- "wind_dir_past1h"     # tilsvarende vindretning


## === 1) Hjælpefunktion: hent ÉN parameter for ÉN station ===
# Denne funktion laver hele API-kaldet for én station og én parameter
# og returnerer et data.frame med:
#  - tid (observed),
#  - stationId,
#  - parameterId,
#  - value (selve målingen).
fetch_one <- function(station_id, param_id) {
  # byg query-strengen til DMI-API
  # Her samler vi station, tidsperiode, parameter og api-key i én lang URL.
  q <- paste0(
    "stationId=", station_id,
    "&datetime=", from_dt, "/", to_dt,
    "&parameterId=", param_id,
    "&api-key=", ak
  )
  url <- paste0(baseurl, q)
  cat("Henter:", url, "\n")   # lille log i konsollen, så man kan se hvad der sker
  
  # kald API'et med GET
  res <- httr::GET(url)
  
  # Hvis status ikke er 200 (OK), advarer vi og returnerer NULL i stedet for at crashe
  if (res$status_code != 200) {
    warning("Kunne ikke hente ", param_id, " for station ", station_id,
            " (status ", res$status_code, ")")
    return(NULL)
  }
  
  # Læs JSON-svaret som tekst og parse det til en R-liste med fromJSON
  raw <- httr::content(res, as = "text", encoding = "UTF-8")
  js  <- jsonlite::fromJSON(raw)
  
  # Hvis der ikke er nogen features, så giver vi en advarsel og returnerer NULL
  if (length(js$features) == 0) {
    warning("Ingen features for ", param_id, " / station ", station_id)
    return(NULL)
  }
  
  # Træk properties ud som data.frame
  # Her ligger bl.a. 'value' og 'observed' (tidspunktet).
  df <- as.data.frame(js$features$properties)
  
  # Tidspunktet ligger i 'observed', vi kopierer det til en mere tydelig kolonne 'time'
  df$time <- js$features$properties$observed
  
  # Tilføj stationId og parameterId, så vi kan se hvor data kommer fra
  df$stationId <- station_id
  df$parameterId <- param_id
  
  # Returnér det færdige data.frame
  return(df)
}


## === 2) Hent alle stationer og begge parametre ===
# Idé: For hver station:
#   1) Hent vindhastighed (p_speed)
#   2) Hent vindretning (p_dir)
#   3) Slå dem sammen på tid + station
# Til sidst: bind_rows() alle stationer til ét samlet datasæt.

all_list <- list()   # tom liste til at gemme data for hver station

for (sid in names(stations)) {
  # 2a) Hent vindstyrke for station 'sid'
  spd <- fetch_one(sid, p_speed)
  
  # 2b) Hent vindretning for station 'sid'
  dir <- fetch_one(sid, p_dir)
  
  # Kun hvis vi fik begge dele (ikke NULL), slår vi dem sammen
  if (!is.null(spd) && !is.null(dir)) {
    df <- spd %>%
      # behold tid, stationId og omdøb value til value_speed
      select(time, stationId, value_speed = value) %>%
      # slå sammen med vindretning (dir) på tid + station
      left_join(
        dir %>% select(time, stationId, value_dir = value), # omdøb value -> value_dir
        by = c("time", "stationId")
      ) %>%
      # tilføj pæn stations-etiket (som vi bruger i plottet)
      mutate(station = stations[sid])
    
    # læg data for denne station ned i listen
    all_list[[sid]] <- df
  }
}

# læg alle stationers data sammen til ét samlet data.frame
obs <- bind_rows(all_list)

# hvis du vil se en smagsprøve:
head(obs)


## === 3) Konvertér datoformat + klassificér vindretning ===
# Her:
#  - laver vi tid om til POSIXct (så ggplot kan forstå det som tid)
#  - sørger for at vindretningen er numerisk
#  - laver en kompas-label (N, Ø, S osv.) ud fra vindretningen (value_dir)

obs <- obs %>%
  mutate(
    # lav tid om til rigtig POSIXct. Format matcher f.eks. "2023-10-19T01:00:00Z"
    time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    value_dir = as.numeric(value_dir)
  ) %>%
  # sorter rækker efter station og tid (pænere til både overblik og plottet)
  arrange(station, time)

# lav 8-dels kompas-labels ud fra vindretning (value_dir i grader)
# Bemærk: retningerne er her skrevet på dansk (ØNØ, ØSØ osv.)
obs <- obs %>%
  mutate(
    wind_label = case_when(
      is.na(value_dir)                               ~ NA_character_,  # hvis ingen retning
      value_dir >= 22.5  & value_dir < 67.5          ~ "ØNØ",
      value_dir >= 67.5  & value_dir < 112.5         ~ "Ø",
      value_dir >= 112.5 & value_dir < 157.5         ~ "ØSØ",
      value_dir >= 157.5 & value_dir < 202.5         ~ "S",
      value_dir >= 202.5 & value_dir < 247.5         ~ "SV",
      value_dir >= 247.5 & value_dir < 292.5         ~ "V",
      value_dir >= 292.5 & value_dir < 337.5         ~ "NV",
      TRUE                                           ~ "N"            # resten regnes som nord
    )
  )


## === 4) Lav særskilt datasæt til pilene ===
# Vi vil ikke tegne én pil for hver time (det bliver hurtigt rodet).
# I stedet tager vi kun hver 4. observation pr. station.
# Vi beregner også start- og slut-punkter for pilene (x/y),
# så ggplot kan tegne dem som geom_segment.

arrows_df <- obs %>%
  group_by(station) %>%
  # slice(seq(1, n(), by = 4)) betyder:
  # "tag hver 4. række" fra 1 til n() for hver station.
  slice(seq(1, n(), by = 4)) %>%
  ungroup() %>%
  mutate(
    len  = 0.5,                      # længden på pilen i 'm/s' på y-aksen
    rad  = value_dir * pi / 180,     # konverter grader -> radianer (til trig-funktioner)
    
    # pilens start-punkt:
    xstart = time,                   # x-start er tidspunktet
    ystart = value_speed,            # y-start er vindhastigheden
    
    # pilens endepunkt:
    # x ændres ikke (pilen står "lodret" over tidspunktet),
    # y rykkes opad afhængig af cos(retningen) * længden.
    # (Her er logikken: vi bruger cos(rad) for at give variation i "opad"-komponenten)
    xend   = time,
    yend   = value_speed + len * cos(rad)
  )


## === 5) Forbered info om maksimal vindhastighed ===
# Vi finder rækken med den største 'value_speed' og gemmer:
#  - selve værdien (max_val)
#  - tidspunktet (max_time)
# Det bruges til tekst-annoteringen i plottet.

max_row  <- obs[which.max(obs$value_speed), ]
max_val  <- max_row$value_speed
max_time <- max_row$time


## === 6) PLOT: vindhastighed + pile + labels ===

ggplot(obs, aes(x = time, y = value_speed, colour = station)) +
  # 6a) selve tidsserien pr. station (vindhastighed som linje)
  geom_line(linewidth = 1) +
  
  # 6b) pile (vindretning). Vi bruger arrows_df som datasæt her.
  geom_segment(
    data = arrows_df,
    aes(x = xstart, y = ystart, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.12, "cm")),
    colour = "black",
    linewidth = 0.4,
    inherit.aes = FALSE   # vi vil ikke arve farve efter 'station' her
  ) +
  
  # 6c) tekst-labels (kompas-retning) lige over pilenes startpunkt
  geom_text(
    data = arrows_df,
    aes(x = xstart, y = ystart + 0.7, label = wind_label),
    colour = "black",
    size = 3,
    vjust = 0
  ) +
  
  # 6d) tekst om den højeste vindhastighed i perioden
  annotate(
    "text",
    x = max_time,
    y = max_val + 1,  # lidt over den højeste værdi, så teksten ikke overlapper linjen
    label = paste0("Højest målte vindstød: ", round(max_val, 1), " m/s"),
    colour = "black",
    hjust = 0.5
  ) +
  
  # 6e) aksetitler og plot-titel
  labs(
    title = "Vind under stormen i oktober 2023",
    x = "Tid (UTC)",
    y = "Vindhastighed (m/s)",
    colour = "station"
  ) +
  
  # 6f) tema (minimalistisk udtryk)
  theme_minimal(base_size = 13)
