##Det virker til at hele koden som Wulf har lagt op, hjælper med at løse alle opgaverne lidt af gangen.

#Nu prøver vi så at svare på spørgsmålene i opgaven.

#Så lad os kalde det her for opgave 3.1

# Fjerner tomme lande
statedfpr_no_na <- subset(statedfpr, !is.na(V3))

country_counts <- table(statedfpr_no_na$V3)


#I "Statedfpr" har vi ikke kolonne navne, men heldigvis har chatten fundet det til os 'woop woop'
##Så vi kører lige en lille kode for at rydde op og navngive datasættet
colnames(statedfpr) <- c(
  "icao24", "callsign", "origin_country", "time_position",
  "last_contact", "longitude", "latitude", "baro_altitude",
  "on_ground", "velocity", "true_track", "vertical_rate",
  "sensors", "geo_altitude", "squawk", "spi", "category"
)

View(statedfpr)

#### vi lagde mærke til at kolonnen med "Sensors" der var alt "NA", så den fjerner vi
statedfpr$sensors <- NULL

####Her kommer vores Plot
## 1. Frekvenser sorteret fra flest -> færrest
country_counts <- sort(table(statedfpr$origin_country), decreasing = TRUE)

## 2. Konkluderende overskrift
top_country <- names(country_counts)[1]
title_text  <- paste0("Flest fly over Nordsøen kommer fra ", top_country)

## 3. Giv mere plads top + bund
op <- par(mar = c(11, 4, 7, 2))   # bottom, left, top, right

## 4. Barplot (uden labels)
bp <- barplot(
  country_counts,
  xaxt = "n",
  col  = "#4C72B0",
  border = NA,
  ylim = c(0, max(country_counts) * 1.15),   # ekstra plads over søjlerne
  ylab = "Antal fly"
)

## 5. Overskrift (garanteret på plottet)
title(main = title_text, line = 2.5, cex.main = 1.4)

## 6. Landenavne med 45 grader
text(
  x = bp,
  y = par("usr")[3] - 0.05 * max(country_counts),
  labels = names(country_counts),
  srt = 45,
  xpd = TRUE,
  adj = 1,
  cex = 0.7
)

## 7. Præcist antal fly oven på hver søjle
text(
  x = bp,
  y = country_counts,
  labels = as.numeric(country_counts),
  pos = 3,
  cex = 1.0
)

## 8. Kilde
mtext("Kilde: OpenSky Network (states/all)", side = 1, line = 9, cex = 0.9)

## 9. Reset graf-indstillinger
par(op)


###### Det var ikke nok!!!! 
### Nu laver vi et plot med top 5 lande som har flest fly.
## 1. Frekvenser sorteret fra flest -> færrest
country_counts <- sort(table(statedfpr$origin_country), decreasing = TRUE)

## 2. Tag de 5 øverste
top5 <- country_counts[1:5]

## 3. Overskrift
title_text <- "Top 5 lande med flest fly over Nordsøen"

## 4. Giv ekstra plads i top og bund
op <- par(mar = c(12, 4, 6, 2))   
# bottom, left, top, right

## 5. Barplot (uden labels)
bp <- barplot(
  top5,
  col = "#4C72B0",
  border = NA,
  xaxt = "n",
  ylim = c(0, max(top5) * 1.25),   # plads til labels
  ylab = "Antal fly"
)

## 6. Overskrift
title(main = title_text, line = 2.5, cex.main = 1.5)

## 7. Landenavne (rykket længere ned)
text(
  x = bp,
  y = par("usr")[3] - 0.10 * max(top5),   # længere ned end før
  labels = names(top5),
  srt = 45,
  xpd = TRUE,
  adj = 1,
  cex = 1
)

## 8. Præcise antal oven på søjlerne
text(
  x = bp,
  y = top5,
  labels = as.numeric(top5),
  pos = 3,
  cex = 1.1
)

## 9. Kilde (rykket længere ned så den IKKE overlapper labels)
mtext("Kilde: OpenSky Network (states/all)", 
      side = 1, line = 10, cex = 0.9)

## 10. Reset graf-indstillinger
par(op)



##### Bum bum, det var opgave 3.1 færdig

