library(serial)
library(DBI)
library(RMariaDB)
library(stringr)
library(stringi)
library(logr)

## 1. Opret seriel forbindelse til Arduino ---- OPG. 1.1
conarduino <- serialConnection(
  name     = "arduino",
  port     = "COM5",        # <-- RET hvis din port er en anden (fx "COM4")
  mode     = "9600,n,8,1",
  newline  = 1              # vi håndterer selv linjeskift med strsplit()
)
close(conarduino)
open(conarduino)
on.exit(close(conarduino))   # sørger for at porten lukkes når scriptet stopper

flush(conarduino)

## 2. Dataframe til at gemme målinger ----
df_lego <- data.frame(
  person_id = numeric(0),
  distance  = numeric(0),
  timestamp = as.POSIXct(character(0))
)

## 3. While-loop der læser fra Arduino ----
stoptime <- Sys.time() + 60   # kør i 60 sekunder (ret hvis du vil)

while (Sys.time() < stoptime) {
  
  # Læs hvad der lige nu ligger i bufferen fra Arduino
  line <- read.serialConnection(conarduino)
  cat("LINE:[", line, "]\n")   # debug: så du kan se, hvad der kommer
  
  # Nogle gange kommer der flere linjer på én gang, fx "1,80\n2,95\n3,110"
  lines_vec <- strsplit(line, "\n")[[1]]
  
  for (ln in lines_vec) {
    ln <- trimws(ln)
    if (nchar(ln) == 0) next      # spring tomme linjer over
    
    # Forventer formatet: person_id,distance
    parts <- strsplit(ln, ",")[[1]]
    if (length(parts) != 2) next  # ignorer alt der ikke matcher
    
    person_id <- suppressWarnings(as.numeric(parts[1]))
    dist      <- suppressWarnings(as.numeric(parts[2]))
    
    if (is.na(person_id) || is.na(dist)) next  # ignorer hvis noget ikke er tal
    
    df_lego <- rbind(
      df_lego,
      data.frame(
        person_id = person_id,
        distance  = dist,
        timestamp = Sys.time()
      )
    )
    
    print(df_lego[nrow(df_lego), ])        # vis seneste række i konsollen
  }
  
  Sys.sleep(0.1)   # lille pause, så vi ikke hammer CPU'en
}

saveRDS(df_lego, "df_lego.rds")

## 4. Kig på resultaterne ----
View(df)
print(df)