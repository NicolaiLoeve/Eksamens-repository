library(serial)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

## 1. Opret seriel forbindelse til Arduino ---- Opg. 1.2
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
df_kantine <- data.frame(
  person_id = numeric(0),
  distance  = numeric(0),
  timestamp = as.POSIXct(character(0))
)

## 3. While-loop der læser fra Arduino ----
stoptime <- Sys.time() + 1200   # kør i 60 sekunder (ret hvis du vil)

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
    
    df_kantine <- rbind(
      df_kantine,
      data.frame(
        person_id = person_id,
        distance  = dist,
        timestamp = Sys.time()
      )
    )
    
    print(df_kantine[nrow(df_kantine), ])        # vis seneste række i konsollen
  }
  
  Sys.sleep(0.1)   # lille pause, så vi ikke hammer CPU'en
}

df_kantine <- df_lego
saveRDS(df_kantine, "df_kantine.rds")

## 4. Nu transformerer vi, så vi kan lave plot ----
# 1) Arbejdskopi med afrundet minut
df_kantobs <- df_kantine %>%
  mutate(
    timestamp = ymd_hms(timestamp),
    interval  = floor_date(timestamp, "1 minute")
  )

# 2) Antal personer pr. minut
df_counts <- df_kantobs %>%
  count(interval, name = "count")

# 3) Lav en komplet liste af alle minutter i perioden
all_intervals <- tibble(
  interval = seq(
    from = floor_date(min(df_kantobs$timestamp), "1 minute"),
    to   = floor_date(max(df_kantobs$timestamp), "1 minute"),
    by   = "1 min"
  )
)

# 4) Join og sæt manglende til 0
df_counts_full <- all_intervals %>%
  left_join(df_counts, by = "interval") %>%
  mutate(count = replace_na(count, 0L))

# 5) Heatmap langs tidsaksen
ggplot(df_counts_full, aes(x = interval, y = 1, fill = count)) +
  geom_tile() +
  
  # --- ADD TEXT LABELS ---
  geom_text(aes(label = count),
            vjust = -0.5,        # flyt teksten op
            size = 3,            # tekststørrelse
            color = "black") +   # evt. "white" hvis du har mørke farver
  # ------------------------

scale_y_continuous(NULL, breaks = NULL) +
  scale_fill_gradient(low = "white", high = "darkred") +
  scale_x_datetime(
    breaks = seq(min(df_counts_full$interval),
                 max(df_counts_full$interval),
                 by = "1 min"),
    date_labels = "%H:%M"
  ) +
  labs(
    title = "Aktiviteten på trappen topper markant omkring kl. 12:15",
    x = "Tid",
    fill = "Antal personer pr. minut"
  ) +
  theme_minimal() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1)
  )

threshold <- mean(df_counts_full$count)

df_counts_full <- df_counts_full %>%
  mutate(traveligt = count > threshold)

ggplot(df_counts_full, aes(x = interval, y = count, fill = traveligt)) +
  geom_col() +
  
  # --- Rød gennemsnitslinje --- #
  geom_hline(yintercept = threshold, 
             color = "red", linetype = "dashed", size = 1) +
  
  scale_fill_manual(values = c("grey80", "tomato"),
                    labels = c("Under/lig gennemsnit", "Over gennemsnit"),
                    name   = "Aktivitetsniveau") +
  
  labs(
    title = "Minutter med ekstra høj aktivitet på trappen",
    subtitle = paste("69 personer i alt - gennemsnit pr. minut:", round(threshold, 1)),
    x = "Tid",
    y = "Antal personer"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )
