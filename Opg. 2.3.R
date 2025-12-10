## ----------------------------------------------------------
## Q4-HJÆLPEFUNKTIONER
## ----------------------------------------------------------

# Funktion: filtrér kun 4. kvartal (K4) og sorter
extract_Q4 <- function(df) {
  df %>%
    filter(substr(KVARTAL, 6, 6) == "4") %>%  # kun kvartaler der ender på K4
    arrange(KVARTAL)
}

# Funktion: byg datasæt til Q4-plot med historik + evt. prognose
# - df:      datasæt (fx ftidstq_wide eller ftidiq_wide)

## Vælg kvartal der skal forudsiges


mål_kvartal <- "2025K4"

# - pred_forbrug: forudsagt forbrug for mål_kvartal (numerisk)
build_Q4_plot_data <- function(df, mål_kvartal, pred_forbrug) {
  
  # 1) Hent de seneste 6 Q4'er
  Q4 <- df %>%
    extract_Q4() %>%
    slice_tail(n = 6)
  
  # 2) Tjek om mål_kvartal allerede findes i de 6 Q4'er
  prognose_findes <- mål_kvartal %in% Q4$KVARTAL
  
  if (!prognose_findes) {
    # 3a) Hvis det ikke findes, tilføj prognosen som ny række
    Q4_plot <- Q4 %>%
      bind_rows(
        tibble(
          KVARTAL = mål_kvartal,
          Forbrug = pred_forbrug,
          type    = "Prediction"
        )
      )
  } else {
    # 3b) Hvis det findes, markér den række som "Prediction"
    Q4_plot <- Q4 %>%
      mutate(
        type = ifelse(KVARTAL == mål_kvartal, "Prediction", "History")
      )
  }
  
  # 4) Sørg for at alle øvrige rækker får type = "History"
  Q4_plot %>%
    mutate(
      type = ifelse(is.na(type), "History", type)
    )
}

## ----------------------------------------------------------
## DST: Q4-plot (forbrug, seneste 6 år + prognose)
## ----------------------------------------------------------

# Byg datasæt til DST Q4-plot
dst_Q4_plot <- build_Q4_plot_data(
  df           = ftidstq_wide,
  mål_kvartal  = mål_kvartal,
  pred_forbrug = pred_DST_forb
)

# Søjlediagram for DST
ggplot(dst_Q4_plot, aes(x = KVARTAL, y = Forbrug, fill = type)) +
  geom_col() +
  scale_fill_manual(values = c("History" = "steelblue", "Prediction" = "red")) +
  geom_text(aes(label = sprintf("%.1f", Forbrug)),
            vjust = -0.5, size = 4) +
  scale_y_continuous(limits = c(0, 375)) +   # y-akse: 0–375 mia. kr.
  labs(
    title    = "Forbrug i 4. kvartal falder",
    subtitle = paste("Seneste 6 år samt prognosen for", mål_kvartal),
    x        = "Kvartal",
    y        = "Forbrug (mia. kr.)",
    caption  = "Kilde: Danmarks Statistik og egen beregning"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(size = 15, face = "bold"),
    plot.caption = element_text(size = 10, hjust = 0)
  )

## ----------------------------------------------------------
## DI: Q4-plot (forbrug, seneste 6 år + prognose)
## ----------------------------------------------------------

# Byg datasæt til DI Q4-plot
di_Q4_plot <- build_Q4_plot_data(
  df           = ftidiq_wide,
  mål_kvartal  = mål_kvartal,
  pred_forbrug = pred_DI_forb
)

# Søjlediagram for DI
ggplot(di_Q4_plot, aes(x = KVARTAL, y = Forbrug, fill = type)) +
  geom_col() +
  scale_fill_manual(values = c("History" = "steelblue", "Prediction" = "red")) +
  geom_text(aes(label = sprintf("%.1f", Forbrug)),
            vjust = -0.5, size = 4) +
  scale_y_continuous(limits = c(0, 375)) +   # y-akse: 0–375 mia. kr.
  labs(
    title    = "Forbrug i 4. kvartal falder",
    subtitle = paste("Seneste 6 år samt prognosen for", mål_kvartal),
    x        = "Kvartal",
    y        = "Forbrug (mia. kr.)",
    caption  = "Kilde: DI og Danmarks Statistik"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(size = 15, face = "bold"),
    plot.caption = element_text(size = 10, hjust = 0)
  )
