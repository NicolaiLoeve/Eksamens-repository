## ----------------------------------------------------------

library(dplyr)
library(tibble)
library(ggplot2)
library(lubridate)

# Konverterer fx "2025K4" -> "2025-10-01" (bruges kun til x-akse i plots)
kvartal_to_date <- function(kvartal_str) {
  year  <- as.integer(substr(kvartal_str, 1, 4))
  qtr   <- as.integer(substr(kvartal_str, 6, 6))
  month <- (qtr - 1) * 3 + 1
  as.Date(sprintf("%04d-%02d-01", year, month))
}

## ----------------------------------------------------------
## 1. Vælg kvartal der skal forudsiges
## ----------------------------------------------------------

mål_kvartal <- "2025K4"

## ----------------------------------------------------------
## 2. Hent indikatorværdier for DI og DST i mål-kvartalet
## ----------------------------------------------------------

# DI's forbrugertillid i mål_kvartal
di_K4 <- ftidiq_wide %>%
  filter(KVARTAL == mål_kvartal) %>%
  pull(Forbrugertillidsindikator)

# DST's forbrugertillid i mål_kvartal
dst_K4 <- ftidstq_wide %>%
  filter(KVARTAL == mål_kvartal) %>%
  pull(Forbrugertillidsindikator)

## ----------------------------------------------------------
## 3. Opret regressionsmodeller (realvækst og forbrug)
## ----------------------------------------------------------

# DI-modeller
model_DI_real <- lm(realvækst ~ Forbrugertillidsindikator, data = ftidiq_wide)
model_DI_forb <- lm(Forbrug   ~ Forbrugertillidsindikator, data = ftidiq_wide)

# DST-modeller
model_DST_real <- lm(realvækst ~ Forbrugertillidsindikator, data = ftidstq_wide)
model_DST_forb <- lm(Forbrug   ~ Forbrugertillidsindikator, data = ftidstq_wide)

## ----------------------------------------------------------
## 4. Forudsig realvækst og forbrug i mål_kvartal
## ----------------------------------------------------------

pred_DI_real <- predict(
  model_DI_real,
  newdata = data.frame(Forbrugertillidsindikator = di_K4)
)

pred_DI_forb <- predict(
  model_DI_forb,
  newdata = data.frame(Forbrugertillidsindikator = di_K4)
)

pred_DST_real <- predict(
  model_DST_real,
  newdata = data.frame(Forbrugertillidsindikator = dst_K4)
)

pred_DST_forb <- predict(
  model_DST_forb,
  newdata = data.frame(Forbrugertillidsindikator = dst_K4)
)

## ----------------------------------------------------------
## 5. Saml forudsigelser i en overskuelig tabel
## ----------------------------------------------------------

forudsigelser <- tibble(
  Model                    = c("DI-model",        "DST-model"),
  KVARTAL                  = mål_kvartal,
  Forudset_realvækst_pct   = c(pred_DI_real,      pred_DST_real),
  Forudset_forbrug_niveau  = c(pred_DI_forb,      pred_DST_forb)
)

forudsigelser   # kig på resultatet

## ----------------------------------------------------------
## 6. Læg forudsigelsen ind i hoved-datasættene
##    (så 2025K4 ligger som en ekstra række med prognose)
## ----------------------------------------------------------

# DI: fjern evt. tidligere 2025K4-række og tilføj ny med forudsigelse
ftidiq_wide <- ftidiq_wide %>%
  filter(KVARTAL != mål_kvartal) %>%
  bind_rows(
    tibble(
      KVARTAL                   = mål_kvartal,
      Forbrugertillidsindikator = di_K4,
      realvækst                 = pred_DI_real,
      Forbrug                   = pred_DI_forb
    )
  )

# DST: samme logik
ftidstq_wide <- ftidstq_wide %>%
  filter(KVARTAL != mål_kvartal) %>%
  bind_rows(
    tibble(
      KVARTAL                   = mål_kvartal,
      Forbrugertillidsindikator = dst_K4,
      realvækst                 = pred_DST_real,
      Forbrug                   = pred_DST_forb
    )
  )

## ----------------------------------------------------------
## 7. NY plotfunktion (Løsning A):
##    Søjler = realvækst, linje = forbrugertillid
##    – viser kun de seneste 'last_n_years' år
## ----------------------------------------------------------

plot_fti_og_realvaekst <- function(df,
                                   kvartal,
                                   title,
                                   source_text,
                                   last_n_years = 10) {
  
  df2 <- df %>%
    mutate(
      KVARTAL_DATE = kvartal_to_date(KVARTAL),
      type         = if_else(KVARTAL == kvartal, "Prediction", "History")
    )
  
  # filtrér til seneste last_n_years år
  cutoff_date <- max(df2$KVARTAL_DATE, na.rm = TRUE) - years(last_n_years)
  
  df_final <- df2 %>%
    filter(KVARTAL_DATE >= cutoff_date)
  
  # skalering: få realvækst ind på samme skala som FTI
  scale_factor <- max(abs(df_final$Forbrugertillidsindikator), na.rm = TRUE) /
    max(abs(df_final$realvækst), na.rm = TRUE)
  
  ggplot(df_final, aes(x = KVARTAL_DATE)) +
    
    # Søjler = realvækst (historik blå, prognose rød)
    geom_col(aes(y = realvækst * scale_factor, fill = type)) +
    scale_fill_manual(values = c("History" = "steelblue",
                                 "Prediction" = "red")) +
    
    # Sort linje og punkt = forbrugertillid (niveau)
    geom_line(aes(y = Forbrugertillidsindikator),
              linewidth = 1.1, colour = "black") +
    geom_point(aes(y = Forbrugertillidsindikator, colour = type),
               size = 3) +
    scale_color_manual(values = c("History" = "black",
                                  "Prediction" = "red")) +
    
    # Primær: FTI. Sekundær: realvækst
    scale_y_continuous(
      name = "Forbrugertillidsindikator (nettotal)",
      sec.axis = sec_axis(~ . / scale_factor,
                          name = "Realvækst i forbrug (pct.)")
    ) +
    
    labs(
      title    = title,
      subtitle = paste("Prognose for", kvartal,
                       "– seneste", last_n_years, "år"),
      x        = "Kvartal",
      caption  = source_text
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1),
      plot.title    = element_text(face = "bold", size = 15),
      plot.caption  = element_text(size = 10, hjust = 0),
      legend.position = "none"
    )
}

## ----------------------------------------------------------
## 8. Kald plots (DI- og DST-model)
## ----------------------------------------------------------

# DI-model: Forbrugertillid + realvækst (seneste 5 år)
plot_fti_og_realvaekst(
  df          = ftidiq_wide,
  kvartal     = mål_kvartal,
  title       = "DI-model: Forbrugertillid og realvækst (historik + prognose)",
  source_text = "Kilde: DI og Danmarks Statistik",
  last_n_years = 5
)

# DST-model: Forbrugertillid + realvækst (seneste 5 år)
plot_fti_og_realvaekst(
  df          = ftidstq_wide,
  kvartal     = mål_kvartal,
  title       = "DST-model: Forbrugertillid og realvækst (historik + prognose)",
  source_text = "Kilde: Danmarks Statistik",
  last_n_years = 5
)