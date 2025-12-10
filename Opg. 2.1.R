library(dplyr)
library(ggplot2)
library(tibble)

# ---------------------------------------------------------------
# HJÆLPEFUNKTION: KONVERTER KVARTALSKODE → R-DATO
# ---------------------------------------------------------------

kvartal_to_date <- function(kvartal_str) {
  year  <- as.integer(substr(kvartal_str, 1, 4))
  qtr   <- as.integer(substr(kvartal_str, 6, 6))
  month <- (qtr - 1) * 3 + 1   # 1 = jan, 4 = apr, 7 = jul, 10 = okt
  as.Date(sprintf("%04d-%02d-01", year, month))
}


# ---------------------------------------------------------------
# 1) FÆLLES BASISDATA TIL PLOTS (DI + REALVÆKST)
# ---------------------------------------------------------------

di_base <- ftidiq_wide %>%
  select(KVARTAL, DI_FTI = Forbrugertillidsindikator) %>%
  left_join(
    ftidstq_wide %>% select(KVARTAL, realvækst),
    by = "KVARTAL"
  ) %>%
  mutate(
    KVARTAL_DATE = kvartal_to_date(KVARTAL)   # <-- eneste nødvendige konvertering
  )


# ---------------------------------------------------------------
# 2) PERIODER
# ---------------------------------------------------------------

di_plot_2000_2016 <- di_base %>%
  filter(KVARTAL_DATE >= as.Date("2000-01-01"),
         KVARTAL_DATE <= as.Date("2016-12-31"))

di_plot_2000_2025 <- di_base %>%
  filter(KVARTAL_DATE >= as.Date("2000-01-01"),
         KVARTAL_DATE <= as.Date("2025-11-01"))


# ---------------------------------------------------------------
# 3) FUNKTION TIL PLOT AF DI + REALVÆKST
# ---------------------------------------------------------------

plot_di_real <- function(data, subtitle) {
  scaleFactor <- max(abs(data$DI_FTI), na.rm = TRUE) /
    max(abs(data$realvækst), na.rm = TRUE)
  
  breaks_years <- seq(
    from = as.Date(paste0(format(min(data$KVARTAL_DATE), "%Y"), "-01-01")),
    to   = as.Date(paste0(format(max(data$KVARTAL_DATE), "%Y"), "-01-01")),
    by   = "1 year"
  )
  
  ggplot(data, aes(x = KVARTAL_DATE)) +
    geom_col(aes(y = realvækst * scaleFactor),
             fill = "#00A2E8",
             colour = "#00A2E8") +
    geom_line(aes(y = DI_FTI),
              linewidth = 1.2,
              colour = "black") +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    scale_y_continuous(
      name = "DI's forbrugertillidsindikator (nettotal)",
      sec.axis = sec_axis(~ . / scaleFactor,
                          name = "Årlig realvækst pr. kvartal i privatforbruget (pct.)")
    ) +
    scale_x_date(
      breaks = breaks_years,
      labels = format(breaks_years, "%Y"),
      expand = c(0, 0)
    ) +
    coord_cartesian(xlim = range(data$KVARTAL_DATE)) +
    labs(
      title = "DI's forbrugertillidsindikator og realvækst i privatforbruget",
      subtitle = subtitle,
      x = NULL,
      caption = "Kilde: Danmarks Statistik og DI"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x        = element_text(angle = 45, hjust = 1),
      axis.title.y.right = element_text(margin = margin(l = 10)),
      axis.title.y.left  = element_text(margin = margin(r = 10)),
      plot.caption       = element_text(size = 10, hjust = 0),
      plot.title         = element_text(face = "bold", size = 15),
      plot.subtitle      = element_text(size = 12),
      panel.grid.minor   = element_blank()
    )
}


# ---------------------------------------------------------------
# 4) GENERÉR PLOTS
# ---------------------------------------------------------------

plot_di_real(di_plot_2000_2016, "2000–2016")
plot_di_real(di_plot_2000_2025, "2000–2025")


# ---------------------------------------------------------------
# 5) BASISDATA TIL TEST (DI/DST + FORBRUG)
# ---------------------------------------------------------------

df_all_base <- ftidstq_wide %>%
  select(KVARTAL,
         DST_FTI   = Forbrugertillidsindikator,
         Forbrug,
         realvækst) %>%
  left_join(
    ftidiq_wide %>% select(KVARTAL, DI_FTI = Forbrugertillidsindikator),
    by = "KVARTAL"
  ) %>%
  mutate(
    KVARTAL_DATE = kvartal_to_date(KVARTAL)   # <-- igen eneste ændring
  )


df_all16 <- df_all_base %>%
  filter(KVARTAL_DATE >= as.Date("2000-01-01"),
         KVARTAL_DATE <= as.Date("2016-12-31"))

df_all25 <- df_all_base %>%
  filter(KVARTAL_DATE >= as.Date("2000-01-01"),
         KVARTAL_DATE <= as.Date("2025-11-01"))


# ---------------------------------------------------------------
# 6) FUNKTION TIL NØGLETAL
# ---------------------------------------------------------------

beregn_nøgletal <- function(df) {
  ## Korrelationer
  cor_DI_forbrug   <- cor(df$DI_FTI,  df$Forbrug,   use = "complete.obs")
  cor_DST_forbrug  <- cor(df$DST_FTI, df$Forbrug,   use = "complete.obs")
  cor_DI_real      <- cor(df$DI_FTI,  df$realvækst, use = "complete.obs")
  cor_DST_real     <- cor(df$DST_FTI, df$realvækst, use = "complete.obs")
  
  ## Regressioner
  lm_DI_forbrug   <- summary(lm(Forbrug   ~ DI_FTI,  data = df))
  lm_DST_forbrug  <- summary(lm(Forbrug   ~ DST_FTI, data = df))
  lm_DI_real      <- summary(lm(realvækst ~ DI_FTI,  data = df))
  lm_DST_real     <- summary(lm(realvækst ~ DST_FTI, data = df))
  
  tibble(
    Indikator    = rep(c("DI-FTI", "DST-FTI"), times = 2),
    Målsvariabel = c(rep("Forbrug", 2), rep("Realvækst", 2)),
    Korrelation  = c(
      cor_DI_forbrug,  cor_DST_forbrug,
      cor_DI_real,     cor_DST_real
    ),
    R2_regression = c(
      lm_DI_forbrug$r.squared,
      lm_DST_forbrug$r.squared,
      lm_DI_real$r.squared,
      lm_DST_real$r.squared
    ),
    Koefficient = c(
      lm_DI_forbrug$coef[2, 1],
      lm_DST_forbrug$coef[2, 1],
      lm_DI_real$coef[2, 1],
      lm_DST_real$coef[2, 1]
    ),
    P_værdi = c(
      lm_DI_forbrug$coef[2, 4],
      lm_DST_forbrug$coef[2, 4],
      lm_DI_real$coef[2, 4],
      lm_DST_real$coef[2, 4]
    )
  )
}


# ---------------------------------------------------------------
# 7) SAMMENLIGNINGSTABEL
# ---------------------------------------------------------------

nøgletal16 <- beregn_nøgletal(df_all16)
nøgletal25 <- beregn_nøgletal(df_all25)

sammenligning <- bind_rows(
  nøgletal16 %>% mutate(Periode = "2000–2016"),
  nøgletal25 %>% mutate(Periode = "2000–2025")
) %>%
  select(Periode, Målsvariabel, everything())

sammenligning
