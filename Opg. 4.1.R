library(dplyr)
library(ggplot2)

##########################################################
# 1. KLARGØR DATA TIL PLOTS OG TABELLER
##########################################################
# Vi laver:
# - år, kvartal og dato for hvert kvartal
# - filtrerer til 1996 og frem
# - sorterer i tidsrækkefølge
##########################################################

ftidstq_plot <- ftidstq_wide %>%
  mutate(
    år      = as.integer(substr(KVARTAL, 1, 4)),
    kvartal = as.integer(substr(KVARTAL, 6, 6)),
    
    # Første måned i kvartalet: 1 (K1), 4 (K2), 7 (K3), 10 (K4)
    måned   = (kvartal - 1) * 3 + 1,
    
    # Vi bruger første dag i kvartalet som dato
    kvartal_dato = as.Date(sprintf("%d-%02d-01", år, måned))
  ) %>%
  # Begræns til perioden fra 1996 og frem og kun kvartaler før eller lig i dag
  filter(
    år >= 1996,
    kvartal_dato <= Sys.Date()
  ) %>%
  arrange(kvartal_dato)


##########################################################
# 2. GRAF: DST'S FORBRUGERTILLIDSINDIKATOR (KVARTALSVIS)
##########################################################

ggplot(ftidstq_plot, aes(x = kvartal_dato, y = Forbrugertillidsindikator)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title    = "DST's forbrugertillidsindikator (kvartalsvis)",
    subtitle = "Beregnet på baggrund af månedlige data, 1996 og frem",
    x        = "År",
    y        = "Forbrugertillidsindikator"
  ) +
  theme_minimal()


##########################################################
# 3. GRAF: ÅRLIG REALVÆKST I PRIVATFORBRUGET
##########################################################

ggplot(ftidstq_plot, aes(x = kvartal_dato, y = realvækst)) +
  geom_line() +
  labs(
    title = "Årlig realvækst i privatforbruget (kvartalsvis)",
    x     = "År",
    y     = "Realvækst, pct."
  ) +
  theme_minimal()


##########################################################
# 4. TOP 5 OG BUND 5 KVARTALER EFTER OPTIMISME
##########################################################
# Vi finder de mest og mindst optimistiske kvartaler
# baseret på forbrugertillidsindikatoren.
##########################################################

top_optimisme <- ftidstq_plot %>%
  arrange(desc(Forbrugertillidsindikator)) %>%
  slice_head(n = 5) %>%
  select(KVARTAL, Forbrugertillidsindikator)

bund_optimisme <- ftidstq_plot %>%
  arrange(Forbrugertillidsindikator) %>%
  slice_head(n = 5) %>%
  select(KVARTAL, Forbrugertillidsindikator)

top_optimisme    # 5 mest optimistiske kvartaler
bund_optimisme   # 5 mindst optimistiske kvartaler


##########################################################
# 5. ABSOLUT TOP- OG BUNDKVARTAL
##########################################################

max_kvartal <- ftidstq_plot %>%
  slice_max(Forbrugertillidsindikator, n = 1, with_ties = FALSE)

min_kvartal <- ftidstq_plot %>%
  slice_min(Forbrugertillidsindikator, n = 1, with_ties = FALSE)


##########################################################
# 6. GRAF MED MARKERET TOP- OG BUNDKVARTAL
##########################################################

ggplot(ftidstq_plot, aes(x = kvartal_dato, y = Forbrugertillidsindikator)) +
  geom_line() +
  
  # Marker top og bund med punkter
  geom_point(data = max_kvartal,
             aes(x = kvartal_dato, y = Forbrugertillidsindikator)) +
  geom_point(data = min_kvartal,
             aes(x = kvartal_dato, y = Forbrugertillidsindikator)) +
  
  # Skriv kvartalskoder ved top og bund
  geom_text(
    data = max_kvartal,
    aes(label = KVARTAL),
    vjust = -1,
    size  = 3
  ) +
  geom_text(
    data = min_kvartal,
    aes(label = KVARTAL),
    vjust = 1.5,
    size  = 3
  ) +
  
  # Nul-linje som reference
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  # X-akse: årstal med passende afstand
  scale_x_date(
    date_breaks = "5 years",              # fx hvert 5. år
    date_labels = "%Y",                   # vis kun årstal
    limits      = c(as.Date("1996-01-01"), NA)
  ) +
  
  # Y-akse: fast interval (kan justeres)
  scale_y_continuous(
    limits = c(-35, 15)
  ) +
  
  labs(
    title = "DST's forbrugertillidsindikator med markeret top og bund",
    x     = "År",
    y     = "Forbrugertillidsindikator"
  ) +
  theme_minimal()