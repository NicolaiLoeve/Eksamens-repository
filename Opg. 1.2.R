# FIND DEN INDIKATOR/KOMBINATION DER GIVER HØJEST R²
##########################################################

# Brug alle kombinationer, men fjern de første kvartaler,
# hvor realvækst er NA pga. lag(Forbrug, 4)
fti_regdata <- fti_kombi_alle %>%
  filter(!is.na(realvækst))

# Funktion der beregner R² for en given kombinations-datasæt
beregn_r2 <- function(df) {
  model <- lm(realvækst ~ indeks, data = df)
  summary(model)$r.squared
}

# Beregn R² for alle kombinationer (én regression pr. kombination)
r2_resultater <- fti_regdata %>%
  group_by(komb_id, n_spg, spg_kombi) %>%
  summarise(
    R2 = beregn_r2(cur_data()),
    .groups = "drop"
  ) %>%
  arrange(desc(R2))

# Top-5 bedste modeller (valgfrit at kigge på)
head(r2_resultater, 5)

# Gem den allerbedste indikator/kombination
bedste <- r2_resultater[1, ]
bedste


##########################################################
# 10. HENT DI-R² DIREKTE FRA EKSISTERENDE TABEL 'sammenligning'
##########################################################
# Vi genbruger den R², du allerede har beregnet tidligere.
# NB: Tjek hvad Periode-strengen præcist hedder i din tabel:
#     fx "2000–2025" eller "2000-2025" og tilpas nedenfor.

di_r2 <- sammenligning %>%
  dplyr::filter(
    Periode == "2000–2025",        # evt. ændr til "2000-2025"
    Målsvariabel == "Realvækst",
    Indikator == "DI-FTI"
  ) %>%
  dplyr::pull(R2_regression)

di_r2


##########################################################
# 11. SAMMENLIGNINGSTABEL (BEDSTE KOMBI vs. DI-INDIKATOR)
##########################################################

sammenligning2 <- tibble::tibble(
  Model = c("Bedste indikator/kombination", "DI's indikator"),
  Indikator = c(bedste$spg_kombi, "DI-FTI"),
  `Antal spørgsmål` = c(bedste$n_spg, 5),
  R2 = c(bedste$R2, di_r2)
)

sammenligning2
