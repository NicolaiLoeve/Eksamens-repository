############################################################
# OPGAVE 1.1 – Find tabel med byer og indbyggertal
############################################################

install.packages(
  "dkstat",
  repos = c(
    "https://ropengov.r-universe.dev",
    getOption("repos")
  )
)


library(dkstat)
library(dplyr)
library(stringr)
library(readr)

# 1. Hent alle tabeller fra Statistikbanken
alle_tabeller <- dst_get_tables(lang = "da")

# 2. Filtrér dem så vi finder BY-tabel med folketal/befolkning
by_tabeller <- alle_tabeller %>%
  filter(
    str_detect(id, "^BY"),
    str_detect(text, regex("folketal|befolkning", ignore_case = TRUE))
  )

# Se resultaterne (BY1, BY2, BY3)
head(by_tabeller)

# 3. Vi vælger BY3 (Folketal pr. by)
meta_by3 <- dst_meta("BY3", lang = "da")

# 4. Find seneste år i tabellen
seneste_aar <- tail(meta_by3$values$Tid$id, 1)

# 5. Hent data: alle byer + folketal for seneste år
df_by <- dst_get_data(
  table = "BY3",
  query = list(
    BYER       = "*",
    FOLKARTAET = "Folketal",
    Tid        = seneste_aar
  ),
  lang = "da"
)

# 6. Rens bynavne
støj_mønster <- regex(
  "\\s*\\(del af [^)]+\\)|\\buden fast bopæl\\b|\\blanddistrikter\\b",
  ignore_case = TRUE
)

df_by_ren <- df_by %>%
  transmute(
    BYNAVN = BYER %>%
      str_replace("^[^\\p{L}]+", "") %>%
      str_remove_all(støj_mønster) %>%
      str_replace_all("[-–]+", " ") %>%
      str_squish(),
    INDbyggere = value
  ) %>%
  filter(
    !is.na(BYNAVN),
    BYNAVN != "",
    INDbyggere > 0
  )

head(df_by_ren)


############################################################
# OPGAVE 1.2 – Lav bykategori (bycat)
############################################################

df_by_cat <- df_by_ren %>%
  mutate(
    bycat = case_when(
      INDbyggere < 250      ~ "landsby",
      INDbyggere < 1000     ~ "lille by",
      INDbyggere < 2500     ~ "almindelig by",
      INDbyggere < 10000    ~ "større by",
      INDbyggere < 50000    ~ "storby",
      TRUE                  ~ "meget stor by"
    )
  )

head(df_by_cat)

#tabellen viser antal byer pr.kategori

(table(df_by_cat$bycat))

############################################################
# OPGAVE 1.3 – Merge boligdata med bykategori
############################################################

# 1. Indlæs boligfilen
newhomes_3_ <- read_csv("newhomes (3).csv")

bolig <- read_csv("newhomes (3).csv")

# Fjern tomme rækker
bolig <- bolig %>% filter(!is.na(zip))

# 2. Funktion til at gøre stednavne ens
rens_by <- function(x) {
  x %>%
    str_to_lower(locale = "da") %>%
    str_replace("^\\d{3,4}\\s*", "") %>% 
    str_replace_all("[^\\p{L}]+", " ") %>%
    str_squish()
}

# 3. Rens bynavne i boligdata
bolig_rens <- bolig %>%
  mutate(by_clean = rens_by(zip))

# 4. Rens bynavne i by-kategoridatasæt
by_data_rens <- df_by_cat %>%
  mutate(by_clean = rens_by(BYNAVN)) %>%
  arrange(desc(INDbyggere)) %>%          # større by først, hvis der er dubletter
  distinct(by_clean, .keep_all = TRUE) %>%  # én række per by_clean
  select(by_clean, bycat, INDbyggere)


# 5. Merge de to datasæt
bolig_merged <- bolig_rens %>%
  left_join(by_data_rens, by = "by_clean")

# 6. Ordne kategorierne og filtrere NA fra
levels_by <- c("landsby", "lille by", "almindelig by",
               "større by", "storby", "meget stor by")

bolig_merged <- bolig_merged %>%
  mutate(bycat = factor(bycat, levels = levels_by, ordered = TRUE)) %>%
  filter(!is.na(bycat))

bolig_merged <- bolig_merged %>%
  mutate(kvm_num_try = suppressWarnings(parse_number(kvm))) %>%
  filter(!is.na(kvm_num_try)) %>%
  select(-kvm_num_try)


# 7. Rens pris og kvm og lav m²-pris
bolig_merged <- bolig_merged %>%
  mutate(
    kvm_num   = parse_number(kvm),
    pris_num  = parse_number(pris, locale = locale(decimal_mark = ",",
                                                   grouping_mark = ".")),
    pris_pr_m2 = if_else(!is.na(kvm_num) & kvm_num > 0,
                         pris_num / kvm_num,
                         NA_real_)
  )

# Skriver den præcise prisi kroner i kolonnen "pris_pr._m2"

bolig_merged <- bolig_merged %>%
  mutate(
    pris_pr_m2_label = paste0(
      format(round(pris_pr_m2), big.mark = ".", decimal.mark = ","),
      " kr./m²"
    )
  )



# 8. Se det endelige resultat
head(bolig_merged %>% 
       select(addr, zip, bycat, INDbyggere, pris_num, kvm_num, pris_pr_m2))

# Tabel der viser antal boliger pr. kategori 
table(bolig_merged$bycat)

############################################################
# Opgave 1.4 – Dataframe + plot af gennemsnitlig m²-pris
############################################################

library(ggplot2)
library(tibble)
library(dplyr)

# 1) Sørg for, at kategorierne står i den ønskede rækkefølge
levels_by <- c("landsby", "lille by", "almindelig by",
               "større by", "storby", "meget stor by")

bolig_merged <- bolig_merged %>%
  mutate(bycat = factor(bycat, levels = levels_by, ordered = TRUE))

# 2) Lav sammendrag pr. kategori (gennemsnitlig m²-pris)
#    Vi laver først et "skelet" med alle kategorier, så de
#    også vises, selv hvis én kategori ingen boliger har.
df_mean <- tibble(
  bycat = factor(levels_by, levels_by, ordered = TRUE)
) %>%
  left_join(
    bolig_merged %>%
      filter(!is.na(pris_pr_m2)) %>%
      group_by(bycat) %>%
      summarise(
        gennemsnit_m2 = mean(pris_pr_m2),
        n = n(),
        .groups = "drop"
      ),
    by = "bycat"
  ) %>%
  mutate(
    # hvis en kategori ingen boliger har:
    n = if_else(is.na(n), 0L, n)
    # evt: gennemsnit_m2 = if_else(is.na(gennemsnit_m2), 0, gennemsnit_m2)
  )

# Jeg vil gøre dataframen pænere så gennemsnitsprisen pr.m2 står pænere
df_mean_pretty <- df_mean %>%
  mutate(
    gennemsnit_m2_pretty = paste0(
      format(
        round(gennemsnit_m2, 0),
        big.mark = ".", 
        decimal.mark = ","
      ),
      " kr./m²"
    )
  ) %>%
  select(bycat, gennemsnit_m2_pretty, n)


# 3) Farvepalette pr. bytype (tilsvarende jeres gamle plot)
pal <- c(
  "almindelig by" = "#E76F51",
  "landsby"       = "#4DAF4A",
  "lille by"      = "#56B4E9",
  "større by"     = "#E78AC3",
  "storby"        = "#619CFF",
  "meget stor by" = "#B79D2E"
)

# 4) Plot: gennemsnitlig m²-pris (i 1.000 kr.) pr. kategori
ggplot(df_mean, aes(x = bycat, y = gennemsnit_m2 / 1000, fill = bycat)) +
  geom_col() +
  geom_text(
    aes(label = ifelse(is.na(gennemsnit_m2), "",
                       round(gennemsnit_m2 / 1000, 1))),
    vjust = -0.2
  ) +
  scale_x_discrete(drop = FALSE, limits = levels_by) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, 5)) +
  scale_fill_manual(
    values = pal,
    breaks = levels_by,
    limits = levels_by,
    drop   = FALSE,
    name   = "Bytype"
  ) +
  labs(
    x = "Bytype",
    y = "Gns. pris pr. m² (t.kr.)",
    title   = "Kvadratmeterpriser fordelt på bytype",
    caption = "Kilde: Boligdata + Danmarks Statistik, BY3"
  ) +
  theme_minimal()

# Landsby har 70 boliger

table(bolig_merged$bycat)

# Lav en dataframe som Wulft har gjort 

bolig_simple <- bolig_merged %>%
  select(
    pris,       # pris i kroner (renset)
    pris_pr_m2_label,     # m2-pris i kroner
    bycat,          # bykategori
    by_clean        # bynavn
  )



