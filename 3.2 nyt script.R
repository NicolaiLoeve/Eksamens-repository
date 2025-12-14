#opgave 3
library(dplyr)
library(ordinal)  

# ---------------------------------------------------------
# 3.2.1 Vælg regnskabsår (2016–2020)
# ---------------------------------------------------------
regn_aar <- "2020"

# Find kolonner til regnskabsåret 2020
balance_col <- grep(paste0("Balance.*", regn_aar), names(virk), value = TRUE)
afkast_col  <- grep(paste0("Afkastningsgrad.*", regn_aar), names(virk), value = TRUE)
solidit_col <- grep(paste0("Soliditetsgrad.*", regn_aar), names(virk), value = TRUE)

# Antal ansatte (typisk uden årstal)
ansatte_col <- grep("^Antal ansatte", names(virk), value = TRUE)[1]

# ---------------------------------------------------------
# 3.2.2 Modeldataset for 2020
# ---------------------------------------------------------
modeldata <- virk %>%
  transmute(
    laane_kategori,
    balance_2020 = as.numeric(gsub(",", ".", .data[[balance_col]])),
    afkast       = as.numeric(gsub(",", ".", .data[[afkast_col]])),
    soliditet    = as.numeric(gsub(",", ".", .data[[solidit_col]])),
    ansatte      = if (!is.na(ansatte_col)) {
      as.numeric(gsub(",", ".", .data[[ansatte_col]]))
    } else NA_real_
  ) %>%
  filter(
    !is.na(laane_kategori),
    !is.na(balance_2020),
    !is.na(afkast),
    !is.na(soliditet)
  )

# Sæt korrekt orden på kategorien
modeldata$laane_kategori <- factor(
  modeldata$laane_kategori,
  levels  = c("Negativ", "Neutral", "Positiv"),
  ordered = TRUE
)

# ---------------------------------------------------------
# 3.2.3 Log-transformering (ingen gennemsnit, ingen skalering)
# ---------------------------------------------------------
modeldata <- modeldata %>%
  mutate(
    log_balance = log(balance_2020)   # log af hver virksomheds balance
  )

# ---------------------------------------------------------
# 3.2.4 CLM-model (ordered logit)
# ---------------------------------------------------------
#Sammen
mod_clm <- clm(
  laane_kategori ~ log_balance + afkast + soliditet + ansatte,
  data = modeldata,
  link = "logit"
)

summary(mod_clm)
#Hver for sig
#log balance
mod_clm1 <- clm(
  laane_kategori ~ log_balance,
  data = modeldata,
  link = "logit"
)

summary(mod_clm1)
#afkast
mod_clm2 <- clm(
  laane_kategori ~ afkast,
  data = modeldata,
  link = "logit"
)

summary(mod_clm2)
#soliditets
mod_clm3 <- clm(
  laane_kategori ~ soliditet,
  data = modeldata,
  link = "logit"
)

summary(mod_clm3)

names(modeldata)

mod_clm4 <- clm(
  laane_kategori ~ ansatte,
  data = modeldata,
  link = "logit"
)

summary(mod_clm4)
