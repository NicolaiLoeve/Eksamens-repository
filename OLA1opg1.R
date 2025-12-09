newhomes_3_ <- read_csv("newhomes (3).csv")
Bolig <- read.csv("newhomes (3).csv")
library(tidyverse)

# Fjerner kolonne "Vej" og "X" da vi vurderede, at informationen ikke var nødvendig
# Samme information bliver oplyst i andre kolonner (Der er flere kolonner som kunne fjernes f.eks. Bolig ID)
Bolig2 <- Bolig %>% select(-vej, -X)

# Rense kolonner for at gøre det lettere at fremsøge det nødvendige data / ved brug af gsub fjerner vi "/adresse/" samt "-" og " ".

Bolig2$addr <- gsub("^/adresse/","", Bolig2$addr)
Bolig2$addr <- gsub("-"," ",Bolig2$addr)

# Opg. 1.1 - find boligerne fra opgavesættet i dataframe - her bruges grep til at søge på adresserne angivet i addr kolonnen

grep("frigrunden 9", Bolig2$addr)
grep("hostrupvej 13a", Bolig2$addr)

# Frigrunden = række 1040, Hostrupvej = række 1112.

# Opg. 1.2 - Udvælg 2 ejendomme og find på boligsiden

sample1 <- Bolig2 %>%
  filter(Bolig2$liggetid > 1) %>%
  slice_sample(n = 2)

# Opg. 1.3
# Hvorfor NA-værdier? Evt. hvis bolig er solgt men side ikke opdateret
# Hvilke variable mangler? Der mangler en variabel som fortæller, om boligen er solgt eller ej
# Hvordan identificerer man en bolig til salg? Filter på Liggetiden. Se docs.

# Opg. 1.4
#Med udgangspunkt i Data Science Modellen skal I gøre rede for de skridt, der er blevet taget for at nå frem
#til csv-filen. I skal komme med et bud på et “research goal” som kunne have optimeret processen med at
#fremskaffe data fra boligsiden. Se docs

#Research goal - 
#Retrieving data - Indlæsning af csv fil 
#Data preparation - Oprydning i fil og fjernelse af "unødvendige" kolonner
#Data exploration - 
#Data modeling - 
#Data presentation - 

