#####Opgave 4.2
install.packages("ggcorrplot")
library(ggcorrplot)
library(tidyverse)


##### Først starter vi med at forberede datasæt til korrelation. 
#Vi laver det på de 5 grupper, som vi samlede i opg 4.1.


# Bruger plotdata_grupperet (dette er datasættet hvor GRUPPE er defineret)
kor5_data <- plotdata_grupperet %>%
  group_by(GRUPPE, ÅR) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  filter(ÅR >= 2000, ÅR <= 2022) %>%
  pivot_wider(names_from = GRUPPE, values_from = value)

##Nu har vi ét datasæt med:
##| ÅR | Alkoholiske læskedrikke | Vin | Spiritus | Øl | Vinbaseret & lavalkohol |

### Så beregner vi korrelationsmatrix
kor5_matrix <- cor(kor5_data[,-1], use = "pairwise.complete.obs")
print(kor5_matrix)


### Nu plotter vi det, med et heat map.
ggcorrplot(kor5_matrix,
           method = "square",
           type = "lower",
           lab = TRUE,
           lab_size = 4,
           colors = c("red", "white", "blue"),
           title = "Korrelationsmatrix for de 5 samlede alkoholgrupper",
           show.legend = TRUE)
###I opgaven står der beskrevet hvorfor vi bruger "Lower", og er derfor vi ikke ser en 5x5 matrix
#### men at vi får "halvdelen"