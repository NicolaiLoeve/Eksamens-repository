#opgave 5.1
#Først opretter vi kolonnen "klasse" 
# Kolonnen 'Klasse': 9 A'er, 9 B'er, 9 C'er, 9 D'er
klasse <- rep(c("A", "B", "C", "D"), each = 9)

#Herefter opretter vi uge
# Kolonnen 'Uge': 1 til 9 gentaget for hver klasse
uge <- rep(1:9, times = 4)

# Til sidst opretter vi en tom eller tilfældig 'Score'-kolonne
# Her bruger vi tilfældige tal mellem 0 og 100 som eksempel

set.seed(123) #for reproducerbarhed #ikke nødvendigt med set.seed her
score <- sample(0:100, 36, replace = TRUE)

#Nu samler vi det i et dataframe 
df <- data.frame(Klasse = klasse, Uge = uge, Score = score)

#Vis dataframe 

# seq(from)laver en jævn sekvens af 36 tal fra 10 til 100.
score <- seq(from = 10, to = 100, length.out = 36)

df <- data.frame(Klasse = klasse, Uge = uge, Score = score)


# Vis resultatet
print(df)


#Opgave 5.2
# Antag at df fra opgave 5.1 allerede eksisterer
# df <- data.frame(Klasse = ..., Uge = ..., Score = ...)


#wulff siger vi skal lave 12x3 istedet for 9 som der står beskrevet.
df.quart=as.data.frame(matrix(data = NA,nrow =12, ncol = 3))     # Hvis det skulle være det opridelige 9x3 så ændre man 12 til 9 
colnames(df.quart)=colnames(df)                                  # I my hoop ændre man 36 til 27                               

myhoopseq=seq(3,36,by=3)   #sekvensen vi bruger (betyder at den tager fra række 3 til 36 og så tager den hver tredje (12x3=36 vi vil kun have 9 rækker))

#loopet 
for (j in 1:length(myhoopseq)) {
  i <- myhoopseq[j]   # slutrække (j 1 vi udfylder første række i df.quart) (i vi henter fra række 3 i df)
  
  
  df.quart$Klasse[j] <- df$Klasse[i]
  df.quart$Uge[j]    <- df$Uge[i]
  df.quart$Score[j]  <- mean(df$Score[(i-2):i])  
}   

#opgave5.3
#I skal nu konvertere denne nye dataframe til en ny dataframe som har følgende navne på kolonnerne:
#”Uge”,”A”,”B”,”C”,D” og rækkerne indeholder de gennemsnit som I har beregnet. (Hint: Brug funktionen
#pivot-wider fra pakken tidyr)

install.packages("tidyr")
library(tidyr)
library(dplyr)

df.quart2 <- df.quart %>%
  pivot_wider(
    names_from = Klasse,
    values_from = Score
  )
