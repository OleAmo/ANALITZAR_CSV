
library(tidyverse)
library(sf) 

rius <- data.frame(
  id = 1:10,
  nom = c("Riu_A","Riu_B","Riu_C","Riu_D","Riu_E",
          "Riu_F","Riu_G","Riu_H","Riu_I","Riu_J"),
  longitud = c(56, 56,
               44, 44,
               120, 120,
               80, 80,
               95, 200)
)

#   Afegeixo 5 linies més

rius<- bind_rows(
  rius,
  data.frame(
    id = 11:14,
    nom = c("Riu_K", "Riu_L", "Riu_M", "Riu_N"),
    longitud = c(150, 73, 310, 18)
  )
)

rius


#   Ara començaré a filtrar
#   Vull saber LONGITUD REPETIDE

longitud_repetida <- c()

longitud <- length(rius[,1])
for (i in 1:longitud){
  val <- rius$longitud[i]
  index_val <- which(rius$longitud == val)
  num_index <- length(index_val)
  if (num_index > 1){
    longitud_repetida <- c(longitud_repetida,val)
    
  }
}

longitud_repetida

#   Ara amb les LONGITUD REPETIDES
#   Crearé DUES TAULES

#   La PRIMERA sense LES REPTETIDES
#   La SEGONA amb NOMES REPETIDES = despres la filtraré


rius_no_repetits <- rius %>%
  filter(!(rius$longitud %in% longitud_repetida)) 

#   Ara ja tinc TAULA NO REPETIDA = rius_no_repetits
#   Tinc tb un VECTOR amb LONGITUD REPETIDES
#   He de transformar el VECTOR de LONG REPETIDES amb una TAULA COMPLETA

rius_repetits <- rius %>%
  filter((rius$longitud %in% longitud_repetida))


#   Ara amb la TAULA REPETIDA he de unir DOS NOMS a UNA LONGITUD

longitud_rep <- length(rius_repetits[,1])+1 



for ( i in 1:longitud_rep){
  nom <- rius_repetits$nom[i]
  nom_2 <- rius_repetits$nom[i+1]
  long_1 <- rius_repetits$longitud[i]
  long_2 <- rius_repetits$longitud[i+1]
  if (long_1==long_1){
    print(paste(nom,' - ',nom_2))
  }
  
}

