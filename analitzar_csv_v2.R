
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


#   Ara amb la TAULA de LONG REPETITS
#   He de CREAR una TAULA NOVA mab els DOS NOMS la UNA LONGITUD REPETIDA

#   EXEMPLE  = Com AFEGIR UNA FILA a un DATA FRAME

#         rius_EX <- rbind(
#          rius_EX,
#          data.frame(id = 2, nom = "nom_nou_2", longitud = 222)
#        )
#         

longitud_rep <- length(rius_repetits[,1])

rius_repetits_units <- data.frame()
for ( i in 1:longitud_rep){
  nom <- rius_repetits$nom[i]
  nom_2 <- rius_repetits$nom[i+1]
  long_1 <- rius_repetits$longitud[i]
  long_2 <- rius_repetits$longitud[i+1]
  id <- rius_repetits$id[i]
  
  if (long_1==long_2){
    
    nom_nou <- paste(nom,' - ',nom_2)
    long_nou <- long_1
    id_nou <- id
    
    print(paste(id_nou," - ",nom_nou," - ",long_nou))
    
    rius_repetits_units <- rbind(
      rius_repetits_units,
      data.frame(id = id, nom = nom_nou, longitud = long_nou)
    )
    
  }
}

rius_repetits_units

#   Ara ja tinc la TAULA de les LONG REPETIDES ara AMB UN NOM UNIT
#   He d'ajuntar AQUESTA TAULA a la TAULA que NO TENIA REPETITS

rius_repetits_units
rius_no_repetits 

#   Abans de unir-les
#   He de comprovar que no estan repetits les LONGITUDS

#   Consulta ens diu si ESTÀ o no DINS

rius_no_repetits$longitud[1] %in% rius_repetits_units$longitud

#   Ara ho consulatarem amb un FOR
#   Comprovem que cada LONG de NO REPETIDA si es troba dins de REPETIDA

#   PROCÉS
#     Faig un FOR on comprova = ada LONG de NO REPETIDA si es troba dins de REPETIDA
#     Si NO HI HA CAP REPETIDA = VALOR = 0
#     Si NO HI HA CAP REPETIDA = VALOR = 1
#     Despres FAIG un IF on CONSULTO el VALOR i
#     En funció d'això ESCRIC UNA FRASE


long_repe <- length(rius_repetits_units$longitud)
long_no_repe <- length(rius_no_repetits$longitud)

for (i in 1:long_no_repe){
  repe <- rius_no_repetits$longitud[i] %in% rius_repetits_units$longitud
  valor <- 0
  if(repe == TRUE){
    valor <- 1
  }
}

if (valor == 0){ 
  print("TOT CORRECTE: NO hi ha LONG repetides")
} else {
  print("INCORRECTE: Hi ha alguna LONG repetida")
}


