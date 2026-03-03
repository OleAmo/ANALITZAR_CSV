# MÍNIM PER COMENÇAR
# 
#     llegir CSV
#     fer filtres i resums
#     crear gràfics
#     llegir shapefiles
#     manipular taula d’atributs

# install.packages("tidyverse")
# install.packages("sf")


library(tidyverse)
library(sf) 

rius <- data.frame(
  ID = c(609, 610, 611),
  LONGITUD = c(1500, 2576,1467),
  NOM_RIU = c("Garona","Ter","Ebre"),
  MUNICIPI = c("Barcelona", "Girona","Tarragona")
)

#  Exemple = CREAR VECTOR 

#      i.) Vull NOMES noms de rius
#      i.) En el FOR selecciono nomes $NOM_RIU
#      i.) Creo un VECTOR BUIT

llista <- c()
for (files in rius$NOM_RIU){
  llista <- c(llista, files)
 }
print (llista)

#  Exemple = CREAR STRING

#      i.) Vull STRING de TOTS noms de rius
#      i.) En el FOR uso el VECTOR de NOM RIUS
#      i.) Creo un STRING BUIT

frase <- ""
for (nom in llista){
  frase <- paste(frase, nom)
}
print (frase)


#  Exemple = CREAR STRING BARREJA

#      i.) Vull STRING de TOTA INFO de UNA FILA
#      i.) En el FOR uso tot el DATAFRAME de RIUS
#      i.) SELECCIONO només la 1r FILA => rius[1,]
#      i.) Faig PASTE per concatenar

fila_1 <-""
for (i in rius[1,]){
  fila_1 <- paste(fila_1,i)
}

print(fila_1)


#  Exemple = CREAR VECOR AMB STRINGS BARREJA  

#      i.) Vull un VECTOR amb CADA STRING de TOTES FILES
#      i.) Necessito el NUMERO DE FILES passat a NUMERIC
#      i.) Necessito un VECTOR BUIT on ompliré amb totes les frases
#      i.) Necessito un TEXT BUIT on fare x cada fila una frase

#      i.) Fare DOS FORs
#      i.) El PRIMER es un IN RANGE on recorra les 3 FILES
#      i.) El SEGON recorrera CADA FILA i FARA UN FRASE amb lo de dins
#      i.) Un copa acabada la frase de una fila LA AFEGEIX al VECTOR COMÚ


num_files = as.numeric(length(rius))-1
total_files <- c()

for (i in 1:num_files){
  text<-""
  for (files in rius[i,]){
    text <- paste(text,files)
  }
  total_files <- c(total_files, text)
}

for (i in total_files){
  print(i)
}

#  Exemple = VULL SUMAR TOTS ELEMENT DE UNA COLUMNA 

#      i.) Creeo una TAULA nova
#      i.) Calculo el NUMERO de ELEMENTS de UNA COLUMNA = rius_2[,1]
#      i.) Faig un FOR que recorri només la COLUMNA
#      i.) Faif sumatori d'acomulacio


rius_2 <- data.frame(
  ID = c(
    609, 610, 611, 612, 613,
    614, 615, 609, 617, 618,
    619, 620, 621, 622, 610,
    624, 625, 626, 627, 628,
    629, 611, 631, 632, 633
  ),
  
  LONGITUD = c(
    1500, 2576, 1467, 890, 2100,
    1345, 980, 1520, 1750, 2400,
    1120, 1890, 760, 1325, 2600,
    1430, 990, 2010, 1700, 1550,
    1875, 1490, 1230, 1650, 1420
  ),
  
  NOM_RIU = c(
    "Garona", "Ter", "Ebre", "Llobregat", "Segre",
    "Fluvià", "Muga", "Garona", "Ter", "Ebre",
    "Llobregat", "Segre", "Fluvià", "Muga", "Ter",
    "Garona", "Ebre", "Llobregat", "Segre", "Fluvià",
    "Muga", "Ebre", "Ter", "Garona", "Segre"
  ),
  
  MUNICIPI = c(
    "Barcelona", "Girona", "Tarragona", "Manresa", "Lleida",
    "Olot", "Figueres", "Vielha", "Salt", "Amposta",
    "Martorell", "Balaguer", "Banyoles", "Roses", "Girona",
    "Bossòst", "Deltebre", "Sant Boi", "La Seu", "Besalú",
    "Castelló", "Tortosa", "Camprodon", "Arties", "Agramunt"
  )
)



num_elements <- length(rius_2[,1])
sum <- 0
for (i in 1:num_elements){
  sum <- sum +rius_2[i,1]
}
print(sum)


#  Exemple = VULL COMPARAR TOTS ELEMENT DE UNA COLUMNA 
#      i.) Busco el NÚMERO de ELEMNTS de una columna
#      i.) Faig un FOR en funció del Nº d'elements
#      i.) Creo VAL = l'emelent a inspeccionar
#      i.) Creo INDEX_VAL = En quines posicions apareix VAL
#      i.) Creo NUM_INDEX = Quants index hi ha x tant quantes vegades apareix VAL
#      i.) Faig un text explicatiu



num_elements <- length(rius_2[,1])

for (i in 1:num_elements){
  val <- rius_2[i,1]
  index_val <- which(rius_2[,1] == val)
  num_index <- length(index_val)
  text <- paste('el valor ',val,', surt ',num_index,' vegades')
  print(text)
}


#  Exemple = VULL COMPARAR I FER CANVIS EN ELEMENTS DE UNA COLUMNA 

#  OBJECTIU
#         +) Detectar els ID iguals
#         +) Nomes dels ID iguals escriurl la info = LONG, MUNICIPI,..

#      i.) Busco el NÚMERO de ELEMNTS de una columna
#      i.) Faig un FOR en funció del Nº d'elements
#      i.) Creo VAL = l'emelent a inspeccionar
#      i.) Creo INDEX_VAL = En quines posicions apareix VAL
#      i.) Creo NUM_INDEX = Quants index hi ha x tant quantes vegades apareix VAL

#      i.) Faig un IF = SI el NUM INDEX > 1 (si està repetit)
#      i.) Creo el VAL del INDEX 1 i del 2 = 
#      i.) Amb aquest INDEX puc ESCRIURE cada linies que té el mateix ID
#      i.) 


num_elements <- length(rius_2[,1])

for (i in 1:num_elements){
  val <- rius_2[i,1]
  index_val <- which(rius_2[,1] == val)
  num_index <- length(index_val)
  if (num_index>1){
    i_1 <- index_val[1]
    i_2 <- index_val[2]
    
    text_1 <- paste(rius_2[i_1,1]," ",rius_2[i_1,2]," ",rius_2[i_1,3]," ",rius_2[i_1,4])
    text_2 <- paste(rius_2[i_2,1]," ",rius_2[i_2,2]," ",rius_2[i_2,3]," ",rius_2[i_2,4])
    print(text_1)
    print(text_2)
    print('---------')
    }
}


#  Exemple = ANALIZAR CSV SHAPE v1 

#  OBJECTIU 1
#         +) Detectar els OBJECT_ID_2 IGUALS = vol dir que han fet INTERSECT amb dos rius
#         +) Un cop detectats AJUNTEM els seus NOM_RIO
#         +) També AJUNTME OBJECTI_ID_2

#  OBJECTIU 2
#         +) El OBJECT_ID_2 es el RIU SENSE NOM que intersecta amb un RIU amb nom
#         +) Amb aquesta info sabrem quin nom donar al OBJECT_ID_2
#         +) Farem un NOU DATA FRAME
#         +) Tindra el OBJECTID_2 i el seu NOU NOM



rius_3 <- data.frame(
  OBJECTID = c(
    60202, 60203, 60203, 60204, 60202, 60205,
    60194, 60207, 60194, 60208, 60194, 60209,
    60213, 60209, 60210, 60210, 60211, 60211,
    60212, 60213, 60218, 60213, 60219, 60221,
    60219, 60220, 60213, 60219, 60221
  ),
  
  nom_rio = c(
    "RIERA DE L'ESPARRA",
    "SIN NOMBRE",
    "SIN NOMBRE",
    "SIN NOMBRE",
    "RIERA DE L'ESPARRA",
    "SIN NOMBRE",
    "RIERA DE MAÇANES",
    "SIN NOMBRE",
    "RIERA DE MAÇANES",
    "SIN NOMBRE",
    "RIERA DE MAÇANES",
    "SIN NOMBRE",
    "TORRENT DE MOLLET",
    "SIN NOMBRE",
    "SIN NOMBRE",
    "SIN NOMBRE",
    "SIN NOMBRE",
    "SIN NOMBRE",
    "SIN NOMBRE",
    "TORRENT DE MOLLET",
    "SIN NOMBRE",
    "TORRENT DE MOLLET",
    "SIN NOMBRE",
    "SIN NOMBRE",
    "SIN NOMBRE",
    "SIN NOMBRE",
    "TORRENT DE MOLLET",
    "SIN NOMBRE",
    "SIN NOMBRE"
  ),
  
  OBJECTID_2 = c(
    60203, 60203, 60204, 60204, 60205, 60205,
    60207, 60207, 60208, 60208, 60209, 60209,
    60209, 60210, 60210, 60211, 60211, 60212,
    60212, 60218, 60218, 60219, 60219, 60219,
    60220, 60220, 60221, 60221, 60221
  ),
  
  nom_rio_2 = rep("SIN NOMBRE", 29)
)

#      PROCEDIMENT

#      i.) Busco el NÚMERO de ELEMNTS de una columna
#      i.) Creo VECTORS BUITS per ID i per RIU
#      i.) Les omplire amb IDs REPETITS i els seus NOUS NOMS

#      i.) Faig un FOR en funció del Nº d'elements
#      i.) Creo ID = primer ID
#      i.) Creo ID_2 = segon ID
#      i.) Creo RIU = primer NOM RIU
#      i.) Creo RIU_2 = segon NOM RIU

#      i.) Faig un IF = SI 1r ID i 2n ID son igualst)
#      i.) Omplo LLISTA de ID i de NOMS RIUS
#      i.) Poso el 1r ID i el 1r NOM = el 2n nom és sempre SIN NOMBRE (ja que el csv es aixi)
#      i.) Creo en NOU DATA FRAME amb aquestes dos vectors


num_elements <- length(rius_3[,3])-1
llista_id <- c()
llista_riu <- c()

for (i in 1:num_elements){
  id <- rius_3[i,3]
  id_2 <- rius_3[i+1,3]
  
  riu <- rius_3[i,2]
  riu_2 <- rius_3[i+1,2]
  
  if (id == id_2){
    llista_id <- c(llista_id,id)
    llista_riu <- c(llista_riu,riu)
    
    rius_v1 <- data.frame(
      OBJECTID_2 = llista_id,
      nom_rio = llista_riu
    )
  }
}

rius_v1


#  Exemple = ANALIZAR CSV SHAPE v2

#  OBJECTIU 1
#         +) Tronar a buscar OBJECTID_2 REPETITS
#         +) Crear una NOVA TAULA només amb els REPETITS

num_elements <- length(rius_v1[,1])-1
llista_id <- c()
llista_riu <- c()

for (i in 1:num_elements){
  id <- rius_v1[i,1]
  id_2<- rius_v1[i+1,1]

  riu <- rius_v1[i,2]
  riu_2 <- rius_v1[i+1,2]
  
  if (id == id_2){
    llista_id <- c(llista_id,id)
    llista_riu <- c(llista_riu,riu)
    
    rius_v2 <- data.frame(
      OBJECTID_2 = llista_id,
      nom_rio = llista_riu
    )
  }
}

rius_v2


#  Exemple = ANALIZAR CSV SHAPE v4

#  OBJECTIU 1
#         +)AJUNTAR les dos TAULES
#         +)QUAN AJUNTEM ES BARREJA info processada i la original

#  PROCÉS
#         +) AJUNTAR FILES amb  rbind()

# TAULES Tractades x ELIMINAR SENSE NOMS
rius_v1
rius_v2

rius_unio <- rbind(rius_v1,rius_v2)



#  Exemple = ANALIZAR CSV SHAPE v5

#  OBJECTIU 1
#         +) AJUNTAR les dos TAULES
#         +) QUAN AJUNTEM ES BARREJA info processada i la original
#         +) SEPARAR les dos infos per tenir una TAULA FINAL BONA
#         +) Hem de saber quins IDs de les ultimes taules es repeteixen
#         +) I borrar just aquestes linies

#  PROCÉS
#         +) AJUNTAR FILES amb  rbind()
#         +) ELIMINAR per FILTER

#  PROCÉS EXEMPLE
#         +) EXEMPLE ELIMINIAR
#         +) EXEMPLE d R

df2 <- data.frame(
  id = c(5,6,7,8),
  nom = c("Fluvià","Muga","mmm","ooo"),
  longitud = c(80, 65, 89,98)
)

df2 <- df2 %>%
  filter(longitud != c(80,65,33))




#  CREAR FUNCIÓ i ACCEDIR A VARIABLES
#         +)Aprenc a crear funció
#         +)I com accedir a les variables creades


nom_funcio <- function() {
  
  n <- 5
  d <- 23
  r <- 33
  
  return(list(
    val_1 = n,
    val_2 = d,
    val_3 = r))
}

valors <- nom_funcio()

valors$val_1



#  CREAR FUNCIÓ QUE CREA RIUS_3 i RIUS_v1 i v2
#         +)Aprenc a crear funció
#         +)I com accedir a les variables creades














#  PROCÉS
#         +)ELIMINIAR IDs REPETITS
#         +)Comparar els IDs de RIUS_V2 vs RIUS_V1
#         +)RIUS_V2 te menys que RIUS_V1
#         +)Crear vectors dels ID del RIU_V2
#         +)Eliminar del RIUS_V1 les files de ID que surten al vector



rius_v1
rius_v2

num_elements_v1 <- length(rius_v1[,1])
num_elements_v2 <- length(rius_v2[,1])

id_rius_v2 <- c()

for (i in 1:num_elements_v2){
  id_rius_v2 <- c(id_rius_v2,rius_v2[i,1])
}



