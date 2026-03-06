#    PROJECTE 
#    
#    Automatizar el procés de depuració de CSV   
#    La ideea poder bolcar el CSV de TOTS els rius de QGIS
#    I que automaticament els SENSE NOMS els hi dongui un nom


library(tidyverse)

#    LLEGIR un CSV
#    Ha estat guardat amb CORTAR/ PEGAR a la carpeta del projecte
#    ASSIGNAR el CSV a una variable RIUS


rius <- read_csv("data/raw/TAULA.csv")

#    veure el tipus de dades que tinc

head(rius)
names(rius)

#   ANALITZAR

#   Vull DETECTAR els ID_2 REPETITS
#   ID_2 REPETITS = vol dir que un RIU SIN NOMBRE ha fet INTERSECT amb RIU AMB NOM (ID)

#   ANALIZAR

#   Creo NOVA TAULA ORDENAD  x ID_2
#   Així podré comparar els ID_2 un darrere l'altre

rius_2 <- rius %>%
  arrange(OBJECTID_2)

#   Fare un FOR per repasar cada ID_2
#   Creo un DATA FRAME on posare NOMÉS els SINNOM amb el NOM NOU
#   Creo id_1 i id_2 que em serveix x comparar un id amb el seu posterior
#   Si id_1 = id_2 creo una nova taula
#   El ID NOU és el ID_2 (el del RIU SENSE NOU)
#   El NOM NOU és el NOM_RIO (el nou del RIU amb NOM que INTERSECTA amb el SIN NOMBRE)
#   Al final tenim la taula del SINOMBRE amb NOM NOU

#   PROBLEMA = a vegades encara queden SINOMBRES
#   El PQ es causa de que hem de tornar a iterar id_1 i id_2
#   El altre PQ és que els CSV de base havia INTERSERCT de un SIN NOMBRE amb un SIN NOMBRE



long <- length(rius_2$nom_rio)

rius_sinnom <- data.frame()
for (i in 1:(long-1)){
  id_1 <- rius_2$OBJECTID_2[i]
  id_2 <- rius_2$OBJECTID_2[i+1]
  
  if(id_1 == id_2 ){
    id_sinnom <- id_1
    nou_nom <- rius_2$nom_rio[i]
    
    rius_sinnom <- rbind(
      rius_sinnom,
      data.frame(OBJECTID = id_sinnom, nom_rio = nou_nom)
    )
  }
  
}




