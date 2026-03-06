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

#   Creo NOVA TAULA ORDENAD  x ID_2
#   Així podré comparar els ID_2 un darrere l'altre

long <- length(rius_2$nom_rio)

rius_nom_nou <- data.frame()
for (i in 1:(long-1)){
  id_1 <- rius_2$OBJECTID_2[i]
  id_2 <- rius_2$OBJECTID_2[i+1]
  
  if(id_1 == id_2 ){
    id_sinnom <- id_1
    nou_nom <- rius_2$nom_rio[i]
    
    rius_nom_nou <- rbind(
      rius_nom_nou,
      data.frame(OBJECTID = id_sinnom, nom_rio = nou_nom)
    )
    
  }
  
  
}




