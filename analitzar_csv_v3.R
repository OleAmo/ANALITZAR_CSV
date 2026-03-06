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

#   ORDENAR una taula x ID_2
#   Així puc veure les dades ordenades

rius %>%
  arrange(OBJECTID_2)

#   ANALIZAR

long <- length(rius$nom_rio)

