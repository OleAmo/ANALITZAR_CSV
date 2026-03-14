#    PROJECTE 
#    
#    Automatizar el procés de depuració de CSV   
#    La ideea poder bolcar el CSV de TOTS els rius de QGIS
#    I que automaticament els SENSE NOMS els hi dongui un nom


library(tidyverse)

#    LLEGIR un CSV
#    Ha estat guardat amb CORTAR/ PEGAR a la carpeta del projecte
#    ASSIGNAR el CSV a una variable RIUS


rius <- read.csv("data/raw/TAULA.csv")


#   OBJECTIU

#   AUTOMATIZAR la NETEJA de una taula CSV
#   Ha de ser una funcio que introdueixis un CSV i et "NETEGI" els sense noms



netja_csv <- function(rius) {
  columnes <- length(rius[1,])
  
  rius_2 <- rius %>%     # Ordeno RIUS per OBJECTID_2
    arrange(OBJECTID_2)
  
  id <- rius_2[,1]
  nom <- rius_2[,2]
  id_2 <- rius_2[,3]
  nom_2 <- rius_2[,4]
  
  #   PRIMER = netejar els ID_1 de ID_2
  #   Els ID_1 són BARREJA de RIUS amb NOM i RIUS sense NOM
  #   Els ID_2 són NOMÉS RIUS SENSE NOM
  #   Pertant lo primer es CREAR una TAULA amb els RIUS amb NOM
  #   Així al final de tot quan tingui la TAULA dels SENSE NOM (ara amb nom), els unire a aquesta taula creada
  #   I així finalment tindre RIUS AMB NOM. Tant els que ja en tenien com el SENSE NOM que li he donat nom
  
  
  

  return(columnes)
}

llista <- netja_csv(rius)

llista

  
