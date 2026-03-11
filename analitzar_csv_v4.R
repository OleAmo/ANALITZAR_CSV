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


#   OBJECTIU

#   AUTOMATIZAR la NETEJA de una taula CSV
#   Ha de ser una funcio que introdueixis un CSV i et "NETEGI" els sense noms



netja_csv <- function(rius) {
  columnes <- length(rius[1,])
  id <- rius[,1]
  nom <- rius[,2]
  id_2 <- rius[,3]
  nom_2 <- rius[,4]
  
  rius_2 <- rius %>%     # Ordeno RIUS per ID_2
    arrange(id_2)
  
  long <- length(nom)             # Comparo si ID_2 es IGUAL al seguent ID_2
  rius_sinnom <- data.frame()     # Si es així poso el ID_2 i el NOM_2 un DATAFRAME
  for (i in 1:(long-1)){          # Així tindré la taula dels SINOMBRE
    
  }
  
  print(long)
  
  
 
  #return(rius_2)
}

netja_csv(rius)

