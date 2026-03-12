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
  
            # Comparo si ID_2 es IGUAL al seguent ID_2
            # Si es així poso el ID_2 i el NOM els poso en un DATAFRAME
 
   long <- length(id)              
  rius_sinnom <- data.frame()    
  for (i in 1:(long-1)){
    id__1 <- id_2[i]
    id__2 <- id_2[i+1]
    
    if(id__1 == id__2){
      id_sinnom <- id__1
      nou_nom <- nom[i]
      
      rius_sinnom <- rbind(
        rius_sinnom,
        data.frame(OBJECTID = id_sinnom, nom_rio = nou_nom)
      )
    }
  }
  
  # Faig NOVA ITERACIÓ de RIUS_SINOM per NETEJAR els ID REPETITS
 
  long_2 <- length(rius_sinnom[,1])         
  rius_sinnom_2 <- data.frame()
  
  for (i in 1:(long_2-1)){
    id__1 <- rius_sinnom$OBJECTID[i]
    id__2 <- rius_sinnom$OBJECTID[i+1]
    
    if(id__1 == id__2){
      id_sinnom_2 <- id__1
      nou_nom_2 <- rius_sinnom$nom_rio[i]
      
      rius_sinnom_2 <- rbind(
        rius_sinnom_2,
        data.frame(OBJECTID = id_sinnom_2, nom_rio = nou_nom_2)
      )
    }
  }
  
  #   Ara que ja tinc RIUS_SINOM_2 una TAULA de ID amb NOMS
  #   Ara que ja tinc RIUS_SINOM una TAULA de ID REPETITS amb SENSE NOMS
  #   He de crear un TUALA RIUS_SINOM_3
  #   Aquesta taula serà els la RIUS_SINOM eliminant els ID REPETITS  
  #   Justament els repetits son els ID de RIUS_SINOM_2
  #   Un cop eliminats li afegirem els ID de RIUS_SIOM_2
  
  rius_sinnom_3 <- rius_sinnom %>%
    filter(!(rius_sinnom$OBJECTID %in% rius_sinnom_2$OBJECTID)) 
  
  long <- length(rius_sinnom_2$OBJECTID)
  for (i in 1:long ){
    
    rius_sinnom_3 <- rbind(
      rius_sinnom_3,
      data.frame(OBJECTID = rius_sinnom_2$OBJECTID[i], 
                 nom_rio = rius_sinnom_2$nom_rio[i])
    )
  }
  
  rius_sinnom_3 <- rius_sinnom_3 %>%     # Ordeno per OBJECTID
    arrange(OBJECTID)
  
  rius <- rius %>%                       # Ordeno per OBJECTID
    arrange(OBJECTID)
  
  
  llista <- list(
    df_1 =rius_sinnom,
    df_2 = rius_sinnom_2,
    df_FINAL = rius_sinnom_3,
    df_ORIGINAL = rius
  )

 
  return(llista)
}

llista <- netja_csv(rius)

llista$df_1
llista$df_2
llista$df_FINAL
llista$df_ORIGINAL

 





#   prova




  
