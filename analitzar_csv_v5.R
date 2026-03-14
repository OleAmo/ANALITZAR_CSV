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
  
  id <- rius_2$OBJECTID
  nom <- rius_2$nom_rio
  id_2 <- rius_2$OBJECTID_2
  nom_2 <- rius_2$nom_rio_2
  
  #   PRIMER = netejar els ID_1 de ID_2
  #   Els ID_1 són BARREJA de RIUS amb NOM i RIUS sense NOM
  #   Els ID_2 són NOMÉS RIUS SENSE NOM
  #   Pertant lo primer es CREAR una TAULA amb els RIUS amb NOM
  #   Així al final de tot quan tingui la TAULA dels SENSE NOM (ara amb nom), els unire a aquesta taula creada
  #   I així finalment tindre RIUS AMB NOM. Tant els que ja en tenien com el SENSE NOM que li he donat nom
  
  #   PROCÉS:
  #   Elimino de RIUS_2 els IDs que son iguals a ID_2
  #   Així tindre la taula de RIUS amb NOM que ja venen així del CSV
  #   Un cop fet DEIXO els VALORS ÚNICS
  
  rius_2_nom <- rius_2 %>%
    filter(!(rius_2$OBJECTID %in% rius_2$OBJECTID_2)) 
  
  rius_2_nom <- rius_2_nom %>%              # Elimino els ID repetits
    distinct(OBJECTID, .keep_all = TRUE)    # I deixo un sola fila dels IDs repetits
                                            # ho ha sabut fer gràcies a CHATGPT 
   
  #   SEGÜENT PROCÉS:
  #   Un cop creada la TAULA amb NOM ORIGINAL = RIUS_2_NOM
  #   Ara ja jodem ANARLITZAR els ID_2 per donar NOM als SENSE NOM
  
  #   Comparo si ID_2 es IGUAL al seguent ID_2
  #   Si es així poso el ID_2 i el NOM els poso en un DATAFRAME
  
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
  
  
  #  CONTINUAAAAAAAAAAAAARRRRRRRRRR
  #  CONTINUAAAAAAAAAAAAARRRRRRRRRR
  #  CONTINUAAAAAAAAAAAAARRRRRRRRRR
  
  
  llista <- list(
    id_1 =id,
    id_2 =id_2,
    df_1 =rius_sinnom,
    df_2 =rius_sinnom_2,
    ORIGINAL_nom = rius_2_nom,
    ORIGINAL = rius_2
  )

  return(llista)
}

llista <- netja_csv(rius)

unique(llista$id_1)
unique(llista$id_2)


llista$ORIGINAL
llista$df_1
llista$df_2
llista$ORIGINAL_nom

  
