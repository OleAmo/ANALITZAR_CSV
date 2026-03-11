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
#       El PQ es causa de que hem de tornar a iterar id_1 i id_2
#       El altre PQ és que els CSV de base havia INTERSERCT de un SIN NOMBRE amb un SIN NOMBRE



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

#   NOVA ITERACIÓ
#   Intentar donar nom als SIN NOMBRE

long <- length(rius_sinnom$nom_rio)

rius_sinnom_2 <- data.frame()
for (i in 1:(long-1)){
  id_1 <- rius_sinnom$OBJECTID[i]
  id_2 <- rius_sinnom$OBJECTID[i+1]
  
  if(id_1 == id_2 ){
    id_sinnom_2 <- id_1
    nou_nom_2 <- rius_sinnom$nom_rio[i]
    
    rius_sinnom_2 <- rbind(
      rius_sinnom_2,
      data.frame(OBJECTID = id_sinnom_2, nom_rio = nou_nom_2)
    )
  }
}

#   Ara ja tenim la 2a TAULA DEPURADA!
#   I també la 1ra DEPURADA que se li  han de treure les files de TAULA DEPURADA 2

rius_sinnom
rius_sinnom_2

#   Ara de la TAULA SINNOM li treiem els SINOMS_2
#   El resulat es SINOM_3


rius_sinnom_3 <- rius_sinnom %>%
  filter(!(rius_sinnom$OBJECTID %in% rius_sinnom_2$OBJECTID)) 

#   Ara tenim SINOM_3 i SINOM_2
#   Son dues taules amb IDs no REPETITS 
#   Les ajuntarem i seran SIN_NOM_FINAL

rius_sinnom_3
rius_sinnom_2

#   Abans comprovem que CAP ID de SINOM_3 esta SINOM2
#   Faig un FOR de SINOM_2
#   Per cada ID de SINOM_2 miro si està  (%in%) en SINOM_3
#   Si dona FALSE cada un vol dir que no estan repetits

long_2 <- length(rius_sinnom_2$OBJECTID)


for (i in 1:long_2){
  boolean <- rius_sinnom_2$OBJECTID[i] %in% rius_sinnom_3$OBJECTID
  if(boolean == FALSE){
    print('No REPETIT')
  }
  
}

#   Un cop comprovat, ja podem UNIR sinom_3 i sionom_2

rius_sinom_final <- rbind(rius_sinnom_3 , rius_sinnom_2)


#   Ara s'haura de unir a RIUS
#   Primer a RIUS li teriem els IDs de SINOM (que en la taula original estan repetits)
#   I un cop trets li UNIM RIUS_SINOM FINAL

rius_2
rius_sinom_final

#   Ara de RIUS_2 (amb els ID ORDENATS) he de eliminiar els SINOMBRE
#   Per ferho ELIMINAREM les files de RIUS_2 que estiguin dins de RIUS_SINOMFINAL

#   Si que RIUS_2 te OBJECTIDs repetits
#   Això passa pk la taula CSV original eren INTERSECTS i  passava aixó

#   PRIMER = he de ORDENAR rius_2 per OBJECTID 
#   -------

rius_2_ordrenat <- rius_2 %>%
  arrange(OBJECTID)

long <- length(rius_2_ordrenat$OBJECTID)

for (i in 1:long){
  id <- rius_2_ordrenat$OBJECTID[i]
  nom <- rius_2_ordrenat$nom_rio[i]
  print(paste(id, ' - ',nom))
  
}

#   SEGON = he de compmrovar que els noms son iguals abans de eliminar repetits
#   Primer de tot he de tenir els ID NO REPTITS
#   Ho faig amb  UNIQUE()
#   Un cop ho tingui, de cada ID no repetit buscaré en QUINS INDEX ES REPETEIX
#   I de cada un de ells GUARDARÉ el NOM en un VECTOR
#   Després de cada vector miraré si els NOMS es REPETEIXEN o no
#   Ara que ja sabem que TOTS els IDs REPETITS tenen UN SOL NOM
#   Ja podem CREAR un nou DATA FREME = RIUS_2_ORDENAT_NET
#   El qual tindpa ID unics amb cada NOM


rius_2_ordrenat <- rius_2 %>%
  arrange(OBJECTID)

long <- length(rius_2_ordrenat$OBJECTID)

unics_id <- unique(rius_2_ordrenat$OBJECTID)
long_unics_id <- length(unics_id)

rius_2_ordrenat_net <- data.frame() # nou DATA FRAME on posare els ID unics amb el NOM

for (i in 1:long_unics_id){
  id <- unics_id[i]
  index_val <- which(rius_2_ordrenat$OBJECTID == id)
  
  vector_noms <- c() # vector on posare tots els noms de cada ID
  long_index_val <- length(index_val)
  
  for (a in 1:long_index_val){
    nom <- rius_2_ordrenat$nom_rio[index_val[a]]  # cada nom que té cada ID repetit
    vector_noms <- c(vector_noms,nom )
  }
  
  unic_nom <- unique(vector_noms)   # vector noms el passo a no repetits 
  long_unic_nom <- length(unic_nom) # calculo longitud d'aquest vector
  
  if(long_unic_nom==1){
    print(paste(id,' té nomes UN nom = ',unic_nom[1]))
  }
  
  #   Ja podem CREAR un nou DATA FREME = RIUS_2_ORDENAT_NET
  #   El qual tindpa ID unics amb cada NOM
  
  rius_2_ordrenat_net <- rbind(
    rius_2_ordrenat_net,
    data.frame(OBJECTID = id, nom_rio= unic_nom[1])
  )
  
}

rius_2_ordrenat_net





