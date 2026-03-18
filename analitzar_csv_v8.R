#    PROJECTE 
#    
#   -------------- USARE UN NOVA TAULA DE CSV --------------
#   --------------------------------------------------------


library(tidyverse)

#    LLEGIR un CSV
#    Ha estat guardat amb CORTAR/ PEGAR a la carpeta del projecte
#    ASSIGNAR el CSV a una variable RIUS


rius_b <- read.csv("data/raw/TAULA_2.csv")

#   OBJECTIU

#   AUTOMATIZAR la NETEJA de una taula CSV
#   Ha de ser una funcio que introdueixis un CSV i et "NETEGI" els sense noms





#   -----------------------------
#   PRIMER DE TOT = ANLITZAR CSV
#   -----------------------------


#   Vull saber de cada CSV quants ID_1 ÚNICS i ID_2 ÚNICS hi ha.
#   Amb aquest recompte sabré si em descuido algun ID en tot el proces.

#   ID_1 ÚNICS = Són rius amb NOM
#   ID_2 ÚNICS = Són rius amb SENSE NOM = Els hauré de processar x donar nom

analitzar_csv <- function(rius) {
  
  rius_2 <- rius %>%     # Ordeno RIUS per OBJECTID_2
    arrange(OBJECTID_2)
  
  id <- rius_2$OBJECTID
  nom <- rius_2$nom_rio
  id_2 <- rius_2$OBJECTID_2
  nom_2 <- rius_2$nom_rio_2
  
  rius_AMB_NOM <- rius_2 %>%
    filter(!(id %in% id_2))
  
  rius_AMB_NOM <- rius_AMB_NOM %>%          # Elimino els ID repetits
    distinct(OBJECTID, .keep_all = TRUE)    # I deixo un sola fila dels IDs repetits
                                            # ho ha sabut fer gràcies a CHATGPT
 
   rius_AMB_NOM <- rius_AMB_NOM %>%         # Elimino les columnes ID_2 i NOM_2 que no m'interessen
    select(-c(OBJECTID_2, nom_rio_2))       # Al final només voldre columna OBJECTID i NOM_RIO
  
  
   
   # Un cop tinc la taula dels RIUS ORIGINALS AMB NOM
   # Vull saber els IDs de  NOM i IDS sense nom
   
   
   id_amb_nom <- unique(rius_AMB_NOM$OBJECTID)
   
   # Ara busco els ID SENSE NOM
   # Seran només els de la COLUMNA ID_2
   # Eliminare els ID AMB NOM
   # I els filtraré
   
   rius_SENSE_NOM <- rius_2 %>%
     filter(!(id_2 %in% id_amb_nom))
   
   rius_SENSE_NOM <- rius_SENSE_NOM %>%          # Elimino els ID repetits
     distinct(OBJECTID_2, .keep_all = TRUE)      # I deixo un sola fila dels IDs repetits
                                                 # ho ha sabut fer gràcies a CHATGPT
   
   rius_SENSE_NOM <- rius_SENSE_NOM %>%         # Elimino les columnes ID i NOM_que no m'interessen
     select(-c(OBJECTID, nom_rio))              # Al final només voldre columna OBJECTID_2 i NOM_RIO_2
   
   id_sense_nom <- unique(rius_SENSE_NOM$OBJECTID_2)
   
   
   
   #  ARA CALCULO ELS TOTALS
   #  Abans de fer-ho canvio el noms de SENSE NOMS
   #  Així podre ajuntar columnes
   
   names(rius_SENSE_NOM) <- c("OBJECTID", "nom_rio")
   
   rius_TOTS <- rbind(rius_SENSE_NOM , rius_AMB_NOM) 
   
   rius_TOTS <- rius_TOTS %>%  # Ordeno RIUS per OBJECTID
     arrange(OBJECTID)
   
   id_tots <- unique(rius_TOTS$OBJECTID)
   
   
   llista <- list(
    RIUS_ORIGINAL =rius_2,
    RIUS_AMB_NOM =rius_AMB_NOM,
    RIUS_SENSE_NOM =rius_SENSE_NOM,
    RIUS_TOTS = rius_TOTS,
    id_AMB_NOM = id_amb_nom,
    id_SENSE_NOM = id_sense_nom,
    id_TOTS = id_tots
    
    
  )
  
  return(llista)
  
}





#   -----------------------------
#   SEGON = PROCESSAR CSV
#   -----------------------------


#  En aquesta versió la FUNCIÓ NETEJA_CSV tindrà DOS ATRIBUTS
#     1r.) El CSV ORIGINAL
#     2n.) El DF de NOMS_ORIGINALS = Creat per funció ANALISIS


netja_csv <- function(rius, rius_noms) {

  rius_2 <- rius %>%     # Ordeno RIUS per OBJECTID_2
    arrange(OBJECTID_2)
  
  id <- rius_2$OBJECTID
  nom <- rius_2$nom_rio
  id_2 <- rius_2$OBJECTID_2
  nom_2 <- rius_2$nom_rio_2
  
  
  # OBJECTIU:
  
  # La columna ID_2 estan repetits
  # He de trobar els REPETITS i fer:
  #    -) GUARDO ID_2
  #    -) GUARDO NOM_1
  
  # PROCÉS:
  
  # Creo una llista amb els ID_2 UNICS
  # Faig un FOR recorrent aquesta llista:
  # De cada valor de la llista CREO un VECTOR de INDEX
  # Aquests INDEX indiquen ON de la TAULA PRINCIPAL es repeteix el ID_2
  # D'aquest VECTOR em quedo amb el 1r INDEX = Que és una posicio de una FILA al RIUS ORIGINAL
  # I d'aquest INDEX n'extrec el ID_2 i el NOM_RIO
  # Per tant al ID_2 que era SIN NOMBRE li donc un valor de NOM_RIO
  # I creoo una TAUALA PER COLOCARLOS
  # Fent-ho així TINC JA una TAULA amb ID_2 NO REPETITS i AMB NOM

  
  id_2_unics <- unique(id_2)
  
  rius_sinnom_1 <- data.frame()
  for (i in id_2_unics){
    index_ids <- which(id_2 == i)
    index_num <- index_ids[1]
    
    id_select <- id_2[index_num]
    nom_select <- nom[index_num]
    
    rius_sinnom_1 <- rbind(
      rius_sinnom_1,
      data.frame(OBJECTID = id_select, nom_rio = nom_select)
    )
  }
  
  
  # ARA UNEIXO = SINOM_1 + RIUS_AMB_NOM
  
  #    -) els SINOM_1 = Son els SENSE NOM ara amb NOM
  #    -) RIUS_AMB_NOM = Son els RIUS AMB NOM de la TAULA ORIGINAL
  
  rius_final_sensenom <- rius_sinnom_1
  rius_final_nom <- rius_noms
  
  rius_FINAL_TOTS <- rbind(rius_final_sensenom , rius_final_nom) 
  
  rius_FINAL_TOTS <- rius_FINAL_TOTS %>%  # Ordeno RIUS per OBJECTID
    arrange(OBJECTID)
  
 

  
  llista <- list(
    
    ORIGINAL = rius_2,
    SINNOM_1 = rius_sinnom_1,
    FINAL_TOTS = rius_FINAL_TOTS
  )

  return(llista)
}

analisis <- analitzar_csv(rius_b)
llista <- netja_csv(rius_b,analisis$RIUS_AMB_NOM)

llista$SINNOM_1
llista$ORIGINAL
llista$FINAL_TOTS

analisis$RIUS_TOTS




#   -----------------------------
#   -----------------------------
#       PRODUCTE FNIAL
#   -----------------------------
#   -----------------------------

# AIXÒ SON ELS PRODUCTE FINALS DE L'ANLISIS

rius_FINAL_TOTS
analisis$id_TOTS
analisis$RIUS_TOTS


