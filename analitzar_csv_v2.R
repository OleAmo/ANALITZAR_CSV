
library(tidyverse)
library(sf) 

rius <- data.frame(
  id = 1:10,
  nom = c("Riu_A","Riu_B","Riu_C","Riu_D","Riu_E",
          "Riu_F","Riu_G","Riu_H","Riu_I","Riu_J"),
  longitud = c(56, 56,
               44, 44,
               120, 120,
               80, 80,
               95, 200)
)

rius
