# P118. EN EL ÚLTIMO MES, ¿CUÁNTAS VECES HA SIDO INVITADO/A A LA CASA DE AMIGOS O INVITADO/A A SALIR?
# P119. CON RESPECTO AL TEMA DE LA AMISTAD, ¿USTED DIRÍA QUE...?

# Librerías
library(dplyr)
library(ggplot2)
library(tidyverse)
library(haven)
library(data.table)
#For replacing NA's
library(naniar)

# Archivo
PNUD_IDH_2012_SPSS <- read_sav("PNUD IDH 2012 SPSS.sav")

# Separar las preguntas a un nuevo data frame
P118_9 <- data.frame(P118 = PNUD_IDH_2012_SPSS$P118,
                     P119 = PNUD_IDH_2012_SPSS$P119)

# Remover los NA
P118_9n <- P118_9 %>% replace_with_na(replace = list(P118 = c(88, 99),
                                                     P119 = c(88, 99)))

# Transformar el df anterior utilizando un transpose para ello (t)
P118_9nf <- as.data.frame(t(P118_9n))

# Obtener el promedio de cada fila y cambiar los nombres del data.frame
P118_9mean <- rowMeans(P118_9nf, na.rm = TRUE)

P118_9df <- data.frame(P118_9mean)
setDT(P118_9df, keep.rownames = TRUE)
names(P118_9df) <- c("Pregunta", "Valor")

# Plotting
ggplot()+
  geom_bar(P118_9df, mapping = aes("P118"), width = 0.5)+
  geom_bar(P118_9df, mapping = aes("P119"), width = 0.5)+
  facet_wrap(~"P118")+
  scale_y_discrete(name = "Total",  limits = c(0,0.5,1, 1.5, 2, 2.5))
  
           