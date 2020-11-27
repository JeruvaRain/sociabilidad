#Sociabilidad

#Librerías
library(dplyr)
library(ggplot2)
library(tidyverse)
library(haven)
library(data.table)


#Archivo
PNUD_IDH_2012_SPSS <- read_sav("PNUD IDH 2012 SPSS.sav")
View(PNUD_IDH_2012_SPSS)

#################################
#SEPARAR LAS PREGUNTAS DEL PNUD_IDH_2012_SPSS A UN DATA FRAME APARTE

  p17_all <- data.frame(P17.1 = PNUD_IDH_2012_SPSS$P17.1, P17.2 = PNUD_IDH_2012_SPSS$P17.2,
                        P17.3 = PNUD_IDH_2012_SPSS$P17.3, P17.4 = PNUD_IDH_2012_SPSS$P17.4,
                        P17.5 = PNUD_IDH_2012_SPSS$P17.5, P17.6 = PNUD_IDH_2012_SPSS$P17.6,
                        P17.7 = PNUD_IDH_2012_SPSS$P17.7, P17.8 = PNUD_IDH_2012_SPSS$P17.8,
                        P17.9 = PNUD_IDH_2012_SPSS$P17.9, P17.10 = PNUD_IDH_2012_SPSS$P17.10,
                        P17.11 = PNUD_IDH_2012_SPSS$P17.11, P17.12 = PNUD_IDH_2012_SPSS$P17.12
                        )
#################################
# Remover valores por encima del 10
# Esto se puede sintetizar con el uso de una función que no he podido crear aún.
#17.1
p17_all$P17.1[p17_all$P17.1 == 77] <- NA
p17_all$P17.1[p17_all$P17.1 == 88] <- NA
p17_all$P17.1[p17_all$P17.1 == 99] <- NA
#17.2
p17_all$P17.2[p17_all$P17.2 == 77] <- NA
p17_all$P17.2[p17_all$P17.2 == 88] <- NA
p17_all$P17.2[p17_all$P17.2 == 99] <- NA
#17.3 
p17_all$P17.3[p17_all$P17.3 == 77] <- NA
p17_all$P17.3[p17_all$P17.3 == 88] <- NA
p17_all$P17.3[p17_all$P17.3 == 99] <- NA
#17.4
p17_all$P17.4[p17_all$P17.4 == 77] <- NA
p17_all$P17.4[p17_all$P17.4 == 88] <- NA
p17_all$P17.4[p17_all$P17.4 == 99] <- NA

p17_all$P17.5[p17_all$P17.5 == 77] <- NA
p17_all$P17.5[p17_all$P17.5 == 88] <- NA
p17_all$P17.5[p17_all$P17.5 == 99] <- NA

p17_all$P17.6[p17_all$P17.6 == 77] <- NA
p17_all$P17.6[p17_all$P17.6 == 88] <- NA
p17_all$P17.6[p17_all$P17.6 == 99] <- NA

p17_all$P17.7[p17_all$P17.7 == 77] <- NA
p17_all$P17.7[p17_all$P17.7 == 88] <- NA
p17_all$P17.7[p17_all$P17.7 == 99] <- NA

p17_all$P17.8[p17_all$P17.8 == 77] <- NA
p17_all$P17.8[p17_all$P17.8 == 88] <- NA
p17_all$P17.8[p17_all$P17.8 == 99] <- NA

p17_all$P17.9[p17_all$P17.9 == 77] <- NA
p17_all$P17.9[p17_all$P17.9 == 88] <- NA
p17_all$P17.9[p17_all$P17.9 == 99] <- NA

p17_all$P17.10[p17_all$P17.10 == 77] <- NA
p17_all$P17.10[p17_all$P17.10 == 88] <- NA
p17_all$P17.10[p17_all$P17.10 == 99] <- NA

p17_all$P17.11[p17_all$P17.11 == 77] <- NA
p17_all$P17.11[p17_all$P17.11 == 88] <- NA
p17_all$P17.11[p17_all$P17.11 == 99] <- NA

p17_all$P17.12[p17_all$P17.12 == 77] <- NA
p17_all$P17.12[p17_all$P17.12 == 88] <- NA
p17_all$P17.12[p17_all$P17.12 == 99] <- NA

# Obtener el promedio

p17_all_mean <- colMeans(p17_all, na.rm = TRUE)

# Convertir a data frame
p17_all_mean_df <- data.frame(p17_all_mean)
setDT(p17_all_mean_df, keep.rownames = TRUE)
names(p17_all_mean_df) <- c("Pregunta", "Valor")

# PLOTTING THE MEAN

# Points
ggplot()+
  geom_point(p17_all_mean_df, mapping = aes(Pregunta, Valor))+
  ggtitle("Promedios en preguntas de sociabilidad")+
  xlab("")+
  ylab("")

# Bars
ggplot()+
  geom_bar(p17_all_mean_df, mapping = aes(Pregunta, Valor), stat = "identity", fill = c(2:13))+
  ggtitle("Promedios: preguntas de sociabilidad")+
    xlab("")+
    ylab("")

# Bars pero ordenado de Mayor a menor
ggplot()+
  geom_bar(p17_all_mean_df, mapping = aes(reorder(Pregunta, -Valor), Valor), 
           stat = "identity", fill = c(2:13))+
  ggtitle("Promedios: preguntas de sociabilidad")+
  xlab("")+
  ylab("")

#

