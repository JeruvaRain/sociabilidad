#Sociabilidad

#Librerías
library(dplyr)
library(ggplot2)
library(tidyverse)
library(haven)
library(data.table)


#Archivo
PNUD_IDH_2012_SPSS <- read_sav("PNUD IDH 2012 SPSS.sav")
xView(PNUD_IDH_2012_SPSS)

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

#
# Crear una matrix con los mean

Matrix_pnud <- matrix(ncol = 12, byrow= TRUE)
  colnames(Matrix_pnud) <- c("P17.1", "P17.2", "P17.3", "P17.4", "P17.5", "P17.6", "P17.7",
                             "P17.8", "P17.9", "P17.10", "P17.11", "P17.12")
Matrix_pnud <- as.table(Matrix_pnud)

Matrix_pnud[,1] = p17_all_mean$P17.1
Matrix_pnud[,2] = p17_all_mean$P17.2
Matrix_pnud[,3] = p17_all_mean$P17.3
Matrix_pnud[,4] = p17_all_mean$P17.4
Matrix_pnud[,5] = p17_all_mean$P17.5
Matrix_pnud[,6] = p17_all_mean$P17.6
Matrix_pnud[,7] = p17_all_mean$P17.7
Matrix_pnud[,8] = p17_all_mean$P17.8
Matrix_pnud[,9] = p17_all_mean$P17.9
Matrix_pnud[,10] = p17_all_mean$P17.10
Matrix_pnud[,11] = p17_all_mean$P17.11
Matrix_pnud[,12] = p17_all_mean$P17.12

# Matrix convertida a data frame

# Para graficar se utilizó dataf_pnud, quedó probado que funciona mejor que 
# las otras data frames.
dataf_pnud <- as.data.frame(dataf_pnud)


P17_1_12 <- c(p17_all_mean$P17.1:p17_all_mean$P17.12)
as.data.frame(P17_1_12)

# Para ordenar de menor a mayor la data.frame:

dataf_pnud_ord <- dataf_pnud %>%
  arrange(Freq)

# PLOTTING THE MEAN

# Points
ggplot()+
  geom_point(dataf_pnud, mapping = aes(Var2, Freq))+
  ggtitle("Promedios en preguntas de sociabilidad")+
  xlab("")+
  ylab("")

# Bars
ggplot()+
  geom_bar(dataf_pnud, mapping = aes(Var2, Freq), stat = "identity", fill = c(2:13))+
  ggtitle("Promedios: preguntas de sociabilidad")+
    xlab("")+
    ylab("")

# Bars pero ordenado de Mayor a menor
ggplot()+
  geom_bar(dataf_pnud_ord, mapping = aes(reorder(Var2, -Freq), Freq), 
           stat = "identity", fill = c(2:13))+
  ggtitle("Promedios: preguntas de sociabilidad")+
  xlab("")+
  ylab("")
