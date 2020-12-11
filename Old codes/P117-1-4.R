# Script para preguntas 

# ¿CON QUÉ FRECUENCIA REALIZA USTED LAS SIGUIENTES ACTIVIDADES CON SU FAMILIA? 
# (CONVERSAR SOBRE ASUNTOS FAMILIARES)
# P117.1 ~ P117.4


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
p117 <- data.frame(P117.1 = PNUD_IDH_2012_SPSS$P117.1,
                  P117.2 = PNUD_IDH_2012_SPSS$P117.2,
                  P117.3 = PNUD_IDH_2012_SPSS$P117.3,
                  P117.4 = PNUD_IDH_2012_SPSS$P117.4)
# # Remover valores por encima del 10

# Remover NA's con la función del paquete naniar "replace_with_na".

p117n <- p117 %>% replace_with_na(replace = list(P117.1 = c(77, 88, 99),
                                        P117.2 = c(77, 88, 99),
                                        P117.3 = c(77, 88, 99),
                                        P117.4 = c(77, 88, 99)))
# Transformar el df anterior utilizando un transpose para ello (t)
p117nf <- as.data.frame(t(p117n))

# Obtener el promedio de cada fila y cambiar los nombres del data.frame
p117mean <- rowMeans(p117nf, na.rm = TRUE)

p117df <- data.frame(p117mean)
setDT(p117df, keep.rownames = TRUE)
names(p117df) <- c("Pregunta", "Valor")

#plotting

ggplot()+
  geom_bar(p117df, mapping = aes(Pregunta, Valor), stat = "identity", 
           position = "dodge", width = 0.5, fill = c("black", "darkblue", "brown", "darkgreen"))+
  ggtitle("Promedios de preguntas 117")+
  theme_linedraw()

      