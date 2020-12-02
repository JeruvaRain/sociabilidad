# P139.1 ¿ESTÁ ENTRE LAS COSAS QUE MÁS LE GUSTA HACER O QUE LE GENERAN MUCHO PLACER SALIR A COMER FUERA DE LA CASA?
# P139.2 ¿ESTÁ ENTRE LAS COSAS QUE MÁS LE GUSTA HACER O QUE LE GENERAN MUCHO PLACER CARRETEAR O SALIR A FIESTAS?
#  P139.3 ¿ESTÁ ENTRE LAS COSAS QUE MÁS LE GUSTA HACER O QUE LE GENERAN MUCHO PLACER COMPARTIR CON LOS AMIGOS?
#  P139.4 ¿ESTÁ ENTRE LAS COSAS QUE MÁS LE GUSTA HACER O QUE LE GENERAN MUCHO PLACER COMPARTIR CON MASCOTAS?
#  P139.5 ¿ESTÁ ENTRE LAS COSAS QUE MÁS LE GUSTA HACER O QUE LE GENERAN MUCHO PLACER COMPRARSE ALGO PARA DARSE UN GUSTO?
#  P139.6 ¿ESTÁ ENTRE LAS COSAS QUE MÁS LE GUSTA HACER O QUE LE GENERAN MUCHO PLACER HACER ACTIVIDADES EN LA NATURALEZA?
#  P139.7 ¿ESTÁ ENTRE LAS COSAS QUE MÁS LE GUSTA HACER O QUE LE GENERAN MUCHO PLACER HACER DEPORTES?
#  P139.8 ¿ESTÁ ENTRE LAS COSAS QUE MÁS LE GUSTA HACER O QUE LE GENERAN MUCHO PLACER DEDICARSE A UN PASATIEMPO O HOBBIE?
#  P139.9 ¿ESTÁ ENTRE LAS COSAS QUE MÁS LE GUSTA HACER O QUE LE GENERAN MUCHO PLACER SALIR SÓLO CON SU PAREJA?
  
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
p139 <- data.frame(P139.1 = PNUD_IDH_2012_SPSS$P139.1,
                   P139.2 = PNUD_IDH_2012_SPSS$P139.2,
                   P139.3 = PNUD_IDH_2012_SPSS$P139.3,
                   P139.4 = PNUD_IDH_2012_SPSS$P139.4,
                   P139.5 = PNUD_IDH_2012_SPSS$P139.5,
                   P139.6 = PNUD_IDH_2012_SPSS$P139.6,
                   P139.7 = PNUD_IDH_2012_SPSS$P139.7,
                   P139.8 = PNUD_IDH_2012_SPSS$P139.8,
                   P139.9 = PNUD_IDH_2012_SPSS$P139.9)

# Remover los NA
p139n <- p139 %>% replace_with_na(replace = list(P139.1 = c(77, 88, 99),
                                                 P139.2 = c(77, 88, 99),
                                                 P139.3 = c(77, 88, 99),
                                                 P139.4 = c(77, 88, 99),
                                                 P139.5 = c(77, 88, 99),
                                                 P139.6 = c(77, 88, 99),
                                                 P139.7 = c(77, 88, 99),
                                                 P139.8 = c(77, 88, 99),
                                                 P139.9 = c(77, 88, 99)))
# Transformar el df anterior utilizando un transpose para ello (t)
p139nf <- as.data.frame(t(p139n))

# Obtener el promedio de cada fila y cambiar los nombres del data.frame
p139mean <- rowMeans(p139nf, na.rm = TRUE)

p139df <- data.frame(p139mean)
setDT(p139df, keep.rownames = TRUE)
names(p139df) <- c("Pregunta", "Valor")

# PLOTTING

ggplot()+
  geom_bar(p139df, mapping = aes(Pregunta, Valor), stat = "identity", width = 0.6)+
  theme_classic()
  