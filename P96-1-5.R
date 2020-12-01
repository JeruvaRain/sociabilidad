# Script para preguntas 

# P96.1. UTILIZANDO LA MISMA ESCALA DE NOTAS DE 1 A 7, QUIERO PEDIRLE QUE LE SIGA PONIENDO 
# NOTA A CHILE. ¿QUÉ NOTA LE PONE A CHILE EN LAS OPORTUNIDADES PARA QUE LAS PERSONAS PUEDAN 
# ESTAR EN CONTACTO CON LA NATURALEZA?

# Librerías
library(dplyr)
library(ggplot2)
library(tidyverse)
library(haven)
library(data.table)

# Archivo
PNUD_IDH_2012_SPSS <- read_sav("PNUD IDH 2012 SPSS.sav")

# Separar las preguntas a un nuevo data frame
p96 <- data.frame(P96.1 = PNUD_IDH_2012_SPSS$P96.1,
                  P96.2 = PNUD_IDH_2012_SPSS$P96.2,
                  P96.3 = PNUD_IDH_2012_SPSS$P96.3,
                  P96.4 = PNUD_IDH_2012_SPSS$P96.4,
                  P96.5 = PNUD_IDH_2012_SPSS$P96.5)

# # Remover valores por encima del 10
# Esto se puede sintetizar con el uso de una función que no he podido crear aún.

p96$P96.1[p96$P96.1 == 88] <- NA
p96$P96.1[p96$P96.1 == 99] <- NA

p96$p96.2[p96$p96.2 == 88] <- NA
p96$P96.2[p96$p96.1 == 99] <- NA

p96$P96.3[p96$P96.3 == 88] <- NA
p96$P96.3[p96$P96.3 == 99] <- NA

p96$P96.4[p96$P96.4 == 88] <- NA
p96$P96.4[p96$p96.4 == 99] <- NA

p96$P96.5[p96$P96.5 == 88] <- NA
p96$P96.5[p96$p96.5 == 99] <- NA

# Obtener el promedio
p96mean <- colMeans(p96, na.rm = TRUE)

# Convertir la tabla con promedios en data.frame
p96df <- data.frame(p96mean)
setDT(p96df, keep.rownames = TRUE)
names(p96df) <- c("Pregunta", "Valor")

# Plot

# Bars
ggplot()+
  geom_bar(p96df, mapping = aes(Pregunta, Valor), stat = "identity", fill = c(1:5))+
  ggtitle("Promedios: preguntas 96.1 a 96.5")+
  xlab("")+
  ylab("")