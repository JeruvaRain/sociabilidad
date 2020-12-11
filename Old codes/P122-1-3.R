# P122.1 ¿SIENTE QUE LE RECONOCEN TODO EL ESFUERZO O APORTE QUE USTED HACE EN SU FAMILIA?
# P122.2 ¿SIENTE QUE LE RECONOCEN TODO EL ESFUERZO O APORTE QUE USTED HACE EN SU TRABAJO?
# P122.3 ¿SIENTE QUE LE RECONOCEN TODO EL ESFUERZO O APORTE QUE USTED HACE EN SU RELACIÓN 
 # DE PAREJA?

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
p122 <- data.frame(P122.1 = PNUD_IDH_2012_SPSS$P122.1,
                   P122.2 = PNUD_IDH_2012_SPSS$P122.2,
                   P122.3 = PNUD_IDH_2012_SPSS$P122.3)

# Remover NA's con la función del paquete naniar "replace_with_na".
p122n <- p122 %>% replace_with_na(replace = list(P122.1 = c(77, 88, 99),
                                                 P122.2 = c(77, 88, 99),
                                                 P122.3 = c(77, 88, 99)))

# Transformar el df anterior utilizando un transpose para ello (t)
p122nf <- as.data.frame(t(p122n))

# Obtener el promedio de cada fila y cambiar los nombres del data.frame
p122mean <- rowMeans(p122nf, na.rm = TRUE)

p122df <- data.frame(p122mean)
setDT(p122df, keep.rownames = TRUE)
names(p122df) <- c("Pregunta", "Valor")

# Plotting

ggplot()+
  geom_bar(p122df, mapping = aes(Pregunta, Valor), stat = "identity", width = 0.7)+
  theme_classic()
