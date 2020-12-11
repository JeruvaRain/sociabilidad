# P121.1 ¿CUÁN DE ACUERDO SE ENCUENTRA CON LAS SIGUIENTES AFIRMACIONES? 

# p121.1 ~ p121.3

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
p121 <- data.frame(P121.1 = PNUD_IDH_2012_SPSS$P121.1,
                   P121.2 = PNUD_IDH_2012_SPSS$P121.2,
                   P121.3 = PNUD_IDH_2012_SPSS$P121.3)

# Remover NA's con la función del paquete naniar "replace_with_na".
p121n <- p121 %>% replace_with_na(replace = list(P121.1 = c(77, 88, 99),
                                                 P121.2 = c(77, 88, 99),
                                                 P121.3 = c(77, 88, 99)))

# Transformar el df anterior utilizando un transpose para ello (t)
p121nf <- as.data.frame(t(p121n))

# Obtener el promedio de cada fila y cambiar los nombres del data.frame
p121mean <- rowMeans(p121nf, na.rm = TRUE)

p121df <- data.frame(p121mean)
setDT(p121df, keep.rownames = TRUE)
names(p121df) <- c("Pregunta", "Valor")

# Plotting
ggplot()+
  geom_bar(p121df, mapping = aes(Pregunta, Valor), stat = "identity", width = 0.6)+
  theme_classic()
