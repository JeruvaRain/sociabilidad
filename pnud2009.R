# Encuesta PNUD 2009

#Librerías
library(dplyr)
library(ggplot2)
library(tidyverse)
library(haven)
library(data.table)
library(naniar) #For replacing NA's

## Archivo a utilizar

PNUD2009 <- read_sav("PNUD IDH 2009 SPSS.sav")

# P12 Generalmente las personas como usted

# Separar las preguntas

# P15
P15 <- data.frame(P15 = PNUD2009$P15)
# P16
P16 <- data.frame(P16 = PNUD2009$P16)
# P17A, P17C
P17 <- data.frame(P17A = PNUD2009$P17_A,
                      P17C = PNUD2009$P17_C)
# P29
P29 <- data.frame(P29B = PNUD2009$P29_B)
# P62
P62 <- data.frame(P62_1 = PNUD2009$P62_1,
                      P62_2 = PNUD2009$P62_2)
# P63
P63 <- data.frame(P63_1 = PNUD2009$P63_1,
                      P63_2 = PNUD2009$P63_2)
# P101A, P101C
P101 <- data.frame(P101A = PNUD2009$P101_A,
                      P101C = PNUD2009$P101_C)
# P102A, P102B, P102C, P102D, P102E
P102 <- data.frame(P102A = PNUD2009$P102_A,
                        P102B = PNUD2009$P102_B,
                        P102C = PNUD2009$P102_C,
                        P102D = PNUD2009$P102_D,
                        P102E = PNUD2009$P102_E)
# P108A, P108B, P108C, P108D
P108 <- data.frame(P108A = PNUD2009$P108_A,
                        P108B = PNUD2009$P108_B,
                        P108C = PNUD2009$P108_C,
                        P108D = PNUD2009$P108_D)
# P114
P114 <- data.frame(P114 = PNUD2009$P114)

## REMOVER NA'S

P15 <- P15 %>% replace_with_na(replace = list(P15 = c(8,9)))
P16 <- P16 %>% replace_with_na(replace = list(P16 = c(8,9)))
P17 <- P17 %>% replace_with_na(replace = list(P17A = c(5,8,9),
                                              P17C = c(5,8,9)))
P29 <- P29 %>% replace_with_na(replace = list(P29B = c(8,9)))
P62 <- P62 %>% replace_with_na(replace = list(P62_1 = c(8,9),
                                              P62_2 = c(8,9)))
P63 <- P63 %>% replace_with_na(replace = list(P63_1 = c(8,9),
                                              P63_2 = c(8,9)))
P101 <- P101 %>% replace_with_na(replace = list(P101A = c(8,9),
                                                P101C = c(8,9)))
P102 <- P102 %>% replace_with_na(replace = list(P102A = c(8,9),
                                                P102B = c(8,9),
                                                P102C = c(8,9),
                                                P102D = c(8,9),
                                                P102E = c(8,9)))
P108 <- P108 %>% replace_with_na(replace = list(P108A = c(88,98,99),
                                                P108B = c(88,98,99),
                                                P108C = c(88,98,99),
                                                P108D = c(88,98,99)))
P114 <- P114 %>% replace_with_na(replace = list(P114 = c(8,9)))



## Transformar el df anterior utilizando un transpose para ello (t)

P15 <- as.data.frame(t(P15))
P16 <- as.data.frame(t(P16))
P17 <- as.data.frame(t(P17))
P29 <- as.data.frame(t(P29))
P62 <- as.data.frame(t(P62))
P63 <- as.data.frame(t(P63))
P101 <- as.data.frame(t(P101))
P102 <- as.data.frame(t(P102))
P108 <- as.data.frame(t(P108))
P114 <- as.data.frame(t(P114))
# Lo anterior debe correrse nuevamente después de ejecutar los sig. códigos para mantener
# el DF con una sola columna.

setDT(P15, keep.rownames = TRUE)
setDT(P16, keep.rownames = TRUE)
setDT(P17, keep.rownames = TRUE)
setDT(P29, keep.rownames = TRUE)
setDT(P62, keep.rownames = TRUE)
setDT(P63, keep.rownames = TRUE)
setDT(P101, keep.rownames = TRUE)
setDT(P102, keep.rownames = TRUE)
setDT(P108, keep.rownames = TRUE)
setDT(P114, keep.rownames = TRUE)

P15$V2 = paste(P16$V1)
P15$V3 = paste(P17$V1)
P15$V4 = paste(P17$V2)
P15$V5 = paste(P29$V1)
P15$V6 = paste(P62$V1)
P15$V7 = paste(P62$V2)
P15$V8 = paste(P63$V1)
P15$V9 = paste(P63$V2)
P15$V10 = paste(P101$V1)
P15$V11 = paste(P101$V2)
P15$V12 = paste(P102$V1)
P15$V13 = paste(P102$V2)
P15$V14 = paste(P102$V3)
P15$v15 = paste(P102$V4)
P15$V16 = paste(P102$V5)


# El resultado es un data frame con todas las columnas. El nombre de la columna
# está escrito en la primera fila "rn".

# OBTENER LOS PORCENTAJES POR PREGUNTAS
as.data.frame(table(P15$V1)) -> P15N
P151 <- P15N$Freq[1] / 1498 * 100
P152 <- P15N$Freq[2] / 1498 * 100
P153 <- P15N$Freq[3] / 1498 * 100

P151 <- as.data.frame(P151, keep.rownames = TRUE)
P152 <- as.data.frame(P152, keep.rownames = TRUE)
P153 <- as.data.frame(P153, keep.rownames = TRUE)

# setDT(P151, keep.rownames = TRUE)

P151$V2 = paste(P152$P152)
P151$V3 = paste(P153$P153)

# Asignar un nuevo nombre al objeto
PREGUNTA_15 <- P151 
#cAMBIAR EL NOMBRE DE LAS COLumnas:
names(PREGUNTA_15)[1] <- "Muchos_amigos"
names(PREGUNTA_15)[2] <- "Pocos_amigos"
names(PREGUNTA_15)[3] <- "No_tiene_amigos"

# Remover la tercera columna
df <- melt(data.table(PREGUNTA_15))
df <- df[,-3]
# Cambiar nombre a la columna 3 "No tiene amigos" y cambiar la 2 "Pocos_amigos".
# Para ello, utilizar # CAMBIAR EL NOMBRE DE LAS COLUMNAS PERO DEL "df"
names(df)[1] <- "Muchos_amigos"
names(df)[2] <- "Pocos_amigos"
names(df)[3] <- "No_tiene_amigos"

df <- as.data.frame(t(df))
names(df)[1] <- "variable"
df$nombre = NULL

# PLOT

ggplot(data = df2)+
  geom_bar(aes(x=df2, y = df2), stat = "identity")+
  xlab("Respuestas")+
  ylab("Porcentajes")



ggplot(data = df)+
  geom_bar(aes(x="Muchos amigos", y = Muchos_amigos, fill = "Muchos amigos"), stat = "identity")+
  geom_bar(aes(x="Pocos amigos", y = Pocos_amigos, fill = "Pocos amigos"), stat = "identity")+
  geom_bar(aes(x="No tiene_amigos", y = No_tiene_amigos, fill = "No tiene amigos"), stat = "identity")+
  labs(title= "Pregunta 15", subtitle = "Con respecto al tema de la mistad, usted diría que tiene...",
       x= "Respuestas",  y= "Porcentajes")+
  theme(legend.position = "none")
  
### CORRECCIONES AL PLOT
#Dos correcciones pa que quede impeque: poner los porcentajes encima de las barras 
#con dos decimales y en el eje Y poner los valores porcentuales entre ...no se... 0 y 60%

#Y dejar el mismo color de las barras
#Para que dps, cuando, las hagamos por género o por edad o por quintil
#colores diferencien los grupos. Ahora que es una única variable puede ir todo del mismo coloR
  



#Old #####

## Obtener el promedio de cada fila y cambiar los nombres del data.frame

q_mean <- rowMeans(questions, na.rm = TRUE)

# DATA FRAMES

q <- data.frame(q_mean)
setDT(q, keep.rownames = TRUE)
names(q) <- c("Pregunta", "Valor")

P15 <- data.frame(P15 = questions[1,])

# PLOTTING

ggplot()+
  geom_bar(q, mapping = aes(Pregunta, Valor), stat = "identity",
           position = "dodge", width = 0.5)

#

