###Script para alluviales 
library(alluvial)
library(plyr)
library(dplyr)
setwd("~/Documentos/gits/GEO_project/R-Scripts/")
GEO<-read.csv(file = "GEO_tablet.tsv", header = T, sep = "\t", na.strings = "", stringsAsFactors = F)
Hs <- GEO[grep("Homo_sapiens", GEO$ORGANISMO), ]
Mm <- GEO [grep("Mus_musculus", GEO$ORGANISMO), ]
homo_musculus<- rbind(Hs, Mm)
rm(GEO)


###LOOP FOR SEPARAR LAS ESPECIES AGLOMERADAS EN UNA MISMA CELDA
FINAL_DF <-homo_musculus[0, ]
INTERMEDIATE_DF <- homo_musculus[0, ]
for(i in 1:nrow(homo_musculus)){
  
  message(paste0("Procesando la línea ", i))
  CELDA<-homo_musculus$ORGANISMO[i]
  ESPECIES<-unlist(strsplit(CELDA, split = ","))
  ESPECIES <- ESPECIES[!grepl('!Sample', ESPECIES)] 
  ESPECIES <- ESPECIES [!grepl('1:9', ESPECIES)]
  NUMERO_DE_ESPECIES <- length(ESPECIES)
  for(x in 1:NUMERO_DE_ESPECIES){
    INTERMEDIATE_DF[x,] <-homo_musculus[i,]
    INTERMEDIATE_DF[x,"ORGANISMO"] <- ESPECIES[x]
  }
  FINAL_DF<-rbind(FINAL_DF, INTERMEDIATE_DF)
}

rm(INTERMEDIATE_DF)

#GENERACIÓN DE SUBSETS PARA ALLUVIALES
#SUBSET CON COLUMNAS POR SEPARADO
SP<-FINAL_DF$ORGANISMO
ST<-FINAL_DF$SERIES_TYPE
FQ<-data.frame(table(ST))
View(FQ)

#SUBSET CON COLUMNAS UNIDAS
SPETST<- data.frame(FINAL_DF$ORGANISMO, FINAL_DF$SERIES_TYPE)
View(SPETST)
SPETST<- data.frame(table(SPETST))
SPETST<-data.frame(SPETST[order(SPETST$Freq, decreasing = T), ], stringsAsFactors = F)

SPETST %>% group_by(SPETST$FINAL_DF.ORGANISMO, SPETST$FINAL_DF.SERIES_TYPE) %>%
  summarise(n = sum(SPETST$Freq)) -> VARD

alluvial(VARD[,1:2], freq=VARD$n, col = "steelblue", border = "white")


##INTENTO CON DATOS SEPARADOS
#Se ha de realizar un loop for que nos permita separar las especies de una misma línea para cada subset
#de esta manera, se ha de procesar la información independientemente

#Mus musculus primero
FINAL_DF <- Mm[0, ]
INTERMEDIATE_DF <- Mm[0, ]
for(i in 1:nrow(Mm)){
  
  message(paste0("Procesando la línea ", i))
  CELDA<-Mm$ORGANISMO[i]
  ESPECIES<-unlist(strsplit(CELDA, split = ","))
  ESPECIES <- ESPECIES[!grepl('!Sample', ESPECIES)] 
  ESPECIES <- ESPECIES [!grepl('1:9', ESPECIES)]
  NUMERO_DE_ESPECIES <- length(ESPECIES)
  for(x in 1:NUMERO_DE_ESPECIES){
    INTERMEDIATE_DF[x,] <-homo_musculus[i,]
    INTERMEDIATE_DF[x,"ORGANISMO"] <- ESPECIES[x]
  }
  FINAL_DF<-rbind(FINAL_DF, INTERMEDIATE_DF)
}

rm(INTERMEDIATE_DF)


