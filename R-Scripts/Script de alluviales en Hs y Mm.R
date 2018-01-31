###Script para alluviales 
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
  CELDA<-homo_musculus$ORGANISMO[33]
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
