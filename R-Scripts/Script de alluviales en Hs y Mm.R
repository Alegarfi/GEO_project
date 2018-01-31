###Script para alluviales 
setwd("~/Documentos/gits/GEO_project/R-Scripts/")
GEO<-read.csv(file = "GEO_tablet.tsv", header = T, sep = "\t", na.strings = "", stringsAsFactors = F)
Hs <- GEO[grep("Homo_sapiens", GEO$ORGANISMO), ]
Mm <- GEO [grep("Mus_musculus", GEO$ORGANISMO), ]
homo_musculus<- rbind(Hs, Mm)
rm(GEO)

for(i in 1:nrow(homo_musculus)){
  FINAL_DF <-homo_musculus[0, ]
  INTERMEDIATE_DF <- homo_musculus[0, ]
  message()
}