setwd("~/Descargas/GEO_tables/")
GEO<-read.csv(file="GEO_tablet.tsv", header = T, sep = "\t", na.strings = "", stringsAsFactors = F) 
top <- c('FILE_PATH', 'GEO_ID', 'NOMBRE_DE_PROYECTO', 'RESUMEN_DEL_PROYECTO', 'PMID', 
         'FECHA_DE_PUBLICACION', 'ORGANISMO', 'BIOPROJECT_ID', 'SRA_ID', 'SERIES_TYPE', 
        'SAMPLE_TYPE', 'SAMPLE_DESCRIPTION')
names(GEO) <- top
rm(top)
bioproject <-subset.data.frame(GEO, !is.na(GEO$BIOPROJECT_ID))
bicho<-as.vector(bioproject$ORGANISMO)
##unlist(strsplit(bicho, split=","))[1] esto para separar términos por comas o cualquier otro
##delimitador

A<-grep("!", bicho, invert = T, value = T)
#A<-grep("1:9", A2, invert = T, value = T)
B<- table(A)
C<- as.data.frame(B)
names(C)[1] = 'Organismo'
Freq<-sum(C$Freq==1)
D<- subset.data.frame(C, C$Freq >= 2)
nr<- data.frame("Otros", Freq)
names(nr)[1]="Organismo"
names(nr)[2]="Freq"
D<-rbind(D, nr)
rm(otros)
sum(D$Freq)
D<-D[order(D$Freq, decreasing = T), c(1,2)]
vectorD <- as.matrix(D$Freq)
VectorC<-as.matrix(C$Freq)
Spp<-D$Organismo

######Barplot2

TFG<-table(FINAL.GEO$ORGANISMO)
TFG<-as.data.frame(TFG)
Spp<-as.data.frame(table(FINAL.GEO$ORGANISMO))
Spp<- Spp$Var1
"Gráfica de especies"<-barplot(height = as.matrix(TFG$Freq), beside = F, space = c(1,5),
                               legend.text = Spp, args.legend = ,
                               ylab = "Frecuencia", col = 0:40, width = 1, 
                               main = "Frecuencia de organismos en GEOdb", axes = T)

### Subsets con Homosapiens
Hs<- GEO[grepl("Homo_sapiens", GEO$ORGANISMO), ]
View(Hs)


###Subset para graficar disponibilidad de SRAs
#SRAvector<-as.vector(strsplit(SRAvector, split= ","))
SRAvector<-as.vector(GEO$SRA_ID)
SRAvector<-table(SRAvector, useNA = "ifany")
SRAdf<-as.data.frame(SRAvector)
View(SRAdf)
sum(SRAdf$Freq==1)
c1<- c("Con SRA", "Sin SRA")
c2<- c(43, 158)
SRAdf<-data.frame(c1,c2)
matrix(SRAdf$c2)
"Gráfica SRA"<-barplot(height = matrix(SRAdf$c2), beside = F, space = c(1,5), legend.text = matrix(SRAdf$c1),
        ylab = "Frecuencia", col = 0:1, width = 1, main = "Frecuencia de estudios con SRA",
        axes = T)

###GREP sobre los términos de inerés
palabras_de_interés<-c("brain", "addiction")
grep1<-GEO[grep("Homo_sapiens", GEO$RESUMEN_DEL_PROYECTO), ]
View(grep1)

####GENERATE an empty dataframe for final results

FINAL.GEO <- GEO[0,]

for(i in 1:nrow(GEO)){

  INTERMEDIATE.DATAFRAME <- GEO[0,]

  message(paste0("Operando sobre la linea ", i))
  CELDA <- GEO$ORGANISMO[i]
  ESPECIES <- unlist(strsplit(CELDA, split = ","))
  #AQUI SE DEBE DE OPERAR SOBRE LA VARIABLE ESPECIES PARA ELIMINAR LOS 
  #ELEMENTOS QUE COMIENZAN CON "!Sample.." Y DESPUES CONTAR EL NUMERO DE ESPECIES
  ESPECIES <- ESPECIES[!grepl('!Sample', ESPECIES)] 
  NUMERO_DE_ESPECIES <- length(ESPECIES)

  for (x in 1:NUMERO_DE_ESPECIES) {
  INTERMEDIATE.DATAFRAME[x,] <- GEO[i,] 
  INTERMEDIATE.DATAFRAME[x,"ORGANISMO"] <- ESPECIES[x]
  }

  FINAL.GEO <- rbind(FINAL.GEO, INTERMEDIATE.DATAFRAME)  
}
rm(INTERMEDIATE.DATAFRAME)
######Barplot3
TFG<-table(FINAL.GEO$ORGANISMO)
TFG <- as.data.frame(TFG)
names(TFG)[1] <- "Especies"
Otros <- sum(TFG == 1)
Otrosdf <- data.frame(Especies ="Otros", Freq = Otros)
TFG<-TFG[TFG$Freq >= 2, ]
TFG<-rbind(TFG, Otrosdf)
names(TFG)[1] <- "Especies"
Spp_abundantes <- as.vector(TFG$Especies)
"Gráfica de especies"<-barplot(height = as.matrix(TFG$Freq), beside = F, space = c(1,5),
                               legend.text = Spp_abundantes, args.legend = ,ylab = "Frecuencia", 
                               col = 0:40, width = 10, main = "Frecuencia de organismos en GEOdb", 
                               axes = T)
