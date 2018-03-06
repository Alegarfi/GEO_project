
setwd("Documentos/gits/GEO_project/R-Scripts/")
GEO<-read.csv(file="GEO_tablet.tsv", header = T, sep = "\t", na.strings = "", stringsAsFactors = F)
Hs<- GEO[grep("Homo_sapiens", GEO$ORGANISMO), ]


#Búsqueda de términos para generar una abla final
a<-Hs[grep("addiction", Hs$RESUMEN_DEL_PROYECTO), ]
b<-Hs[grep("Alcoholism", Hs$RESUMEN_DEL_PROYECTO), ]
c<-Hs[grep("OCD", Hs$RESUMEN_DEL_PROYECTO), ]
d<-Hs[grep("Brain", Hs$RESUMEN_DEL_PROYECTO), ]
e<-Hs[grep("Coccaine", Hs$RESUMEN_DEL_PROYECTO), ]
f<-Hs[grep("Drug_abuse", Hs$RESUMEN_DEL_PROYECTO), ]
g<-Hs[grep("Alcohol", Hs$RESUMEN_DEL_PROYECTO), ]
h<-Hs[grep("Reward", Hs$RESUMEN_DEL_PROYECTO), ]
i<-Hs[grep("Substance_related_disorders", Hs$RESUMEN_DEL_PROYECTO), ]
j<-Hs[grep("Neuroscience", Hs$RESUMEN_DEL_PROYECTO), ]
k<-Hs[grep("Dopamine", Hs$RESUMEN_DEL_PROYECTO), ]
l<-Hs[grep("Glutamate", Hs$RESUMEN_DEL_PROYECTO), ]
m<-Hs[grep("Reward_mechanisms", Hs$RESUMEN_DEL_PROYECTO), ]
n<-Hs[grep("Alcohol_dependence", Hs$RESUMEN_DEL_PROYECTO), ]
dflist<- list(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
semifinal <- do.call("rbind", dflist)
final.geo<-semifinal[!grepl("cancer", semifinal$RESUMEN_DEL_PROYECTO), ]

rm(a,b,c,d,e,f,g,h,i,j,k,l,m,n, semifinal, dflist, GEO)

#Procesamiento de dataframe final para las primeras gráficas con respecto al tipo de estudio

var<- data.frame(final.geo$ORGANISMO, final.geo$SERIES_TYPE)
table(var)

FINAL.GEO <- GEO[0,]

for(i in 1:nrow(Hs)){
  
  INTERMEDIATE.DATAFRAME <- GEO[0,]
  
  message(paste0("Operando sobre la linea ", i))
  CELDA <- Hs$ORGANISMO[i]
  ESPECIES <- unlist(strsplit(CELDA, split = ","))
  #AQUI SE DEBE DE OPERAR SOBRE LA VARIABLE ESPECIES PARA ELIMINAR LOS 
  #ELEMENTOS QUE COMIENZAN CON "!Sample.." Y DESPUES CONTAR EL NUMERO DE ESPECIES
  ESPECIES <- ESPECIES[!grepl('!Sample', ESPECIES)] 
  ESPECIES <- ESPECIES [!grepl('1:9', ESPECIES)]
  NUMERO_DE_ESPECIES <- length(ESPECIES)
  
  for (x in 1:NUMERO_DE_ESPECIES) {
    INTERMEDIATE.DATAFRAME[x,] <- GEO[i,] 
    INTERMEDIATE.DATAFRAME[x,"ORGANISMO"] <- ESPECIES[x]
  }
  
  FINAL.GEO <- rbind(FINAL.GEO, INTERMEDIATE.DATAFRAME)  
}

study_type<- table(Hs$SERIES_TYPE)
study_type<- data.frame(study_type, stringsAsFactors = F)
names(study_type)[1] <-"series_type"
otrosdf<- data.frame(series_type = "Otros", Freq = sum(study_type == 1))
study_type <- data.frame(study_type[study_type$Freq >= 2, ])
study_type <- rbind(study_type, otrosdf)
study_type <- study_type[order(study_type$Freq, decreasing = F), ]
labls <- study_type$series_type
study_type<-study_type[order(study_type$Freq, decreasing = T), ]

#Inicio de scripts para generación de gráficas con ggplot2 más paquetería necesaria para más procesamiento
library(ggplot2)
library(scales)
library(plyr)

Tipo_de_experimento <- as.vector(study_type$series_type)
Freq<- study_type$Freq
series_type <- as.vector(study_type$series_type)
study_type <- data.frame(series_type, Freq)
porcentaje <- percent(Freq/sum(Freq))

##barplot sencillo 

bp <- ggplot(study_type, aes(x="", y=study_type$Freq, fill= study_type$series_type)) + 
  geom_bar(stat = "identity", width = 0.3) + scale_fill_brewer(palette = "Dark2") 
  bp + geom_text(aes(label=porcentaje, check_overlap = F))
  
  ##RECORDAR MODIFICAR EL CÓDIGO PARA MEJOR VISUALIZACIÓN

##piechart
  pie <- bp + coord_polar("y", start=0)
  pie
  pie + scale_fill_brewer(palette="Dark2") + geom_text(aes(y = frecs), label = porcentaje)
  
  pie + scale_fill_brewer(palette="Dark2") +   geom_text(aes(y = Freq/7 + 
                                                               c(0, cumsum(Freq)[-length(Freq)]), label = porcentaje), size=5)
  
  
  #######Inicio de aluvial pot, ejemplos del paquete:
  library(alluvial)
  library(plyr)
  library(dplyr)
  # Titanic data
  tit <- as.data.frame(Titanic)
  alluvial(tit)
  
  
# Alluvial con datos de GEO
  var<- data.frame(final.geo$ORGANISMO, final.geo$SERIES_TYPE, stringsAsFactors = F)
  var <-as.data.frame(table(var), stringsAsFactors = F)
    names(var)[1] <-"ORGANISMO"
    names(var)[2] <-"SERIES_TYPE"  
    names(var)[3] <-"FREQ"
  var[0,]
  var %>% group_by(var$ORGANISMO, var$SERIES_TYPE) %>%
    summarise(n = sum(var$FREQ)) -> tit2d
  
alluvial(tit2d[,1:2], freq=tit2d$n)
  ##ESTE ES EL PRIMER ALLUVIAL CON DOS VARIABLES CATEGÓRICAS, EL PASO SIGUIENTE ES LIMPIAR LA INFORMACIÓN
  ##PARA QUE LAS FRECUENCIAS CORRESPONDAN A CADA UNA DE LAS VARIABLES OBSERVADAS EN LA TABLA Y QUE, 
  ##POSTERIORMENTE, SEAN FÁCILMENTE INTERPRETADAS EN LA GRÁFICA.

  #GENERACIÓN DE UN LOOP FOR PARA DIVIDIR LAS ESPECIES EN LA COLUMNA CORREPONDIENTE.
FINAL.VAR <- var[0,]
for(i in 1:nrow(var)){
  INTERMEDIATE.DATAFRAME <- var[0,]
  message(paste0("operando sobre la línea ", i))
  CELDA <-var$ORGANISMO[i]
  ESPECIES <-unlist(strsplit(CELDA, split = ","))
  ESPECIES <- ESPECIES[!grepl('!Sample', ESPECIES)] 
  ESPECIES <- ESPECIES [!grepl('1:9', ESPECIES)]
  NUMERO_DE_ESPECIES <- length(ESPECIES)
  
  for (x in 1:NUMERO_DE_ESPECIES) {
    INTERMEDIATE.DATAFRAME[x,] <- var[i,] 
    INTERMEDIATE.DATAFRAME[x,"ORGANISMO"] <- ESPECIES[x]
  }
  
  FINAL.VAR <- rbind(FINAL.VAR, INTERMEDIATE.DATAFRAME)
  
}
rm(INTERMEDIATE.DATAFRAME)

##PROCESAMIENTO DE LA TABLA "FINAL.VAR"
View(table(FINAL.VAR))
  
FINAL.VAR<- as.data.frame(FINAL.VAR)
VAR2<-data.frame(FINAL.VAR$ORGANISMO, FINAL.VAR$SERIES_TYPE)
VAR2 <-as.data.frame(table(VAR2), stringsAsFactors = F)
names(VAR2)[1] <-"ORGANISMO"
names(VAR2)[2] <-"SERIES_TYPE"  
names(VAR2)[3] <-"FREQ"


##INTENTO 2 DE ALLUVIAL CON DATOS REALES DE GEO
library("ggplot2")

VAR2 %>% group_by(VAR2$ORGANISMO, VAR2$SERIES_TYPE) %>%
  summarise(n = sum(VAR2$FREQ)) -> tit2d

alluvial(tit2d[,1:2], freq=VAR2$FREQ, col = "steelblue")
write.csv(final.geo, file = "greps", "~/Documentos/gits/GEO_project/R-Scripts/", sep = "\t")


#TERCER ALLUVIAL
VAR2<-VAR2[order(VAR2$ORGANISMO),]
View(VAR2)
VAR3 <- VAR2[grep("Homo_sapiens", VAR2$ORGANISMO), ]
View(VAR3)

VAR3 %>% group_by(VAR3$ORGANISMO, VAR3$SERIES_TYPE) %>%
  summarise(n = sum(VAR3$FREQ)) -> VAR3D

alluvial(VAR3D[,1:2], freq=VAR3D$n, col = "steelblue")

###Arreglado de los datos para el alluvial
var<- data.frame(final.geo$ORGANISMO, final.geo$SERIES_TYPE)
vst <- data.frame(table(var$final.geo.SERIES_TYPE))
vst2<-vst[vst$Freq>=2,]
human<- data.frame(rep("Homo_sapiens", times=15))
VAR4<-bind_cols(human, vst)
names(VAR4)[1] <-"ORGANISMO"
names(VAR4)[2] <-"SERIES_TYPE"  
names(VAR4)[3] <-"FREQ"
VAR4<-data.frame(VAR4[order(VAR4$FREQ, decreasing = T), ], stringsAsFactors = F)
View(VAR4)

VAR4 %>% group_by(VAR4$ORGANISMO, VAR4$SERIES_TYPE) %>%
  summarise(n = sum(VAR4$FREQ)) -> VAR4D

alluvial(VAR4D[,1:2], freq=VAR4$FREQ, col = "steelblue", border = "white")
