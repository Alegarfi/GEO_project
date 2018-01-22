setwd("Documentos/gits/GEO_project/R-Scripts/")
GEO<-read.csv(file="GEO_tablet.tsv", header = T, sep = "\t", na.strings = "", stringsAsFactors = F)
Hs<- GEO[grep("Homo_sapiens", GEO$ORGANISMO, value = T), ]


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
rm(a,b,c,d,e,f,g,h,i,j,k,l,m,n, semifinal, dflist)

#Procesamiento de dataframe final para las primeras gráficas con respecto al tipo de estudio
study_type<- table(final.geo$SERIES_TYPE)
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
  bp + geom_text( check_overlap = F)
  
  ##RECORDAR MODIFICAR EL CÓDIGO PARA MEJOR VISUALIZACIÓN

##piechart
  pie <- bp + coord_polar("y", start=0)
  pie
  pie + scale_fill_brewer(palette="Dark2") + geom_text(aes(y = frecs), label = porcentaje)
  
  pie + scale_fill_brewer(palette="Dark2") +   geom_text(aes(y = Freq/7 + 
                                                               c(0, cumsum(Freq)[-length(Freq)]), label = porcentaje), size=5)
  
  