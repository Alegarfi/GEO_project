setwd("~/Descargas/GEO_tables/") #Ubicación de la tabla
#Cargando la tabla y especificando los detalles de ésta, como el uso de "NA"s 
#para los espacios en blanco. La tabla contiene 52996 líneas en total
GEO<-read.csv(file="GEO_tablet.tsv", header = T, sep = "\t", na.strings = "", stringsAsFactors = F)
#Elaboración del subsets para sólo tener los estudios realizados en Humano (Homo sapiens).
#Líneas en total para este subset: 21905
Hs<- GEO[grepl("Homo_sapiens", GEO$ORGANISMO), ]
#View(Hs)
#search_terms <- "drugs|addiction|OCD" #Términos de interés en forma de vector para usarlos con grep
#Data frames vacíos para usarlos en el loop for
INTERMEDIATE_DATAFRAME <- Hs[0,] 
FINAL_GEO <- Hs[0,]

#El siguiente loop for buscará las palabras/términos de interés en el subset de Homo sapiens en una
#columna específica (RESUMEN_DEL_PROYECTO), pues se infiere, que en esta columna en específico, 
#contiene las información que encaja con la búsqueda

for (i in 1:nrow(Hs)) { 
  INTERMEDIATE_DATAFRAME <- Hs[0,]
  message(paste0("Operando sobre la linea ", i))
  Description <- Hs$RESUMEN_DEL_PROYECTO[i]
  matches<- grep("Addiction", Description, value = T, fixed = T)
  number_of_matches <- length(matches)
  #Este segundo loop for es para fusionar la tabla que se modifica con cada ciclo con la versión final
  for (e in 1:number_of_matches){
      INTERMEDIATE_DATAFRAME[e,] <- number_of_matches
      INTERMEDIATE_DATAFRAME[e,"RESUMEN_DEL_PROYECTO"] <- number_of_matches
      
    }
  FINAL_GEO <- rbind(FINAL_GEO, INTERMEDIATE_DATAFRAME)
  }
#Al finalizar el loop, genera el mismo número de líneas de la tabla original. Esto indica que el 
#grep n funciona, así que se recurre a otras aproximaciones

##Prueba con un grep simple
try <- grep("addiction", Hs$RESUMEN_DEL_PROYECTO, value = T, fixed = T)
View(as.data.frame(try))
#El resultado arroja sólo líneas de una columna. No podemos saber el nombre del proyecto con el que 
#haya tenido match

#Prueba con grep en data frame
Only_addiction<-Hs[grep("addiction", Hs$RESUMEN_DEL_PROYECTO), ]
#El resultado arroja lo esperado. Aparentemente son todos los estudios relacionados con la palabra 
#"Addiction" a partir del subset Homo sapiens.
Only_alcoholism <-Hs[grep("Alcoholism", Hs$RESUMEN_DEL_PROYECTO), ]

#Intento con más términos. En esta ocasión, la búsqueda va con el trastorno obsesivo-compulsivo (OCD)
Only_OCD<-Hs[grep("OCD", Hs$RESUMEN_DEL_PROYECTO), ]

#Prueba con dos palabras simultáneas. El resultado debería ser la suma de los resultados anteriores
Addiction_and_OCD <- rbind(Only_addiction, Only_OCD)

dflist<- list(Only_addiction, Only_alcoholism, Only_OCD)
View(dflist)
final.geo<- do.call("rbind", dflist)

#Prueba con la búsqueda de dos palabras en forma de vector
tdi<- c("Addiction", "OCD")
    #No funciona

#El grep en R funciona con un término por cada búsqueda. La solción a esto es generar un loop for
#de nuevo para que haga dos búsquedas (una por cada palabra), ésta será una solución temporal en  
#espera de llegar a una solución más eficaz

for (i in 1:nrow(Hs)) { 
  message(paste0("Operando sobre la linea ", i))
  #Description <- Hs$RESUMEN_DEL_PROYECTO[i]
  OCD<- Hs[grep("OCD", Hs$RESUMEN_DEL_PROYECTO), ]
  Addiction<- Hs[grep("Addiction", Hs$RESUMEN_DEL_PROYECTO), ]
  number_of_matches <- length(matches)
#Este segundo loop for es para fusionar la tabla que se modifica con cada ciclo con la versión final
  FINAL_GEO <- rbind(OCD, Addiction)
}


###MEJORANDO EL CÓDIGO
for (i in 1:nrow(Hs)) {
  a<-Hs[grep("addiction", Hs$RESUMEN_DEL_PROYECTO), ]
  b <-Hs[grep("Alcoholism", Hs$RESUMEN_DEL_PROYECTO), ]
  c<-Hs[grep("OCD", Hs$RESUMEN_DEL_PROYECTO), ]
  d <-Hs[grep("Brain", Hs$RESUMEN_DEL_PROYECTO), ]
  e <- Hs[grep("Coccaine", Hs$RESUMEN_DEL_PROYECTO), ]
  f <- Hs[grep("Drug_abuse", Hs$RESUMEN_DEL_PROYECTO), ]
  g <- Hs[grep("Alcohol", Hs$RESUMEN_DEL_PROYECTO), ]
  h<- Hs[grep("Reward", Hs$RESUMEN_DEL_PROYECTO), ]
  i<- Hs[grep("Substance_related_disorders", Hs$RESUMEN_DEL_PROYECTO), ]
  j<- Hs[grep("Neuroscience", Hs$RESUMEN_DEL_PROYECTO), ]
  k<- Hs[grep("Dopamine", Hs$RESUMEN_DEL_PROYECTO), ]
  l<- Hs[grep("Glutamate", Hs$RESUMEN_DEL_PROYECTO), ]
  m<- Hs[grep("Reward_mechanisms", Hs$RESUMEN_DEL_PROYECTO), ]
  n<- Hs[grep("Alcohol_dependence", Hs$RESUMEN_DEL_PROYECTO), ]
  
  dflist<- list(letters[1:15])
  final.geo<- do.call("rbind", dflist)
}

##### INTENTO 2 CON ggplot2
  
bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp

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

Terms <- c("DDR2", "OPIOIDS")
dflist<- list(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
semifinal <- do.call("rbind", dflist)
final.geo<-semifinal[!grepl("cancer", semifinal$RESUMEN_DEL_PROYECTO), ]
rm(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
rm(semifinal)
rm(Terms)

study_type<- table(final.geo$SERIES_TYPE)
study_type<- data.frame(study_type, stringsAsFactors = T)
names(study_type)[1] <-"series_type"
otrosdf<- data.frame(series_type = "Otros", Freq = sum(study_type == 1))
study_type <- data.frame(study_type[study_type$Freq >= 2, ])
study_type <- rbind(study_type, otrosdf)
study_type <- study_type[order(study_type$Freq, decreasing = T), ]
labls <- study_type$series_type
study_type<-study_type[order(study_type$Freq, decreasing = T), ]

library(ggplot2)
library(scales)
library(plyr)
##Gráfica de los estudios realizados en barras  
porcentaje<-percent(prop.table(study_type$Freq))
Freq<- study_type$Freq

porcentaje<-data.frame(porcentaje)
st_graficable<-cbind(study_type, porcentaje)
st_graficable = ddply(st_graficable, .(Freq), transform, position = cumsum(Freq) - 0.5*Freq)

# Format the labels and calculate their positions
st_graficable = ddply(st_graficable, .(series_type), transform, pos = (cumsum(Freq) - 0.5 * Freq))
st_graficable$label = paste0(sprintf("%.0f", st_graficable$porcentaje), "%")

ggplot(st_graficable, aes(x = "", y = st_graficable$Freq, 
                          fill = st_graficable$series_type)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(y = pos, label = porcentaje), size = 2)

##barplot sencillo 
study_type<-study_type[order(study_type$Freq, decreasing = T), ]
Tipo_de_experimento<-as.factor(study_type$series_type)
Freq<- study_type$Freq
bp <- ggplot(study_type, aes(x="Estudios", y=as.vector(Freq), fill= Tipo_de_experimento)) + 
  geom_bar(stat = "identity", width = 0.3) + scale_fill_brewer(palette = "Dark2") 
bp + geom_text(aes(label = as.vector(porcentaje)), check_overlap = F)

position <- Freq*0.5
study_type <- ddply(study_type, .(Freq), transform, position = Freq*1)
##Gráficas en pastel
pie <- bp + coord_polar("y", start=0)
pie
pie + scale_fill_brewer(palette="Dark2") + geom_text(aes(y = frecs), label = porcentaje)

pie + scale_fill_brewer(palette="Dark2") +   geom_text(aes(y = Freq/7 + 
c(0, cumsum(Freq)[-length(Freq)]), label = porcentaje), size=5)

