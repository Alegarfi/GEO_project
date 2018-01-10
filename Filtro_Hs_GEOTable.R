setwd("~/Descargas/GEO_tables/") #Ubicación de la tabla
#Cargando la tabla y especificando los detalles de ésta, como el uso de "NA"s 
#para los espacios en blanco. La tabla contiene 52996 líneas en total
GEO<-read.csv(file="GEO_tablet.tsv", header = T, sep = "\t", na.strings = "", stringsAsFactors = F)
#Elaboración del subsets para sólo tener los estudios realizados en Humano (Homo sapiens).
#Líneas en total para este subset: 21905
Hs<- GEO[grepl("Homo_sapiens", GEO$ORGANISMO), ]
View(Hs)
search_terms <- "drugs|addiction|OCD" #Términos de interés en forma de vector para usarlos con grep
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
      INTERMEDIATE_DATAFRAME[e,] <- Hs[i, ] 
      INTERMEDIATE_DATAFRAME[e,"RESUMEN_DEL_PROYECTO"] <- number_of_matches[e]
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
try2<-as.data.frame(Hs[grep("addiction", Hs$RESUMEN_DEL_PROYECTO), ])
try2<-Hs[grep("addiction", Hs$RESUMEN_DEL_PROYECTO), ]
#El resultado arroja lo esperado. Aparentemente son todos los estudios relacionados con la palabra 
#"Addiction" a partir del subset Homo sapiens.