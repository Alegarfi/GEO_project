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


#Unión de más de dos dataframes a partir del cmando "do.call". Se debe enlistar los dataframes, 
#asignarlo a un vector para poder utilizar el primer comando 
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

search_terms<-c("Addiction", "Alcoholism", "OCD", "Brain", "Coccaine", "Drug_abuse", "Alcohol",
                "Reward", "Substance_related_disorders", "Neuroscience", "Dopamine", 
                "Glutamate", "Reward_mechanisms", "Alcohol_dependence")
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
  
###LOOP FOR DE ISRA 
search_terms<-c("Addiction", "Alcoholism", "OCD", "Brain", "Coccaine", "Drug_abuse", "Alcohol",
               "Reward", "Substance_related_disorders", "Neuroscience", "Dopamine", 
               "Glutamate", "Reward_mechanisms", "Alcohol_dependence")
for (i in 1:length(search_terms)) { 
  #message(search_terms[i])
  TERMINO <- search_terms[i]
  message(TERMINO)
  FINAL_GEO <- Hs[0,]
  INTERMEDIATE_DATAFRAME <- Hs[0,] 
  INTERMEDIATE_DATAFRAME <-Hs[grep(TERMINO, Hs$RESUMEN_DEL_PROYECTO), ]
  finalbind<-rbind(INTERMEDIATE_DATAFRAME, FINAL_GEO)
  }


#### Líneas de loop for para ejecutarse a mano.

a<-Hs[grep("addiction", Hs$RESUMEN_DEL_PROYECTO), ]
b<-Hs[grep("Alcoholism", Hs$RESUMEN_DEL_PROYECTO), ]
c<-Hs[grep("OCD", Hs$RESUMEN_DEL_PROYECTO), ]
d<-Hs[grep("Brain", Hs$RESUMEN_DEL_PROYECTO), ]
e<-Hs[grep("Coccaine", Hs$RESUMEN_DEL_PROYECTO), ]
f<-Hs[grep("Drug_abuse", Hs$RESUMEN_DEL_PROYECTO), ]
g<-Hs[grep("Alcohol", Hs$RESUMEN_DEL_PROYECTO), ]
h<- Hs[grep("Reward", Hs$RESUMEN_DEL_PROYECTO), ]
i<- Hs[grep("Substance_related_disorders", Hs$RESUMEN_DEL_PROYECTO), ]
j<- Hs[grep("Neuroscience", Hs$RESUMEN_DEL_PROYECTO), ]
k<- Hs[grep("Dopamine", Hs$RESUMEN_DEL_PROYECTO), ]
l<- Hs[grep("Glutamate", Hs$RESUMEN_DEL_PROYECTO), ]
m<- Hs[grep("Reward_mechanisms", Hs$RESUMEN_DEL_PROYECTO), ]
n<- Hs[grep("Alcohol_dependence", Hs$RESUMEN_DEL_PROYECTO), ]

dflist<- list(a,b,c,d,e,f,g,h,i,j,k,l,m,n)
final.geo<- do.call("rbind", dflist)
  #El resultado es el esperado. El siguiente paso es definir mejor la tabla y los filtros.