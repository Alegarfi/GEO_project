##Scripts para loops for

#loop for 1, intento de greps con múltiples términos.

vocablos <- c("addiction", "Alcoholism", "OCD", "Brain", "coccaine", "drug_abuse", "alcohol", 
                           "reward", "substance_related_disorders", "neuroscience", "dopamine", "glutamate",
                           "reward_mechanisms", "alcohol_dependence", "DDR")
for (i in 1:nrow(Hs)){
  message(paste0("operando sobre la línea", i))
  result0 <- Hs[0,]
  result1<-data.frame(grepl(vocablos, i, fixed = T))
}
###loop for que busca las palabras en la tabla de manera inversa a la utilizada
for (i in vocablos){
  message(paste0("Operando sobre la línea ", i))
  r3<-Hs[grep(i, Hs$RESUMEN_DEL_PROYECTO), ]
  r3<-r3[grep("cancer", r3$RESUMEN_DEL_PROYECTO, invert = T), ]
}


#Comparación de resultados 

A<- final.geo$GEO_ID
B<- r3$GEO_ID
n<-max(length(A), length(B))
length(A)<- n
length(B)<- n
AvsB<-cbind(A,B)
View(AvsB)
