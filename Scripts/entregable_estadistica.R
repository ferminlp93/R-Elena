#Importamos el fichero de jaen
Datos.Jaen <- read.table('./Datos/JaenIndicadores.txt',sep="\t", header=TRUE, encoding  ="UTF-8")
municipio <- Datos.Jaen[,c("municipio")]
municipio <-iconv(municipio,to="ASCII//TRANSLIT", sub=NA)
municipio
