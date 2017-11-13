
#Importamos el fichero de jaen
#Código INE del municipio.
#Nombre del municipio.
#Consumo de energía eléctrica en megavatios por hora.
#Consumo medio de agua en invierno, en metros cúbicos por día.
#Consumo medio de agua en verano, en metros cúbicos por día.
#Destino de los residuos sólidos urbanos: las posibilidades son vertedero controlado, vertedero incontrolado, compostaje.
#Cantidad de residuos sólidos urbanos, en toneladas.

#1. Importar el fichero JaenIndicadores.txt y denominar a la hoja de datos (data frame) Datos.Jaen
Datos.Jaen <- read.table('./Datos/JaenIndicadores-utf8.txt',sep="\t", header=TRUE, encoding  ="UTF-8")


#2. Recodificar la variable Poblacion en una variable cualitativa tipo factor llamada Tamaño con tres categorías:
#  Si la población es inferior a 2000 habitantes, Tamaño será “Pequeño”.
#  Si la población está entre 2000 y 4500 habitantes, Tamaño será “Mediano”.
#  Si la población es superior a 4500 habitantes, Tamaño será “Grande”.

Datos.Jaen$poblacioncat[(as.numeric(as.character(Datos.Jaen$poblacion)))<= 2000] <- "Pequeño"

Datos.Jaen$poblacioncat[(as.numeric(as.character(Datos.Jaen$poblacion)))> 2000 & (as.numeric(as.character(Datos.Jaen$poblacion)))< 4500] <- "Mediano"

Datos.Jaen$poblacioncat[(as.numeric(as.character(Datos.Jaen$poblacion)))>= 4500] <- "Grande"

Datos.Jaen$poblacioncat <- as.factor(Datos.Jaen$poblacioncat)

Datos.Jaen$poblacion <- Datos.Jaen$poblacioncat 

Datos.Jaen<-Datos.Jaen[1:8]

