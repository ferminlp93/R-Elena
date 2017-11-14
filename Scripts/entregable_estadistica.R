#####################1. Cálculo de nuevas variables, recodificación y filtrado#####################

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
#Limpiar datos faltantes
Datos.Jaen <- Datos.Jaen[!Datos.Jaen$poblacion == "..", ]
options(warn=-1)
#Cambiar comas por puntos para poder calcular más tarde
Datos.Jaen$residuoscant <- gsub("\\,", ".", Datos.Jaen$residuoscant)
#Cambiar a numerico o doble las variables para los calculos
Datos.Jaen$poblacion <- (as.numeric(as.character(Datos.Jaen$poblacion)))
Datos.Jaen$consumoelec <- (as.numeric(as.character(Datos.Jaen$consumoelec)))
Datos.Jaen$consumoaguainv <- (as.numeric(as.character(Datos.Jaen$consumoaguainv)))
Datos.Jaen$consumoaguaver <- (as.numeric(as.character(Datos.Jaen$consumoaguaver)))
Datos.Jaen$residuoscant <- (as.double(Datos.Jaen$residuoscant))

#2. Recodificar la variable Poblacion en una variable cualitativa tipo factor llamada Tamaño con tres categorías:
#  Si la población es inferior a 2000 habitantes, Tamaño será “Pequeño”.
#  Si la población está entre 2000 y 4500 habitantes, Tamaño será “Mediano”.
#  Si la población es superior a 4500 habitantes, Tamaño será “Grande”.


Datos.Jaen$poblacioncat[Datos.Jaen$poblacion<= 2000] <- "Pequeño"

Datos.Jaen$poblacioncat[Datos.Jaen$poblacion> 2000 & (as.numeric(as.character(Datos.Jaen$poblacion)))< 4500] <- "Mediano"

Datos.Jaen$poblacioncat[Datos.Jaen$poblacion>= 4500] <- "Grande"

Datos.Jaen$poblacioncat <- as.factor(Datos.Jaen$poblacioncat)

#Datos.Jaen$poblacion <- Datos.Jaen$poblacioncat 

#Datos.Jaen<-Datos.Jaen[1:8]

options(warn=0)
#3. Calcular los siguientes promedios que se especifican a continuación y añadirlos como nuevas variables al fichero Datos.Jaen obtenidas a partir de las variables existentes: 
#  Variable elec.hab que contendrá el consumo de energía eléctrica por habitante, obtenida como Consumo.de.energia.electrica/Poblacion
#  Variable agua.hab que contendrá el consumo medio de agua por habitante y día, obtenida como (Consumo.de.agua..Invierno + Consumo.de.agua..V erano)/Poblacion
#  Variable res.hab que contendrá los residuos sólidos urbanos por habitante, obtenida como Residuos.solidos.urbanos..Cantidad/Poblacion
Datos.Jaen$elec.hab <- Datos.Jaen$consumoelec/Datos.Jaen$poblacion
Datos.Jaen$agua.hab <- (Datos.Jaen$consumoaguainv+Datos.Jaen$consumoaguaver)/Datos.Jaen$poblacion
Datos.Jaen$res.hab <- Datos.Jaen$residuoscant/Datos.Jaen$poblacion

#4. Crear una nueva hoja de datos con todas las variables que contiene actualmente el data frame Datos.Jaen, pero referida 
#sólo a los municipios de tamaño mediano y denominarla Datos.Jaen.Medianos

Datos.Jaen.Medianos <- Datos.Jaen[Datos.Jaen$poblacioncat == 'Mediano',]

#5. Guardar la hoja de datos Datos.Jaen con las nuevas variables creadas en los apartados anteriores y la hoja que contiene 
#los datos de las poblaciones medianas (Datos.Jaen.Medianos) en un archivo de datos de R y llamadlo JaenIndicadores.RData

save(Datos.Jaen, Datos.Jaen.Medianos, file = "./Datos/JaenIndicadores.RData")

