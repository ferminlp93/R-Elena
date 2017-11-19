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


#####################2. Análisis Estadístico Descriptivo de Datos#####################

#1. Importar el fichero Andalucia.txt y denominar a la hoja de datos (data frame) Datos.Andalucia. Comprobar si en el archivo 
#.txt hay datos faltantes y cómo están codificados.

Datos.Andalucia <-  read.table('./Datos/Andalucia-utf8.txt',sep="\t", header=TRUE, encoding  ="UTF-8")
Datos.Andalucia[Datos.Andalucia$Renta.familiar.por.habitante.2003 == "..","Renta.familiar.por.habitante.2003"] <- NA

#2. A partir de la variable código INE, construir una variable tipo factor que distinga la provincia de pertenencia de cada municipio,
#denominarla “Provincia” y añadirla al data frame. 
#head(ine)
Datos.fProv = c("Almeria","Cadiz","Cordoba","Granada","Huelva","Jaen","Malaga","Sevilla")

Datos.Andalucia$IdINE <- substr(Datos.Andalucia$Codigo.INE,1,nchar(Datos.Andalucia$Codigo.INE)-3)

Datos.Andalucia$Provincia[Datos.Andalucia$IdINE == 4] <- Datos.fProv[1]
Datos.Andalucia$Provincia[Datos.Andalucia$IdINE == 11] <- Datos.fProv[2]
Datos.Andalucia$Provincia[Datos.Andalucia$IdINE == 14] <- Datos.fProv[3]
Datos.Andalucia$Provincia[Datos.Andalucia$IdINE == 18] <- Datos.fProv[4]
Datos.Andalucia$Provincia[Datos.Andalucia$IdINE == 21] <- Datos.fProv[5]
Datos.Andalucia$Provincia[Datos.Andalucia$IdINE == 23] <- Datos.fProv[6]
Datos.Andalucia$Provincia[Datos.Andalucia$IdINE == 29] <- Datos.fProv[7]
Datos.Andalucia$Provincia[Datos.Andalucia$IdINE == 41] <- Datos.fProv[8]

#Obtener la distribución de frecuencias absolutas y relativas, un diagrama de barras con las frecuencias absolutas y un 
#diagrama de sectores con las frecuencias relativas en porcentajes de esta variable tipo factor. 
#¿Qué provincia tiene más municipios? ¿Cuál tiene menos? ¿Qué porcentaje representa en cada caso?

Datos.Andalucia$Provincia <- as.factor(Datos.Andalucia$Provincia)
print('Frecuencia absoluta: ')
frec.abs <-table(Datos.Andalucia$Provincia)
frec.abs
print('Frecuencia relativa')
frec.rel <-table(Datos.Andalucia$Provincia)/sum(table(Datos.Andalucia$Provincia))*100
frec.rel
#Bar plot frecuencias absolutas
barplot(frec.abs, main="Frecuencia absolutas por provincia")

#Pie plot frecuencias relativas
slices <- frec.rel
lbls <- rownames(frec.rel)
pct <- round(slices/sum(slices)*100,2)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Frecuencias relativas por provincia") 

#3. Obtener un resumen descriptivo de la variable tasa de actividad de 2001 que incluya parámetros de posición, dispersión, asimetría
#y curtosis, histograma y diagrama de caja. En función de este resumen, contestar a las siguientes preguntas:

#Vamos a arreglar la columna para los calculos
#Cambiar comas por puntos. Si lo dejaramos como está la conversión a otro tipo de dato sería errónea.

Datos.Andalucia$Tasa.actividad.2001 <- gsub("\\,", ".", Datos.Andalucia$Tasa.actividad.2001)
Datos.Andalucia$Tasa.actividad.2001 <- (as.double(Datos.Andalucia$Tasa.actividad.2001))

#####################Parámetros de posición#####################

summary(Datos.Andalucia$Tasa.actividad.2001)
#Media
media <- mean(Datos.Andalucia$Tasa.actividad.2001)
#Mediana
mediana <- median(Datos.Andalucia$Tasa.actividad.2001, na.rm=T)
# Percentil 5% y percentil 95%
percep <-quantile(Datos.Andalucia$Tasa.actividad.2001,probs=c(0.05,0.95),na.rm=TRUE)
## Mínimo
min.Tasa<-min(Datos.Andalucia$Tasa.actividad.2001, na.rm=T)
## Máximo
max.Tasa<-max(Datos.Andalucia$Tasa.actividad.2001, na.rm=T)
## Primer cuartil
q1.Tasa<-quantile(Datos.Andalucia$Tasa.actividad.2001, probs=0.25, na.rm=T)
## Tercer cuartil
q3.Tasa<-quantile(Datos.Andalucia$Tasa.actividad.2001, probs=0.75, na.rm=T)

#####################Parámetros de dispersión#####################
# Desviación típica
Des.tipica <- sd(Datos.Andalucia$Tasa.actividad.2001,na.rm=TRUE)
# Varianza
Varianza <- var(Datos.Andalucia$Tasa.actividad.2001,na.rm=TRUE)
# Recorrido intercuartílico
ri.Tasa<-q3.Tasa-q1.Tasa
# Rango
rango.Tasa<-max.Tasa-min.Tasa


#####################Parámetros de forma###################
install.packages("e1071", dep = TRUE)
library(e1071)
coef_asimetria <- skewness(Datos.Andalucia$Tasa.actividad.2001,na.rm=TRUE)
print (coef_asimetria)

coef_curtosis <- kurtosis(Datos.Andalucia$Tasa.actividad.2001,na.rm=TRUE)
print (coef_curtosis)

#histograma
hist(Datos.Andalucia$Tasa.actividad.2001, main = "Tasa de Actividad de Andalucía en 2001", xlab="Tasa Actividad")

#Diagrama de caja
boxplot(Datos.Andalucia$Tasa.actividad.2001, main = "Tasa de Actividad de Andalucía en 2001", 
        col = "lightgray", ylab="Tasa Actividad")

## boxplot on a formula:
boxplot(count ~ spray, data = InsectSprays, col = "lightgray")
# *add* notches (somewhat funny here):
boxplot(count ~ spray, data = InsectSprays,
        notch = TRUE, add = TRUE, col = "blue")

#3.1. ¿Cuál es la tasa media de actividad de los municipios andaluces? 
#¿Crees que este valor es adecuado para representar la Tasa de Actividad de los municipios andaluces durante 2001?

#Valor medio = 51.44
#Creemos que es un valor correcto ya que no tenemos outlayers muy grandes lo que hace que la media y la media estén muy cercanas

#3.2. ¿Cómo valoras la homogeneidad de los valores de la tasa de actividad en los municipios andaluces? 
#¿Qué parámetro elegirías para representar la dispersión de la Tasa de Actividad de 2001?

#En el dataset analizado la tasa de actividad parece homogneo ya que los datos no presentan mucha desviación y el coeficiente de
#asimetría presenta un valor muy bajo.

#Podemos elegir la desviación típica ya que es una medida de dispersión de las variables Des.tipica. Segun los valores que tenemos
#consideramos que el valor de desviación típica 6.98 es bastante bajo viendo que los valores estan entre 30 y 70

#3.3. ¿En ese sentido, qué municipios andaluces destacan significativamente del resto (como atípicos) 
#por su alta tasa de actividad y por su baja tasa de actividad? ¿Se te ocurre alguna explicación al respecto?

#which(Datos.Andalucia$Tasa.actividad.2001==min.Tasa)
outlier_values <- boxplot.stats(Datos.Andalucia$Tasa.actividad.2001)$out #vemos los outlayers
Datos.Andalucia$Municipio[which(Datos.Andalucia$Tasa.actividad.2001<=28.08)] #-- Benitagla    Lobras  
Datos.Andalucia$Municipio[which(Datos.Andalucia$Tasa.actividad.2001>=70.96)] #-- Vícar                Ejido (El)           Mojonera (La)        Alcalá del Valle     Castilleja de Guzmán)$out 

#En principio, no parece que los outliers se deban a datos anómalos o mal registrados, puesto que la tasa de actividad no deja de ser un valor razonable.
#Para los municipios que poseen una tasa de actividad baja (Benitagla y Lobras) se ha observado que se trata de pueblos con muy poca población y una edad media bastante elevada. Lo cual explicaria una reducida tasa de actividad.
#Por el contrario, los municipios que presentan una tasa de actividad alta (Vícar, Ejido (El), Mojonera (La), Alcalá del Valle y Castilleja de Guzmán)
#son municipios con una media de edad baja y de poblacion bastante elevada, lo cual explicaria una tasa de actividad mas alta.


#3.4. ¿Cómo valoras la simetría de la distribución de frecuencia?

#Es bastante simétrica ya que el coefieciente de asimetria es cercano a 0. 