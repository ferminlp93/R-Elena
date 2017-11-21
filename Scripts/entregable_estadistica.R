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

#4. Obtener un gráfico de caja de la Tasa de actividad en 2001 en función de la provincia y describe brevemente la información que contienen los datos a partir del gráfico.

boxplot(formula = Datos.Andalucia$Tasa.actividad.2001 ~ Datos.Andalucia$Provincia, 
        main = "Tasa de Actividad por Provincia",
        xlab = "Provincia",
        ylab = "Actividad",
        col = rainbow(8))

#A la vista del gráfico de cajas podemos observar que la mediana en todas las provincias se situa entorno al mismo rango (entre el 45 y 55 de Tasa de Actividad). Hecho que coincide 
#con el análisis realizado por el conjunto de tasa de actividad sobre todo el conjunto andaluz.
#Además, si bien es cierto que la mayoría de provincias presentan una varianza similar, hay casos especiales como puede ser Almería (que muestra una alta dispersión)
#y Cádiz (que presenta una varianza muy baja, lo que hace que la mayoría de valores se sitúen próximos a su mediana).
#Por otra parte, a pesar de que tanto Cádiz como Sevilla presentan ligeras asimetrías negativa y positiva, respectivamente, no se trata de asimetrías que puedan ser
#consideradas significativas. En conclusión podemos afirmar que la tasa de actividad de todas las provincias presentan un coeficiente de asimetría prácticamente nulo.
#Por último, podemos observar como el conjunto de outliers depende de cada provincia. Habiendo provincias que no presentan datos anómalos (Córdoba o Málaga),
#mientras que otras representan varios outliers (Cádiz y Sevilla, por ejemplo).




#####################################
# 3. DISTRIBUCIONES DE PROBABILIDAD #
#####################################

#1. Consideremos una variable aleatoria que sigue una distribución B (15, 0.33). Se pide:

#1.1. ¿Qué valor de la variable deja por debajo de sí el 75% de la probabilidad?
qbinom(0.75, 15, 0.33) #6

#1.2. Calcular el percentil 95% de la distribución.
qbinom(0.95, 15, 0.33) #8

#1.3. Obtener una muestra de tamaño 1000 de esta distribución, representarla gráficamente las frecuencias observadas de cada valor de la distribución 
#mediante un diagrama de barras y comparar éste con las frecuencias esperadas según el modelo que genera los datos.

#Generamos las muestras
muestra_binomial <- rbinom(1000, 15, 0.33)

#Comprobación de datos
summary(muestra_binomial)

#Histograma de la muestra binomial generada
hist(muestra_binomial, main="Distribución Binomial")

#A la vista del histograma con las frecuencias obtenidas, vemos que los resultados se ajustan al modelo esperado:
#Como ya hemos observado con el comando 'summary' la mediana se encuentra en el valor 5, mientras que los C1 y C3 se sitúan en los valores 4 y 6, respectivamente.
#Además, a la vista de la gráifca vemos que coincide con los valores obtenidos en los puntos 1.1 y 1.2:
#Ya que el valor 6 intuimos que puede dejar a su izquierda el 75% de los datos, mientras que el valor 8 abarca el 95% de los mismos.


#2. Consideremos una variable aleatoria W con distribución N (250, 13). Se pide:

#2.1. P [240 < W ≤ 245.5]

#P(240 < W < 245.5) = F(245.5) - F(240)
pnorm(245.5, 250, 13) - pnorm(240, 250, 13) #0.1437354

#2.2. P [W ≥ 256]
#P(W > 256) = 1 - P(W <= 256)
1 - pnorm(256, 250, 13) #0.3222062

#2.3. Si queremos desechar el 5% de valores más altos de la distribución y el 5% de valores más bajos, ¿con qué intervalo de valores nos quedaremos?

#Limite inferior: N(m=250, S=13). P(W <= w)=0.05 ¿w? 
qnorm(0.05, 250, 13)  #228.6169

#Limite superior: N(m=250, S=13). P(W <= w)=0.95 ¿w? 
qnorm(0.95, 250, 13)  #271.3831

#Por tanto, el intervalo de valores con el que nos quedaremos será [228.6169 - 271.3831]


#2.4. Obtener una muestra de tamaño 1000 de la distribución, representar la función de densidad de esta distribución y compararla con el histograma de la muestra obtenida.

#Media
m <- 250 
#Desviación típica
sd <- 13
#Números
n <- 1000

#Generación de la muestra
muestra <- rnorm(n, m, sd) 

#Histograma
hist(muestra)

#Función de densidad
li<-m-3*sd
ls<-m+3*sd

# Número de puntos a dibujar
npuntos<-ls-li
x<-seq(li, ls, length.out=npuntos)
# Valores de y
y<-dnorm(x, m, sd)

# Dibujar f(w)
fw<-plot(x, y, type="l", xlab="Variable W", ylab="f(w)", main="Función de densidad N(250, 13)", col="red")

#Representamos histograma y función de densidad en la misma gráfica
#Intervalos
int<-round(sqrt(n), 0) 

# Histograma de la muestra
hist(muestra, breaks=int, freq=F, xlab="muestra", ylab="Densidad",     main="Histograma", col="lightblue", border="blue")
# Incluimos f(w)
lines(muestra, dnorm(muestra,m,sd), type="p", col="red") 


########################################################
# 4. CONTRASTES DE HIPÓTESIS E INTERVALOS DE CONFIANZA #
########################################################

#Descripción del dataset
#Mediante una red de sensores se han recogido datos sobre la temperatura media diaria (ºC) en dos estaciones A y B durante 52 días. 
#Los valores recogidos de la temperatura se encuentran en la hoja de datos “Temper” incluida en el fichero Temperatura.RData.

#Ejercicios
#1. Cargar el fichero Temperatura.RData. 
load('./Datos/Temperatura.RData')
str(Temper)

#2. Crear dos nuevas variables, temp.A y temp.B, que contengan las temperaturas de las estaciones A y B, respectivamente.
temp.A <- Temper[Temper$Estacion == "A",]$Temper
temp.B <- Temper[Temper$Estacion == "B",]$Temper


#3. Da un intervalo de confianza para la temperatura media diaria de la estación A, al 95%, y a partir de éste indica si se puede admitir, y por qué, 
#que la temperatura media diaria en dicha estación sea de 19ºC, con ese mismo nivel de confianza.
hist(temp.A)
summary(temp.A)

IC <- t.test(temp.A, alternative = "two.sided", conf.level = 0.95)
IC #El intervalo de confianza al 95% es [19.39401, 20.24131].
#Por lo tanto, no se puede admitir que 19ºC sea la temperatura media diaria de la estación A si se desea un intervalo de confianza del 95%

#4. Plantea un test de hipótesis que refleje la pregunta del apartado anterior y resuélvelo sin usar el intervalo de confianza (riesgo de 1ª especie 5%)

#El contraste que se plantea es:
#H0: mA = 19
#H1: mA <> 19
#Como se pide explícitamente contrastar si la media puede ser 19ºC, ahora sí hay que especificar el valor del argumento mu = 19.
Contraste <- t.test(temp.A, alternative = "two.sided", mu = 19, conf.level = 0.95)
Contraste
#El p-valor es 0.0002496, y es mucho menor que 0,05, por lo que podemos afirmar que la temperatura media en la estación A
#no estaría en 19ºC con un riesgo de 1ª especie del 5%.

#5. Determina si puede admitirse, con un riesgo de primera especie de 1%, que la temperatura media diaria es la misma en las dos estaciones. 
#Plantea previamente el correspondiente contraste de hipótesis.

#En este caso vamos a comparar la temperatura en la estación A (temp.A) y la temperatura en la estación B (temp.B).
#Se trata de una comparación de medias de 2 poblaciones pareadas: se comparan dos características (temp.A y temp.B) para 2 estaciones de temperatura.
#Mediante el test vamos a tratar de determinar si puede admitirse, con un riesgo de primera especie
#de 1% (alfa), que la temperatura media es la misma en la estación A y en la B.
#El contrsaste o test de hipótesis que se plantea es:
#H0: mA = mB
#H1: mA <> mB

#En primer lugar vamos a determinar si las varianzas de la temp media en ambas estaciones son iguales con el var.test() (Test F). 
var.temp <- var.test(temp.A, temp.B, ratio = 1, alternative = "two.sided", conf.level = 0.99)
var.temp
#El p-valor es 0.6825, y es mayor que 0,1, por lo que podemos afirmar que la varianza de la temperatura en la estación A no difere de la del B,
#con un riesgo de 1ª especia del 1%
#Así pues, ahora podemos volver sobre la comparación de medias (temp) asumiendo que las varianzas de las temperaturas en las estaciones A y B son iguales.
#Por otra parte, tal y como se ha planteado el contraste inicialmente, consideramos que el procedimiento estadístico empleado es bilateral,
#por lo que incorporamos el argumento alternative = "two.sided". 

#Por último, la hipótesis a contrastar (mA = mB) representa que las medias de la temperatura en la estación A y en la estación B pueden considerarse iguales.
#Esto esm la hipótesis nula implícita es que la diferencia de las medias de la temperatura es cero (mA - mB = 0).
#Por tanto, el valor del argumento mu, que representa la diferencia de medias que se contrasta en la H0, debe ser mu = 0.
#Ya tenemos toda la información necesaria para realizar el contraste ejecutando el test.t:
test.temperaturaAB <- t.test(temp.A, temp.B, alternative = "two.sided", mu = 0, paired = F, var.equal = T, conf.level = 0.99)
test.temperaturaAB
#El p-valor es 0.5224, y es mayor que 0,1 (mayor que 0.5, de hecho), por lo que podemos afirmar que la temperatura media
#en la estación A no difiere significativamente de la temp media de la B, con un riesgo de 1ª especie del 1%. 

#6. Obtén un intervalo de confianza (99%) para la diferencia de temperaturas entre estaciones. 

#Este intervalo de confianza se puede obtener a raíz del resultado obtenido en el apartado anterior. Donde obtuvimos:
#99 percent confidence interval:
#  -0.9490744  0.5745257

#Así pues, el intervalo de confianza al 99% es el siguiente: [-0.9490744, 0.5745257].
#Es importante destacar que este rango no representa un intervalo para las temperaturas medias, sino para la diferencia de las medias entre la temperatura en la estación A y la temp en la estación B.

#¿Aporta alguna información adicional al resultado obtenido en el apartado anterior?

#Para saber si las medias de temperatura entre ambas estaciones pueden considerarse iguales, bastaría sólo con el contraste.
#Sin embargo, con el intervalo de confianza podemos determinar también el umbral a partir del cuál podemos considerar ambas medias significativamente iguales o no.
#De este modo el intervalo [-0.9490744, 0.5745257] nos indica que cualquier hipótesis nula que planteáramos donde las diferencias de las medias difirieran
#en -0.9490744 (si mA < mB) o 0.5745257 (si mA > mB) sería aceptada también.

#Si lo que queremos es simplemente determinar si podemos admitir que hay o no diferencias en cuanto a la media de la temperatura en las estaciones A y B,
#el cálculo del intervalo de confianza no aportaría nada, sería suficiente realizar el test propiamente dicho y evaluar el p valor.
#Ahora bien, dar un intervalo de confianza es más informativo que aceptar o rechazar una hipótesis concreta para un nivel de confianza determinado.
#En este caso, el intervalo [-0.9490744, 0.5745257] nos indica que cualquier hipótesis nula que planteáramos de modo que las medias de las temperaturas
#difirieran realmente en -0.9490744 (si mA < mB) o 0.5745257 (si mA > mB) sería aceptada también.
#Por tanto, si mA < mB no sería admisible una diferencia entre las medias menor que ****, en caso


#7. Se sabe que a lo largo de los 52 días, la estación A falló 5 días y la B 7 días. 
#¿Puede afirmarse con un nivel de confianza del 90% que la proporción de días fallados es la misma en las dos estaciones? 

#Para responder a esta pregunta podemos construir un IC para la diferencia de proporciones de temperaturas en ambas estaciones.
#Creamos matriz de contingencia
fallo.A <- 5
fallo.B <- 7

exito.A <- length(temp.A) - fallo.A
exito.B <- length(temp.B) - fallo.B

tabla_cont <- matrix(c(fallo.A,fallo.B, exito.A, exito.B), 2, 2)
tabla_cont

#Para un nivel de confianza del 90%, se tiene que el nivel de significación alfa es 0,1. Suponemos también que el contraste es bilateral.
#Realizamos el contraste: 
prop.test(tabla_cont, alternative = "two.sided", conf.level = 0.9)

#Como p-valor es 0.7993, y es mayor que 0,1, podemos afirmar que la proporción de fallos en la estación A no difiere
#significativamente de los de la estación B, con un riesgo de 1ª especie del 10%. 

#También llegamos a la misma conclusión observando que el 0 (que implicaría una diferencia nula entre ambas proporciones)
#se encuentra también en el intervalo de confianza para la diferencia de proporciones obtenido: [-0.10648910  0.05914404].
