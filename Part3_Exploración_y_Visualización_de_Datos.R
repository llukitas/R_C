#####################################################################
#                 Análisis Exploratorio de Datos                    #
#                               y                                   #
#                   Estadisticas Descriptivas                       #
#####################################################################
############                LAPOP                        ############
############              2004-2012                      ############
#####################################################################
#MOSTRAR DATOS
View(lapop)#visualizar data frame
str(lapop)#estructura de datos de toda la base de datos
str(lapop$b1)

#-----------------------------------------------------------------------------#
#  a) Estadísticas descriptivas y graficos de una variable cualitativa        #
#-----------------------------------------------------------------------------#
#Presente una tabla de distribucion de frecuencias para la pregunta:
#b1: ¿Hasta que punto cree que los tribunales de justicia de [PAIS] garantizan un juicio justo? 
table(lapop$b1)

#La variable b1 está representada en la base de datos como un factor. Para poder 
#sumarla a las demás es necesario convertirla en un vector numérico y excluir los 
#valores perdidos (DK, NR y N/A): 
lapop$b1.r <- as.numeric(lapop$b1) 
lapop$b1.r[lapop$b1.r > 7] <- NA 
table(lapop$b1.r,useNA="ifany") 

# Tabla de Frecuencia
# -------------------
ni<-table(lapop$b1.r) #frecuencia
fi<-prop.table(table(lapop$b1.r)) #proporciones
pi<-round(prop.table(table(lapop$b1.r))*100,2) #porcentajes
b1.tabla<-t(rbind(ni,fi,pi)) #mostrar en tabla
b1.tabla #llamar presentaci?n de tabla



# Grafico de Barras
# -------------------
barplot(pi, main="Distribucion de la intensidad de Apoyo a los Tribunales de Justicia", 
        xlab="Intensidad",
        ylab="Porcentaje")

par(mfrow=c(2,1))
barplot(table(lapop$b1.r), 
        main="Distribucion de la intensidad de Apoyo a los Tribunales de Justicia", 
        col=2,xlab="Edad",ylab="Numero")
barplot(prop.table(table(lapop$b1.r))*100,
        main="Distribucion de la intensidad de Apoyo a los Tribunales de Justicia", 
        col=3,xlab="Edad",ylab="Porcentaje")

par(mfrow=c(1,1))


# Grafico de Sectores Circulares
pie(pi, main="Distribucion de la intensidad de Apoyo a los Tribunales de Justicia")

## Colocar porcentajes #paste para concatenar
lbls1 <- paste(names(table(lapop$b1.r)), "\n",
               round(prop.table(table(lapop$b1.r))*100,2),"%", sep="")
pie(pi, labels = lbls1,
    main="Distribucion de la intensidad de Apoyo a los Tribunales de Justicia")

# graficos con porcentajes: en 3D
library(plotrix)#paquete que contiene la funcion pie3D para realizar el diagrama 3D
pie3D(table(lapop$b1.r),labels=lbls1,explode=0.1,
      main="Distribucion de la intensidad de Apoyo a los Tribunales de Justicia")

# Dot Plots
dotchart(table(lapop$b1.r), cex=.7,
         main="Distribucion de la intensidad de Apoyo a los Tribunales de Justicia", 
         xlab="Porcentaje")


#------------------------------------------------------------------#
#  b) Tablas de contingencia                                       #
#------------------------------------------------------------------#

#q1: Sexo
#1=hombre
#2=mujer
#b1: ¿Hasta que punto cree que los tribunales de justicia de [PAIS] garantizan un 
#juicio justo? 


#Excluimos los valores perdidos (DK, NR y N/A): 
lapop$b1.r <- as.numeric(lapop$b1) 
lapop$b1.r[lapop$b1.r > 7] <- NA 
lapop$q1.r <- as.numeric(lapop$q1) 
lapop$q1.r[lapop$q1.r > 2] <- NA 

tabla1=table(lapop$q1.r,lapop$b1.r)
tabla1


#------------------------------------------------------------------#
#  c) Distribucion condicional                                     #
#------------------------------------------------------------------#

#Presente la distribucin condicional de la intensidad de aprobación a los tribunales
#de justicia por grupo de sexo. Construya una grafica adecuada. Es posible afirmar que 
#existe asociaci?n entre ambas variables?

tabla2=round(prop.table(tabla1,margin=1),2)
tabla2

# Barras agrupadas
barplot(tabla2,col=2:3,beside = T,
        xlab="intensidad de Apoyo a los Tribunales de Justicia",
        ylab="Proporcion de Hombres y Mujeres",
        main="Distribucion de la intensidad de Apoyo a los Tribunales de Justicia segun sexo")

#----------------------------------------------------------------------------------#
#  d) Representacion y visualizacion de Datos cuantitativos discretos              #
#----------------------------------------------------------------------------------#

#Presente una tabla de distribucion de frecuencias para el numero de hijos. 
#Construya una grafica adecuada.

lapop$q12.r <- as.numeric(lapop$q12) 
lapop$q12.r[lapop$q12.r > 25] <- NA 

# Tabla de Frecuencias
ni<-table(lapop$q12.r)
fi<-prop.table(table(lapop$q12.r))
pi<-round(prop.table(table(lapop$q12.r))*100,2)
hijos.tabla<-t(rbind(ni,fi,pi))
hijos.tabla

#Gráfico de Varas
plot(pi, type="h", lwd=2,
     xlab="Numero de hijos",
     ylab="Porcentaje de entrevistados",
     main="Distribucion del numero de hijos de los entrevistados")
points(x =as.numeric(row.names(pi)),
       y =as.numeric(pi),
       pch=19,cex=1.5)

#----------------------------------------------------------------------------------------------#
#  e) Construcción de indicadores y analisis exploratorio datos cuantitativos continuos        #
#----------------------------------------------------------------------------------------------#

# Indice de Apoyo al Sistema Politico - ASP
# -------------------
lapop$b1.r <- as.numeric(lapop$b1) 
lapop$b2.r <- as.numeric(lapop$b2) 
lapop$b3.r <- as.numeric(lapop$b3) 
lapop$b4.r <- as.numeric(lapop$b4) 
lapop$b6.r <- as.numeric(lapop$b6) 
lapop$b1.r[lapop$b1.r > 7] <- NA 
lapop$b2.r[lapop$b2.r > 7] <- NA 
lapop$b3.r[lapop$b3.r > 7] <- NA 
lapop$b4.r[lapop$b4.r > 7] <- NA 
lapop$b6.r[lapop$b6.r > 7] <- NA
lapop$asp <- (((lapop$b1.r+lapop$b2.r+lapop$b3.r+lapop$b4.r+lapop$b6.r)-5)/30)*100 

hist(lapop$asp)

#Para ver cuantos valores perdidos (missing values) tiene el índice: 
miss.asp <- is.na(lapop$asp) 
t1 <- table(miss.asp) 
t1
prop.table(t1)*100

data<-lapop[,c("asp", "pais", "tamano")]
#Para ver el porcentaje de valores perdidos en las columnas
per.miss.col=100*colSums(is.na(data))/dim(data)[1]
per.miss.col

#preparas la data
data$pais = as.factor(data$pais)
data$tamano = as.factor(data$tamano)

# K-Vecinos más cercanos
library(DMwR)
str(data)
data.d<-centralImputation(data)

factorx <- factor(cut(lapop$asp, breaks=nclass.Sturges(lapop$asp),right=TRUE))
xout <- as.data.frame(table(factorx))
colnames(xout)<-c("asp","ni")
xout <- transform(xout, 
                  fi=prop.table(ni), #proporciones
                  pi=prop.table(ni)*100, #porcentaje
                  Ni = cumsum(ni), #frecuencia
                  Fi = cumsum(prop.table(ni)), #proporcion acumulada
                  Pi = cumsum(prop.table(ni))*100 #porcentaje acumulado
)
xout


library(agricolae)
(table.freq(hist(lapop$asp,breaks = "Sturges"))) #Regla Sturges
(table.freq(hist(lapop$asp,breaks = "Scott")))   #Regla de Scott
(table.freq(hist(lapop$asp,breaks = "FD")))      #Regla de Friedman-Diaconis
(table.freq(graph.freq(lapop$asp,plot=FALSE)))   #Regla Sturges (Agricolae)


# Histograma y poligono de frecuencia
h1<-hist(lapop$asp,breaks = "Sturges",
         xlab="asp",
         ylab="Numero de entrevistados")
polygon.freq(h1,frequency=1,col="red")


# Poligono de Frecuencias (solo)
h1<-hist(lapop$asp,border=FALSE)
polygon.freq(h1,frequency=1,col="red")

# Histograma (Comparativo)
par(mfrow=c(1,3))
hist(lapop$asp[lapop$pais=="11"],ylim=c(0,1800))
hist(lapop$asp[lapop$pais=="10"],ylim=c(0,1800))
hist(lapop$asp[lapop$pais=="16"],ylim=c(0,1800))
par(mfrow=c(1,1))

#Histograma y Densidad
hist(data$asp,prob=TRUE)
lines(density.default(data$asp))#para grafico de densidad se tiene que trabajar con data limpia

#Grafico de Densidad
plot(density(data$asp))

#Boxplots
boxplot(lapop$asp)


#------------------------------------------------------------------#
#  f) Analisis descriptivo                                         #
#------------------------------------------------------------------#


# Resumen basico
summary(data$asp)

# Otras funciones de resumen
library(Hmisc)
describe(data$asp)

library(fBasics)
basicStats(data$asp)

#------------------------------------------------------------------#
#  g) Analisis descriptivo comparativo                             #
#------------------------------------------------------------------#
data$tamano <- factor(data$tamano)#para eliminar las etique tas vacías


me<-tapply(X = data$asp,INDEX = data$tamano,FUN=mean)
med<-tapply(X = data$asp,INDEX = data$tamano,FUN=median)
q1<-tapply(X = data$asp,INDEX = data$tamano,FUN=quantile,probs = 0.25,type = 6)
q3<-tapply(X = data$asp,INDEX = data$tamano,FUN=quantile,probs = 0.75,type = 6)
r<-tapply(X = data$asp,INDEX = data$tamano,FUN=rango)
ric<-tapply(X = data$asp,INDEX = data$tamano,FUN=RIC)
s<-tapply(X = data$asp,INDEX = data$tamano,FUN=sd)
cv<-tapply(X = data$asp,INDEX = data$tamano,FUN=CV)
as3<-tapply(X = data$asp,INDEX = data$tamano,FUN=A3)

resumen<-t(as.matrix(rbind(me,med,q1,q3,r,ric,s,cv,as3)))
resumen

# h)

boxplot(data$asp ~ data$tamano,
        xlab="tamano de la ciudad",ylab="asp",
        main="Comparacion del asp por tamano de la ciudad")

