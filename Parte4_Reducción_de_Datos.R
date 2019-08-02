# Instalacion de Paquetes
install.packages(c("VIM","DEoptimR", "psych", "minqa","nloptr","DMwR", "mvoutlier","TTR","caTools",
                   "AppliedPredictiveModeling","caret","arules"),dependencies = c("Depends", "Suggests"))

#########################################################
#  Discretizacion                                       #
#########################################################
#db: voto_capitales_2016
#pw: sdc2019PERU
capitales <- dbGetQuery(con, "SELECT * from tabla_voto_capitales")
names(capitales)
#Discretizacion con intervalos de igual amplitud
library(arules)
names(capitales)[8]
nclass_scott<-nclass.scott(capitales[,8]) # Usando Scott
nclass_sturges<-nclass.Sturges(capitales[,8]) #Usando Sturges
nclass_fd<-nclass.FD(capitales[,8]) # Usando Friedman-Diaconis

str(capitales)
db.capitales=capitales
db.capitales[,8]<-discretize(capitales[,8],method="interval",breaks = nclass_sturges)
table(db.capitales[,8])


#Discretizacion con intervalos de igual frecuencia
db.capitales=capitales
db.capitales[,11]<-discretize(capitales[,11],method="frequency",breaks = 10)
table(db.capitales[,11])

#cargar data para discretización por entriopia y chiMerge
#db: ausentismo_2011
#pw: sdc2019PERU
ausentismo <- dbGetQuery(con, "SELECT * from tabla_resultados")
names(ausentismo)
ausentismo_selec<-ausentismo[ c("IDH", "alf", "NBI", "GINI","dic_aus")]
str(ausentismo_selec)

per.miss.col=100*colSums(is.na(ausentismo_selec.c))/dim(ausentismo_selec.c)[1]
per.miss.col

library(DMwR)
#centralImputation reemplaza:
#Si la variable es numerica (numeric o integer en R) reemplaza los valores faltantes con la mediana.
#Si la variable es categórica (factor en R) reemplaza los valores faltantes con la moda.
ausentismo_selec.c<-centralImputation(ausentismo_selec)
ausentismo_selec.c$dic_aus = as.factor(ausentismo_selec.c$dic_aus)
str(ausentismo_selec.c)

#Discretizacion por entropia
library(discretization)
dausentismo=mdlp(ausentismo_selec.c)
dausentismo$Disc.data
table(dausentismo[,1])
table(dausentismo[,2])
table(dausentismo[,3])
table(dausentismo[,4])

#Discretizacion con chiMerge
dausentismo=chiM(ausentismo_selec.c, alpha =0.10)$Disc.data
table(dausentismo[,1])
table(dausentismo[,2])
table(dausentismo[,3])
table(dausentismo[,4])

#Discretización con chiMerge de data Iris
data(iris)
disc=chiM(iris,alpha=0.05)$Disc.data

#########################################################
#  Analisis de componentes principales                  #
#########################################################

####################################################################
#Ejemplo: Crimenes y Arrestos en EEUU                              #
####################################################################
# El conjunto de datos USArrests de la librer?a datasets de R contiene las
# estad?sticas de arrestos por cada 100000 residentes por asaltos (Assault),
# asesinatos (Murder) y violaciones (Rape) en cada uno de los 50 estados de
# los EEUU en el a?o 1973. Adem?s se muestra el porcentaje de la poblaci?n 
# que vive en el ?rea urbana (UrbanPop).

data(USArrests)
head(USArrests)

# Analisis descriptivo
library(psych)
describe(USArrests)
corr.test(USArrests)
cor.plot(cor(USArrests))

# Prueba de Esfericidad de Bartlett
cortest.bartlett(cor(USArrests),n=dim(USArrests)[1])

# Numero de Componentes
scree(USArrests)
fa.parallel(USArrests,fa="pc")

pc <- prcomp(x=USArrests,
             scale=TRUE, center=TRUE,tol=0)
pc
plot(pc)

# Media y desviaci?n est?ndar de las variables 
pc$center
pc$scale

# Carga de los componentes
pc$rotation

# Biplot
biplot(pc,scale=0)

# Varianza Explicada
pc.var<-pc$sdev^2
pc.var
pve<-pc.var/sum(pc.var)#variabilidad
pve #impirta
plot(pve,xlab="Componente Principal", ylab="Proporci?n de Varianza Explicada",
     ylim=c(0,1), type="b")
plot(cumsum(pve),xlab="Componente Principal", ylab="Proporci?n Acumulada de Varianza Explicada",
     ylim=c(0,1), type="b" )

# Scores
scale(as.matrix(USArrests))%*%(pc$rotation)
pc$x
cor(pc$x)

# Adicionar Scores a la base de datos
USArrests_nuevo <- cbind(USArrests,pc$x[,1:2])
head(USArrests_nuevo)



####################################################################
#Ejemplo: Distritos                                                #
####################################################################

library(haven)
distritos <- as.data.frame(read_sav(file.choose())) 

colnames(distritos) <- tolower(colnames(distritos))
nombres=distritos[,1]
distritos=distritos[,-1]
rownames(distritos)=nombres
head(distritos)

library(psych)
describe(distritos)
corr.test(distritos)
cor.plot(cor(distritos))


#Bartlett
cortest.bartlett(cor(distritos[,-1]),n=dim(distritos)[1])


scree(distritos)
fa.parallel(distritos,fa="pc")#n de componentes

pc <- prcomp(x=distritos,
             scale=TRUE, center=TRUE,tol=0)

#Media y desviaci?n est?ndar de las variables 
pc$center
pc$scale

#Cargas de los componentes
pc$rotation

biplot(pc,scale=0)

pc.var<-pc$sdev^2
pc.var

#variabilidad
pve<-pc.var/sum(pc.var)
pve

plot(pve,xlab="Componente Principal", ylab="Proporcion de Varianza Explicada",
     ylim=c(0,1), type="b")
plot(cumsum(pve),xlab="Componente Principal", ylab="Proporci?n Acumulada de Varianza Explicada",
     ylim=c(0,1), type="b" )
