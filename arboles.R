
#------------------------------------------------------------------#
#  Arboles de Clasificacion (Algoritmo RPART)                      #
#------------------------------------------------------------------#
# Leer conjunto de datos
bupa <- read.csv(file.choose())
#bupa<-read.table("bupa.txt",header=T,sep=",") 
head(bupa)
# V7: 1 (higado enfermo) 2 (higado sano)
# V3: alamine aminotransferase
# V5: gamma-glutamyl transpeptidase
str(bupa)
# Declarar V7 como un factor
bupa[,7]<-as.factor(bupa[,7])

# Cargar libreria rpart
library(rpart)
table(bupa$V7)
# Ejemplo 1: considerando minbucket=50 (minsplit=150)

#Estimar el árbol
arbol1=rpart(V7~V3+V5,data=bupa,method="class",
             minbucket=50)
arbol1

# Graficando el arbol
plot(arbol1,margin=.25)
text(arbol1,use.n=T)

#Mejorando los Gráficos
library(partykit)
plot(as.party(arbol1), tp_args = list(id = FALSE))

library(rattle)
drawTreeNodes(arbol1)



#Ejemplo 2: minbucket=20 (minsplit=60) para obtener un árbol con más ramas
arbol2=rpart(V7~V3+V5,data=bupa,method="class",
             minbucket=20)
arbol2
drawTreeNodes(arbol2)
plot(as.party(arbol2), tp_args = list(id = FALSE))

#Ejemplo 3: Controlando el crecimiento del árbol
#con el parámetro de complejidad (cp=0.05)

arbol3=rpart(V7~V3+V5,data=bupa,method="class", cp=0.05)
drawTreeNodes(arbol3)
plot(as.party(arbol3), tp_args = list(id = FALSE))

#Ejemplo 4: cp=0.001 para obtener un árbol con más ramas
arbol4=rpart(V7~V3+V5,data=bupa,method="class",
             cp=0.001)
drawTreeNodes(arbol4)
plot(as.party(arbol4), tp_args = list(id = FALSE))

#Ejemplo 5: Controlando el crecimiento del árbol
#por número máximo de niveles (maxdepth=3)
arbol5=rpart(V7~V3+V5,data=bupa,method="class",
             maxdepth=3)
drawTreeNodes(arbol5)
plot(as.party(arbol5), tp_args = list(id = FALSE))

#Ejemplo 6: Podar el árbol
  set.seed(060717)
  arbol=rpart(V7~V3+V5,data=bupa,method="class",
             cp=0.001)
  plot(as.party(arbol), tp_args = list(id = FALSE))
  
  arbol6=prune(arbol,cp=.1)
  plot(as.party(arbol6), tp_args = list(id = FALSE))

# Elegir un valor de CP
  printcp(arbol)
  plotcp(arbol)

## Usando el criterior del Min xerror
arbol7=prune(arbol,cp=0.0137931)
plot(as.party(arbol7), tp_args = list(id = FALSE))

## Usando el criterior de +-xstd
arbol7=prune(arbol,cp=0.058)
plot(as.party(arbol7), tp_args = list(id = FALSE))

# Automatizando la selección del Valor óptimo de CP (creterio Min xerror)
arbol.completo <- rpart(V7~ .,data=bupa,method="class",
             cp=0, minbucket=0)
arbol.completo
xerr <- arbol.completo$cptable[,"xerror"]
minxerr <- which.min(xerr)
mincp <- arbol.completo$cptable[minxerr, "CP"]
arbol.prune <- prune(arbol.completo,cp=mincp)
plot(as.party(arbol.prune), tp_args = list(id = FALSE))

#Predicción usando el árbol podado
#Calcular los valores predichos
pred<-predict(arbol.prune,bupa[,c(-7)],type="class")
#pred<-predict(arbol.prune,bupa[,c(-7)],type="class")

#Calcular la matriz de confusión
library(caret)
confusionMatrix(pred,bupa$V7)

#---------------------------------------------------------------------#
#  Arboles de Clasificacion (Algoritmo Party - Inferencia condicional)#
#---------------------------------------------------------------------#

bupa$V7<- as.factor(bupa$V7)

library(party)
arbolc1<-ctree(V7~V3+V5,data=bupa)
arbolc1

plot(arbolc1)

arbolc2<-ctree(V7~.,data=bupa,
               controls = ctree_control(mincriterion = 0.95))
arbolc2
plot(arbolc2)

lista = c(3,5)

pred<-Predict(arbolc2,bupa[,-7],type="response")
confusionMatrix(pred,bupa$V7)

