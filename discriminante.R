# Instalación de Paquetes
install.packages(c("psych"),
                 dependencies = c("Depends", "Suggests"))

#########################################################
#  Análisis Discriminantel Lineal (LDA)                 #
#########################################################
#DiabetesTrain
require("RPostgreSQL")
diabetes= dbGetQuery(con, "SELECT * from tabla_train")

# Analisis descriptivo
library(psych)
describeBy(diabetes[,-4],diabetes$clase)
pairs.panels(diabetes[,1:3],
bg=c("red","yellow","blue")[diabetes$class],pch=21)
par(mfrow=c(3,1))
boxplot(glucosa~clase,data=diabetes,main="glucosa")
boxplot(insulina~clase,data=diabetes,main="insulina")
boxplot(sspg~clase,data=diabetes,main="sspg")
par(mfrow=c(1,1))

# Estimación
library(MASS)
lda1<-lda(clase~.,data=diabetes)
lda1
# Predicción
plda1=predict(lda1,diabetes[-4])$class

diabetes$clase = as.factor(diabetes$clase)
str(diabetes)
confusionMatrix(plda1,diabetes$clase)
# Matriz de confusion
prop.table(table(diabetes$clase,plda1),1)
# Error
error1=mean(plda1!=diabetes$clase)
error1

library(klaR)
greedy.wilks(clase~.,data=diabetes)
stepclass(diabetes[,-4],diabetes$clase,method="lda")

# Estimación
lda2<-lda(clase~glucosa+insulina,data=diabetes)
# Predicción
plda2=predict(lda2,diabetes[-4])$class

# Matriz de confusion
prop.table(table(diabetes$clase,plda2),1)
# Error
error2=mean(plda2!=diabetes$clase)
error2

#########################################################
#  Análisis Discriminante Cuadrático (QDA)              #
#########################################################

qda(clase~.,data=diabetes)

# Estimación
qda1<-qda(clase~.,data=diabetes)
# Predicción
pqda1=predict(qda1,diabetes[-4])
pqda1=pqda1$class
# Matriz de confusion
prop.table(table(diabetes$clase,pqda1),1)

# Error
error1=mean(pqda1!=diabetes$clase)
error1

stepclass(diabetes[,-4],diabetes$clase,method="qda")

# Estimación
qda2<-qda(clase~insulina,data=diabetes)
# Predicción
pqda2=predict(qda2,diabetes[-4])$class
# Matriz de confusion
prop.table(table(diabetes$clase,pqda2),1)

# Error
error1=mean(pqda2!=diabetes$clase)
error1

#########################################################
#  Análisis Discriminante Regularizado (RDA             #
#########################################################

rda1<-rda(clase~.,data=diabetes)
# Predicción
prda1=predict(rda1,diabetes[-4])$class
# Matriz de confusion
prop.table(table(diabetes$clase,prda1),1)
# Error
error1=mean(prda1!=diabetes$clase)
error1


stepclass(diabetes[,-4],diabetes$clase,method="rda")

rda2<-rda(clase~insulina,data=diabetes)

# Predicción
prda2=predict(rda2,diabetes[-4])$class

# Matriz de confusion
prop.table(table(diabetes$clase,prda2),1)

# Error
error1=mean(prda2!=diabetes$clase)
error1
