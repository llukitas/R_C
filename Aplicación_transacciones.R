#########################################################
#                  Radios en Linea                     #
#########################################################
getwd()
#Instalar paquetes
install.packages("arules", dependencies = T)
install.packages("arulesViz", dependencies = T)
#Llamar librerías
library(arules)
library(arulesViz)
#establecer escritorio
#Leer Data Frame
trans=read.transactions(file = file.choose(),format="single",sep=",",cols=c(1,2),rm.duplicates=TRUE)

#A
#Establecer regla
rules <- apriori(trans, parameter = list(supp = 0.01, conf = 0.5, target = "rules"))
inspect(rules)
plot(rules)

#B
inspect(subset(rules, subset=lift > 5)) 

