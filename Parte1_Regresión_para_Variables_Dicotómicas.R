require(MASS)
####################################################################
# Regresión Logística Múltiple                                     #
####################################################################

####################################################################
# # Ejemplo: Keiko Primera Vuelta 2016                           # #
####################################################################
#Lectura de Datos
keiko <- read.delim("clipboard")
head(keiko)
plot(keiko)
# Verificar si las variables categóricas son factores
sapply (keiko, class)
# Convertir a factor
keiko$Presencia16 <- factor(keiko$Presencia16)
keiko$Exito16 <- factor(keiko$Exito16)

str(keiko)
# Estimacion
keiko.fit16 <- glm(Exito16~. , family = binomial(link = "probit"),data=keiko)
summary(keiko.fit16)

confint(keiko.fit16)


################################################################################
#Interpretación:
# - Para cada cambio unitario en gre, el logaritmo de la ventaja de ser admitido
#   (versus no ser admitido) se incrementa en 0.002.
# - Para un cambio unitario en gpa, el logaritmo de la ventaja de ser admitido 
#   a la escuela de postgrado se incrementa en 0.804.
#   Las variables indicadoras de rank tiene una interpretación distinta. 
#   Por ejemplo, si el estudiante estudió en pregrado en un institución educativa
#   de rango 2, el logaritmo de las ventaja disminuye en -0.675 en comparación 
#   con los que asistieron a una institución de rango 1.
################################################################################

ypred <- as.numeric((predict(keiko.fit16, newdata=keiko[,-3], type="response") >= 0.3) )
ytrue <- keiko$Exito16

# Matriz de confusion
table(ypred,ytrue)

#Error
testerr <- mean(ypred!=ytrue)
testerr

error1.prop<-prop.table(table(ypred,keiko$Exito16),2)
error1.prop
# Sensitividad
error1.prop[2,2]

# Especificidad
error1.prop[1,1]

## Curvas ROC
library(ROCR)
keiko.log.predict<-prediction(ypred,keiko$Exito16)
keiko.log.auc <- performance(keiko.log.predict,"tpr","fpr") #True y False postivie.rate
# Area bajo la curva ROC


# GRAFICO CURVA ROC
#------------------------------------------------------------------------------
auc <- as.numeric(performance(keiko.log.predict ,"auc")@y.values)
plot(keiko.log.auc,type='o', main = paste('Area Bajo la Curva =',round(auc,2)))  
abline(a=0, b= 1)



# Intervalos de Confianza para los coeficientes
confint(keiko.fit16)

## solo tasa de ventajas
exp(coef(keiko.fit16))

## tasa de ventajas e IC 95% 
exp(cbind(OR = coef(keiko.fit16), confint(keiko.fit16)))

################################################################################
#Interpretación:
# De los resultados es posible afirmar que ante un cambio unitario en gpa,
# la ventaja de ser admitido en la escuela de postgrado (frente a que no ser admitido)
# se incrementa en un factor de 2.23.
################################################################################

# Calidad de Ajuste (Prueba de Hosmer y Lemeshow)
library(ResourceSelection)
hl <- hoslem.test(keiko.fit16$y, fitted(keiko.fit16), g=10)

hl
cbind(hl$observed,hl$expected)






# Diagnóstico
source.with.encoding('envel.bino.R', encoding='ISO-8859-1')
envel.bino(keiko.fit16)
