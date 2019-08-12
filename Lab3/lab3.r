setwd("C:/Users/Dany/Google Drive/Universidad/Semestre 8/Analisis")

DB <- read.table("tic-tac-toe.data", header=FALSE, sep=",", 
                 col.names=c("p1","p2","p3","p4","p5","p6","p7","p8","p9","class"))

#Numerizacion

DB$p1<-as.numeric(DB$p1)
DB$p2<-as.numeric(DB$p2)
DB$p3<-as.numeric(DB$p3)
DB$p4<-as.numeric(DB$p4)
DB$p5<-as.numeric(DB$p5)
DB$p6<-as.numeric(DB$p6)
DB$p7<-as.numeric(DB$p7)
DB$p8<-as.numeric(DB$p8)
DB$p9<-as.numeric(DB$p9)
DB$class<-as.numeric(DB$class)

# Arreglo de la numerizacion
# b de 1 pasa a 0
# o de 2 pasa a 1
# x de 3 pasa a 2
# negative de 1 pasa a 0
# positive de 2 pasa a 1. Para poder aplicar glm sobre class.
DB[DB==1]<-0
DB[DB==2]<-1
DB[DB==3]<-2

########################################################################
#Modelos de Regresion Logística y odss ratios

reg1 <- glm(class ~., data = DB, family = binomial(link="logit"))
summary(reg1)
odsr1 <- exp(reg1$coefficients)
print(odsr1)

reg2 <- glm(class~p1+p3+p5+p7+p9, DB, family = binomial(link = "logit"))
summary(reg2)
odsr2 <- exp(reg2$coefficients)
print(odsr2)

reg3<- glm(class~p5, DB,family= binomial(link="logit"))
summary(reg3)
odsr3 <- exp(reg3$coefficients)
print(odsr3)

reg4 <- glm(class~p1+p3+p7+p9, DB, family = binomial(link = "logit"))
summary(reg4)
odsr4 <- exp(reg4$coefficients)
print(odsr4)


#Analisis de varianza, test de Chi-Cuadrado
anav <-anova(reg1,reg2,reg3,reg4,test = "Chisq")

#AIC
aic <-AIC(reg1,reg2,reg3,reg4)

########################################################################
#Curvas ROC

library(pROC)
prob <- predict(reg1,type=c("response"))
curva1 <- roc(class~prob, data = DB)
plot(curva1, col="red",  main="Curva ROC modelo 1")


prob <- predict(reg2,type=c("response"))
curva2 <- roc(class~prob, data = DB)
plot(curva2, col="green",  main="Curva ROC modelo 2")


prob <- predict(reg3,type=c("response"))
curva3 <- roc(class~prob, data = DB)
plot(curva3, col="blue",  main="Curva ROC modelo 3")

prob <- predict(reg4,type=c("response"))
curva4 <- roc(class~prob, data = DB)
plot(curva4, col="yellow",  main="Curva ROC modelo 4")

########################################################################

prob <- predict(reg1,type=c("response"))
curva1 <- roc(class~prob, data = DB)
plot(curva1, col="red",  main="Curvas ROC combinadas")


prob <- predict(reg2,type=c("response"))
curva2 <- roc(class~prob, data = DB)
plot(curva2, col="green",  add=TRUE)


prob <- predict(reg3,type=c("response"))
curva3 <- roc(class~prob, data = DB)
plot(curva3, col="blue",  add=TRUE)

prob <- predict(reg4,type=c("response"))
curva4 <- roc(class~prob, data = DB)
plot(curva4, col="yellow",  add=TRUE)





