setwd("C:/Users/Dany/Google Drive/Universidad/Semestre 8/Analisis")
DB <- read.table("tic-tac-toe.data", header=FALSE, sep=",", 
                 col.names=c("p1","p2","p3","p4","p5","p6","p7","p8","p9","class"))

library(arulesViz)
library(arules)

reglas = apriori(DB, parameter=list(support=0.1, confidence=0.3))

reglasPositive<-subset(reglas, subset= rhs %in% "class=positive")
summary(reglasPositive)
inspect(reglasPositive)
plot(reglasPositive, main= "Distribución de las reglas Positive")

reglasNegative<-subset(reglas, subset= rhs %in% "class=negative")
summary(reglasNegative)
inspect(reglasNegative)
plot(reglasNegative, main= "Distribución de las reglas Negative")

############################################################################
#Reglas Positive

reglaPos1 = head(sort(reglasPositive, by="lift",decreasing=TRUE),10)
inforegla1Pos<-head(quality(reglaPos1),10)

reglaPos2 = head(sort(reglasPositive, by="support",decreasing=TRUE),10)
inforegla2Pos<-head(quality(reglaPos2),10)

reglaPos3 = head(sort(reglasPositive, by="confidence",decreasing=TRUE),10)
inforegla3Pos<-head(quality(reglaPos3),10)

summary(reglaPos2) #Positives


subreglasPos = reglasPositive[quality(reglasPositive)$confidence>0.65 & 
                                quality(reglasPositive)$support >0.23 & 
                                quality(reglasPositive)$lift >1]

summary(subreglasPos)
inspect(subreglasPos)
plot(subreglasPos, main= "Distribución de las reglas")
subreglas_ordenadasPos <- head(sort(subreglasPos, by="support"), 10)
plot(subreglas_ordenadasPos, method="graph")

###########################################################################
#Reglas Negative

reglaNeg1 = head(sort(reglasNegative, by="lift",decreasing=TRUE),10)
inforegla1Neg<-head(quality(reglaNeg1),10)

reglaNeg2 = sort(reglasNegative, by="support",decreasing=TRUE)
inforegla2Neg<-head(quality(reglaNeg2),10)

reglaNeg3 = head(sort(reglasNegative, by="confidence",decreasing=TRUE),10)
inforegla3Neg<-head(quality(reglaNeg3),10)

summary(reglaNeg2) #Negatives

reglaPos1 = head(sort(reglasPositive, by="lift",decreasing=TRUE),10)
inforegla1Pos<-head(quality(reglaPos1),10)

subreglasNeg = reglasNegative[quality(reglasNegative)$confidence>0.34 & 
                                quality(reglasNegative)$support >0.15 & 
                                quality(reglasNegative)$lift >1]

summary(subreglasNeg)
inspect(subreglasNeg)
plot(subreglasNeg, main= "Distribución de las reglas")
subreglas_ordenadasNeg <- head(sort(subreglasNeg, by="support"), 10)
plot(subreglas_ordenadasNeg, method="graph")


# Reglas Juntas, a modo de comparación
reglasJuntas <- c(subreglas_ordenadasNeg,subreglas_ordenadasPos)
plot(reglasJuntas, method="graph")
