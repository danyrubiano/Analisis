library(psych)
library(help = "datasets")
library(modeest)

setwd("C:/Users/Dany/Google Drive/Universidad/Semestre 8/Analisis")
data<-read.csv(header=FALSE,col.names=c("topLeft","topMiddle","topRight","middleLeft","middleMiddle","middleRight","bottomLeft","bottomMiddle","bottomRight","class"),"tic-tac-toe.data")

topLeft<-data$topLeft
topMiddle<-data$topMiddle
topRight<-data$topRight
middleLeft<-data$middleLeft
middleMiddle<-data$middleMiddle
middleRight<-data$middleRight
bottomLeft<-data$bottomLeft
bottomMiddle<-data$bottomMiddle
bottomRight<-data$bottomRight
class<-data$class

atributo<-c("topLeft","topMiddle","topRight","middleLeft","middleMiddle","middleRight","bottomLeft","bottomMiddle","bottomRight")

###########################################################################################

#Funciones para obtener los totales de cada clase
temp<-which(data$class=="positive")
typePositive<-data[temp,]
temp<-which(data$class=="negative")
typeNegative<-data[temp,]

#funciones para obtener las frecuencias de cada clase
fPositive<-nrow(typePositive)
fNegative<-nrow(typeNegative)
total<-nrow(data)

relfPositive<-fPositive/total
relfNegative<-fNegative/total

typeNames<-c("positive","negative")

absFreq<-c(fNegative,fPositive)
relFreq<-c(relfNegative,relfPositive)
freqTable<-matrix(c(absFreq,relFreq),nrow=2,ncol=2)

dimnames(freqTable) = list(c("Positive","Negative"),c("Frecuencia absoluta","Frecuencia relativa"))

#Grafico de barras con la frecuencia absoluta
barplot(c(fPositive,fNegative),space=0.1,names.arg=typeNames,main="Numero de Instancias por Clases",
  xlab="Clase",ylab="Numero de Instancia")


# x| | 
# -----
#  |x| 
# -----
#  | |x
temp<-which(data$topLeft=="x" & data$middleMiddle=="x" & data$bottomRight=="x" & data$class=="positive")
ganadasXDagonal1<-data[temp,]

#  | |x 
# -----
#  |x| 
# -----
# x| |
temp<-which(data$topRight=="x" & data$middleMiddle=="x" & data$bottomLeft=="x" & data$class=="positive")
ganadasXDagonal2<-data[temp,]

# X gana Vertical izquierda
temp<-which(data$topLeft=="x" & data$middleLeft=="x" & data$bottomLeft=="x" & data$class=="positive")
ganadasXVerticalIzq<-data[temp,]

# X gana Vertical medio
temp<-which(data$topMiddle=="x" & data$middleMiddle=="x" & data$bottomMiddle=="x" & data$class=="positive")
ganadasXVerticalMedio<-data[temp,]

# X gana Vertical derecha
temp<-which(data$topRight=="x" & data$middleRight=="x" & data$bottomRight=="x" & data$class=="positive")
ganadasXVerticalDer<-data[temp,]

# X gana Horizontal arriba
temp<-which(data$topLeft=="x" & data$topMiddle=="x" & data$topRight=="x" & data$class=="positive")
ganadasXHorizontalTop<-data[temp,]

# X gana Horizontal medio
temp<-which(data$middleLeft=="x" & data$middleMiddle=="x" & data$middleRight=="x" & data$class=="positive")
ganadasXHorizontalMed<-data[temp,]

# X gana Horizontal abajo
temp<-which(data$bottomLeft=="x" & data$bottomMiddle=="x" & data$bottomRight=="x" & data$class=="positive")
ganadasXHorizontalBottom<-data[temp,]

fXWinDiag1<-nrow(ganadasXDagonal1)
fXWinDiag2<-nrow(ganadasXDagonal2)
fXWinVertIzq<-nrow(ganadasXVerticalIzq)
fXWinVertMed<-nrow(ganadasXVerticalMedio)
fXWinVertDer<-nrow(ganadasXVerticalDer)
fXWinHorTop<-nrow(ganadasXHorizontalTop)
fXWinHorMed<-nrow(ganadasXHorizontalMed)
fXWinHorBot<-nrow(ganadasXHorizontalBottom)

relXWinDiag1<-fXWinDiag1/total
relXWinDiag2<-fXWinDiag2/total
relXWinVertIzq<-fXWinVertIzq/total
relXWinVertMed<-fXWinVertMed/total
relXWinVertDer<-fXWinVertDer/total
relXWinHorTop<-fXWinHorTop/total
relXWinHorMed<-fXWinHorMed/total
relXWinHorBot<-fXWinHorBot/total

typeNames<-c("Diagonal 1","Diagonal 2", "Vertical Izquierda", "Vertical Medio", "Vertical Derecho", "Horizontal Arriba", "Horizontal Medio", "Horizontal Abajo")

absFreq2<-c(fXWinDiag1,fXwinDiag2,fXWinVertIzq,fXWinVertMed,fXWinVertDer,fXWinHorTop,fXWinHorMed,fXWinHorBot)
relFreq2<-c(relXWinDiag1,relXWinDiag2,relXWinVertIzq,relXWinVertMed,relXWinVertDer,relXWinHorTop,relXWinHorMed,relXWinHorBot)
freqTable2<-matrix(c(absFreq2,relFreq2),nrow=8,ncol=2)
dimnames(freqTable2) = list(c("Diagonal 1","Diagonal 2", "Vertical Izquierda", "Vertical Medio", "Vertical Derecho", "Horizontal Arriba", "Horizontal Medio", "Horizontal Abajo"),c("Frecuencia absoluta","Frecuencia relativa"))

#Grafico de barras con la frecuencia absoluta
barplot(c(fXWinDiag1,fXwinDiag2,fXWinVertIzq,fXWinVertMed,fXWinVertDer,fXWinHorTop,fXWinHorMed,fXWinHorBot),space=0.1,names.arg=typeNames,main="Numero de Instancias por Clases",
        xlab="Clase",ylab="Numero de Instancia") 


# O| | 
# -----
#  |O| 
# -----
#  | |O
temp<-which(data$topLeft=="o" & data$middleMiddle=="o" & data$bottomRight=="o" & data$class=="negative")
ganadasODagonal1<-data[temp,]

#  | |O 
# -----
#  |O| 
# -----
# O| |
temp<-which(data$topRight=="o" & data$middleMiddle=="o" & data$bottomLeft=="o" & data$class=="negative")
ganadasODagonal2<-data[temp,]

# O gana Vertical izquierda
temp<-which(data$topLeft=="o" & data$middleLeft=="o" & data$bottomLeft=="o" & data$class=="negative")
ganadasOVerticalIzq<-data[temp,]

# O gana Vertical medio
temp<-which(data$topMiddle=="o" & data$middleMiddle=="o" & data$bottomMiddle=="o" & data$class=="negative")
ganadasOVerticalMedio<-data[temp,]

# O gana Vertical derecha
temp<-which(data$topRight=="o" & data$middleRight=="o" & data$bottomRight=="o" & data$class=="negative")
ganadasOVerticalDer<-data[temp,]

# O gana Horizontal arriba
temp<-which(data$topLeft=="o" & data$topMiddle=="o" & data$topRight=="o" & data$class=="negative")
ganadasOHorizontalTop<-data[temp,]

# O gana Horizontal medio
temp<-which(data$middleLeft=="o" & data$middleMiddle=="o" & data$middleRight=="o" & data$class=="negative")
ganadasOHorizontalMed<-data[temp,]

# O gana Horizontal abajo
temp<-which(data$bottomLeft=="o" & data$bottomMiddle=="o" & data$bottomRight=="o" & data$class=="negative")
ganadasOHorizontalBottom<-data[temp,]

fOWinDiag1<-nrow(ganadasODagonal1)
fOWinDiag2<-nrow(ganadasODagonal2)
fOWinVertIzq<-nrow(ganadasOVerticalIzq)
fOWinVertMed<-nrow(ganadasOVerticalMedio)
fOWinVertDer<-nrow(ganadasOVerticalDer)
fOWinHorTop<-nrow(ganadasOHorizontalTop)
fOWinHorMed<-nrow(ganadasOHorizontalMed)
fOWinHorBot<-nrow(ganadasOHorizontalBottom)

totalGanadosO<-fOWinDiag1+fOWinDiag2+fOWinVertIzq+fOWinVertMed+fOWinVertDer+fOWinHorTop+fOWinHorMed+fOWinHorBot
empates<-total-fPositive-totalGanadosO

relOWinDiag1<-fOWinDiag1/total
relOWinDiag2<-fOWinDiag2/total
relOWinVertIzq<-fOWinVertIzq/total
relOWinVertMed<-fOWinVertMed/total
relOWinVertDer<-fOWinVertDer/total
relOWinHorTop<-fOWinHorTop/total
relOWinHorMed<-fOWinHorMed/total
relOWinHorBot<-fOWinHorBot/total

typeNames<-c("Diagonal 1","Diagonal 2", "Vertical Izquierda", "Vertical Medio", "Vertical Derecho", "Horizontal Arriba", "Horizontal Medio", "Horizontal Abajo")

absFreq3<-c(fOWinDiag1,fOWinDiag2,fOWinVertIzq,fOWinVertMed,fOWinVertDer,fOWinHorTop,fOWinHorMed,fOWinHorBot)
relFreq3<-c(relOWinDiag1,relOWinDiag2,relOWinVertIzq,relOWinVertMed,relOWinVertDer,relOWinHorTop,relOWinHorMed,relOWinHorBot)
freqTable3<-matrix(c(absFreq3,relFreq3),nrow=8,ncol=2)
dimnames(freqTable3) = list(c("Diagonal 1","Diagonal 2", "Vertical Izquierda", "Vertical Medio", "Vertical Derecho", "Horizontal Arriba", "Horizontal Medio", "Horizontal Abajo"),c("Frecuencia absoluta","Frecuencia relativa"))

#Grafico de barras con la frecuencia absoluta
barplot(c(fOWinDiag1,fOWinDiag2,fOWinVertIzq,fOWinVertMed,fOWinVertDer,fOWinHorTop,fOWinHorMed,fOWinHorBot),space=1,names.arg=typeNames,main="Numero de Instancias por Clases",
        xlab="Clase",ylab="Numero de Instancia") 

temp<-which(data$middleLeft=="o" & data$middleMiddle=="o" & data$middleRight=="o" & data$class=="negative")
ganadasOHorizontalMed<-data[temp,]

namesResultados<-c("Ganadas por x","Ganadas por o","Empates")

## Resultados totales
barplot(c(fPositive, totalGanadosO, empates), space=0.1,names.arg=namesResultados, main="Resultados Totales", xlab="Resultados", ylab="Numero de Jugadas")
