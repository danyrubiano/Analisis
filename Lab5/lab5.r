setwd("C:/Users/Dany/Google Drive/Universidad/Semestre 8/Analisis")
library(C50)
library(rpart)
library(rpart.plot)
DB <- read.table("Lab5/tic-tac-toe.data", header=FALSE, sep=",", col.names=c("p1","p2","p3","p4","p5","p6","p7","p8","p9","class"))

#arbol1<- C50::C5.0(class ~., data = DB)
#summary(arbol1)

#arbol2<- C50::C5.0(class ~., data = DB,rules = TRUE)
#summary(arbol2)


#http://apuntes-r.blogspot.cl/2014/09/predecir-perdida-de-clientes-con-arbol.html
training    <- DB
test        <- DB

Arbol1<-rpart(class ~ .,data=training,parms=list(split="information"),method = "class", 
                   control = rpart.control(cp = 0.01))
                   
Prediccion <- predict(Arbol1, test,type="class")
MC         <- table(test[, "class"], Prediccion)
print(MC)

par(mar = rep(2, 4))
rpart.plot(Arbol1, type=1, extra=100,cex = .7,box.col=c("gray99", "gray88")[Arbol1$frame$yval])

#summary(Arbol1)
