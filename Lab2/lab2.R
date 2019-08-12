setwd("C:/Users/Dany/Google Drive/Universidad/Semestre 8/Analisis")

DB2 <- read.table("tic-tac-toe.data", header=FALSE, sep=",")
#INSTALAR LA PRIMERA VEZ
#install.packages(cluster)
#install.packages('e1071', dependencies=TRUE)
library(e1071)
library(fpc)
library(cluster)

# Eliminacion de registros con datos perdidos
DB2<-DB2[complete.cases(DB2), ]

DB <- DB2[,1:9]


# http://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters

num <- numeric(10)
# Note that "k=1" won’t work!
for (k in 2:10)
  num[k] <- pam(DB, k, metric='Hamming') $ silinfo $ avg.width

k.best <- which.max(num)

cat("silhouette-optimal number of clusters:", k.best, "\n")

plot(1:10, num, type= "h",
    main = "Clustering assessment",
    
    xlab= "k (numero de clusters)",
    
    ylab = "average silhouette width")

axis(1, k.best, paste("MejorK",k.best,sep="\n"),
     
     col = "blue", col.axis = "blue")
     

grupos <- pam(DB,5, diss = FALSE, metric = 'Hamming')
gClusinfo <- grupos$clusinfo
gSilinfo <- grupos$silinfo
gDiss <- grupos$diss
clusplot(grupos,color=TRUE,main= "k=5 con distancia de Hamming")
plot(1:5,grupos$clusinfo[,1], main= "Tama�o de los clusters", xlab="Número de Clúster", ylab = "Cantidad de datos", type = 'b', col =c("red","green","black","blue","brown"))
plot(grupos$clustering, main = "Ubicacion de cada dato por clúster", ylab="Clúster", xlab="Índice", col =c("red","green","grey","blue","brown"))
tabla_avg_silueta<-matrix(gSilinfo$clus.avg.widths)
