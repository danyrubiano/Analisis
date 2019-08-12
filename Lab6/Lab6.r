setwd("C:/Users/Dany/Google Drive/Universidad/Semestre 8/Analisis/Lab6")

Normocapnia<-read.table("SH000.txt", col.names=c("PAM","CO2", "VFSC"))
Hipercapnia<-read.table("SH001.txt", col.names=c("PAM","CO2", "VFSC"))

pam_normocapnia<-ts(Normocapnia$PAM)
pam_hipercapnia<-ts(Hipercapnia$PAM)

vfsc_normocapnia<-ts(Normocapnia$VFSC)
vfsc_hipercapnia<-(Hipercapnia$VFSC)

correlacion_normocapnia<-ccf(pam_normocapnia,vfsc_normocapnia)
correlacion_hipercapnia<-ccf(pam_hipercapnia,vfsc_hipercapnia)

correlacion_normocapnia
correlacion_hipercapnia

autocorrelacion_normocapnia<-acf(pam_normocapnia)
autocorrelacion_hipercapnia<-acf(pam_hipercapnia)

autocorrelacion_hipercapnia
autocorrelacion_normocapnia

modelo<-arima(pam_normocapnia, order = c(1,0,0))
summary(modelo)
acf(modelo$residuals)
modelo$coef
