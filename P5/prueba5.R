library(parallel)
library(distr)
D <- data.frame(tam = numeric(), valor = numeric())
DT <- data.frame(tam = numeric(), valor = numeric())
DE <- data.frame(tam = numeric(), valor = numeric())

names(D) = c("n", "e")
names(DT) = c("n", "e")
names(DE) = c("n", "e")


rep <- 5
known = 0.0488341111
error <-function(x){return(abs(x-known))}
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
g <- function(x) { return((2 / pi) * f(x)) }
generador <- r(AbscontDistribution(d = g)) # creamos un generador
desde <- 3
hasta <- 7
cuantos <- 200
parte <- function(z) {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}
cluster<-makeCluster(detectCores() - 1)
clusterExport(cluster,"generador")
clusterExport(cluster,"parte")
clusterExport(cluster,"desde")
clusterExport(cluster,"hasta")
for(p in seq(10, 20, 2)){
  print(p)
  pedazo=2**p
  clusterExport(cluster,"pedazo")
  datos<-numeric()
  datostiempo<-numeric()
  datoserror<-numeric()
  for (l in 1:rep){
    tiempo<-system.time(integral <- sum(parSapply(cluster, 1:cuantos, parte))/(cuantos * pedazo))
    integral <- (pi/2)*integral
    eror <- error(integral)
    
    datos = c(datos, integral)
    datostiempo=c(datostiempo,tiempo[3])
    datoserror=c(datoserror,eror)
  }
  
  D = rbind(D, c(2**p,datos))
  names(D) = c("n", "e")
  DT = rbind(DT, c(2**p, datostiempo))
  names(DT) = c("n", "e")
  DE = rbind(DE, c(2**p, datoserror))
  names(DE) = c("n", "e")
}
stopCluster(cluster)
ap <-t(D[ ,2:rep+1])
ti <-t(DT[ , 2:rep+1])
er <-t(DE[ , 2:rep+1])

png("aprox.png")
boxplot(ap,names= c(expression(2^10), expression(2^12), expression(2^14), expression(2^16), expression(2^18), expression(2^20)))
abline(h=0.0488341111) 
graphics.off()

png("tiempos.png")
boxplot(ti,names= c(expression(2^10), expression(2^12), expression(2^14), expression(2^16), expression(2^18), expression(2^20)))
graphics.off()

png("error.png")
boxplot(er,names= c(expression(2^10), expression(2^12), expression(2^14), expression(2^16), expression(2^18), expression(2^20)))
graphics.off()

write.csv(ap,file="datos.csv")
write.csv(ti,file="tiempos.csv")
write.csv(er,file="error.csv")