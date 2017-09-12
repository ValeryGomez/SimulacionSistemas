library(parallel)
library(distr)
D <- data.frame(tam = numeric(), valor = numeric())
DT <- data.frame(tam = numeric(), valor = numeric())
DE <- data.frame(tam = numeric(), valor = numeric())

names(D) = c("n", "e")
names(DT) = c("n", "e")
names(DE) = c("n", "e")


rep <- 20
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
ap <-t(D[ ,2:21])
ti <-t(DT[ , 2:21])
er <-t(DE[ , 2:21])

png("aprox.png")
boxplot(ap,names= c("2^10", "2^12", "2^14", "2^16", "2^18", "2^20", "2^22", "2^24", "2^26", "2^28", "2^30"))
abline(h=0.0488341111) 
graphics.off()

png("tiempos.png")
boxplot(ti,names= c("2^10", "2^12", "2^14", "2^16", "2^18", "2^20", "2^22", "2^24", "2^26", "2^28", "2^30"))
graphics.off()

png("error.png")
boxplot(er,names= c("2^10", "2^12", "2^14", "2^16", "2^18", "2^20", "2^22", "2^24", "2^26", "2^28", "2^30"))
graphics.off()



#png("aprox.png", width = 1000, height = 1000)
#boxplot(as.numeric(D[1,2:21]), as.numeric(D[2,2:21]), 
#        as.numeric(D[3,2:21]), as.numeric(D[4,2:21]), 
#        as.numeric(D[5,2:21]), as.numeric(D[6,2:21]),
#        as.numeric(D[7,2:21]), as.numeric(D[8,2:21]), 
#        as.numeric(D[9,2:21]), as.numeric(D[10,2:21]), 
#        as.numeric(D[11,2:21]),
#        names=c("2^10", "2^12", "2^14", "2^16", "2^18", "2^20", "2^22", "2^24", "2^26", "2^28", "2^30"))
#graphics.off()
#png("tiempo.png")
#boxplot(as.numeric(DT[1,2:21]), as.numeric(DT[2,2:21]), 
#        as.numeric(DT[3,2:21]), as.numeric(DT[4,2:21]), 
#        as.numeric(DT[5,2:21]), as.numeric(DT[6,2:21]),
#        as.numeric(DT[7,2:21]), as.numeric(DT[8,2:21]), 
#        as.numeric(DT[9,2:21]), as.numeric(DT[10,2:21]), 
#        as.numeric(DT[11,2:21]),
#        names=c("2^10", "2^12", "2^14", "2^16", "2^18", "2^20", "2^22", "2^24", "2^26", "2^28", "2^30"),
#        ylim=c(0,.01))
#graphics.off()