repetir <- 300
dd <- c(50,80,150,200,400)
eucl <- TRUE
dimm <- 8
library(parallel)
library('ggplot2')
cluster <- makeCluster(detectCores() - 1)

datos <- data.frame()
graficas<- data.frame()
porcientos <- c()
for (l in 1:length(dd)){
  duracion <- dd[l]
  clusterExport(cluster, "duracion")
  clusterExport(cluster, "eucl")
for (dimension in 1:dimm) {
  clusterExport(cluster, "dimension")
  resultado <- parSapply(cluster, 1:repetir,
                         function(r) {
                           pos <- rep(0, dimension)
                           mayor <- 0
                           centro <- 0
                           for (t in 1:duracion) {
                             cambiar <- sample(1:dimension, 1)
                             cambio <- 1
                              
                             if (runif(1) < 0.5) {
                               cambio <- -1
                             }
                             pos[cambiar] <- pos[cambiar] + cambio
                             if (eucl) {
                               d <- sum(sqrt(pos**2))
                             } else { # Manhattan
                               d <- sum(abs(pos))
                             }
                             if (d > mayor) {
                               mayor <- d
                             }
                             if(all(pos==0))
                               centro <- centro + 1
                           }
                           todo <- c(duracion,dimension,mayor,centro)
                           return(todo)
                         })
  datos <- rbind(datos, resultado)
}
  
}
stopCluster(cluster)

datos<- t(datos)
graff <- data.frame()
write.csv(datos,file = "datos.csv")
la <- 1
valores <- c()
colnames(datos)= (rep(c("Pasos","Dimension", "Valor M", "Origen"),dimm*length(dd)))
for(i in 1:(dimm*length(dd))){
  graff <- datos[,la:(la+3)]
  graficas <- rbind(graficas,graff)
  la <- la + 4
    porcentaje<- sum(datos[,la-1])/repetir
    porcientos <- c(porcientos, porcentaje)
}
colnames(graficas)= c("Pasos","Dimension", "Valor M", "Origen")
graficas$Pasos <- as.factor(graficas$Pasos)
graficas$Dimension <- as.factor(graficas$Dimension)

if (eucl) {
  png(paste("PorcentajesEu300.png", sep=""), width=700, height=700)
  ggplot(data=graficas,aes(x=Dimension,y=Origen,fill=Pasos))+geom_boxplot()+xlab("Dimensi\u{F3}n (300 Rep)")+ ylab("Cantidad de llegadas al Origen")
  graphics.off()
  write.csv(graficas,file = "datos300.csv")
} else {
  png(paste("PorcentajesMa300.png", sep=""), width=700, height=700)
  ggplot(data=graficas,aes(x=Dimension,y=Origen,fill=Pasos))+geom_boxplot()+xlab("Dimensi\u{F3}n (100 Rep)")+ ylab("Cantidad de llegadas al Origen")
  graphics.off()
  write.csv(graficas,file = "datos300.csv")
}
v <- 0
porcentaje <- data.frame()
for(vi in 1:length(dd)){
  porcentaje <- rbind(porcentaje,porcientos[v+1:(v+dimm)])
  v <- v + 8
}
colnames(porcentaje)= seq(1:8)
row.names(porcentaje)= dd
#png(paste("PorcMa.png", sep=""), width=700, height=700)
#plot(porcentaje[1,], col="purple", type="d")
#for(la in 2:length(dd)){
#  lines(porcentaje[la,], type = "l", col=sample(rainbow(10)))
#}
#graphics.off()


