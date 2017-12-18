repetir <- 100
dd <- c(50,80,150,200)
eucl <- TRUE
dimm <- 8
library(parallel)
library('ggplot2')
cluster <- makeCluster(detectCores() - 1)

datos <- data.frame()
graficas<- data.frame()
porcientos <- c()
tiempos <- data.frame()
for (l in 1:length(dd)){
  duracion <- dd[l]
  clusterExport(cluster, "duracion")
  clusterExport(cluster, "eucl")
  for (dimension in 1:dimm) {
    clusterExport(cluster, "dimension")
    resultado <- parSapply(cluster, 1:repetir,
                           function(r) {
                             c <- Sys.time()
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
                             d <- Sys.time()
                             ti <- c(c,d)
                             tie <- diff(ti,units="secs")
                             todo <- c(duracion,dimension,mayor,tie)
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
colnames(datos)= (rep(c("Pasos","Dimension", "Valor M", "Tiempo"),dimm*length(dd)))
for(i in 1:(dimm*length(dd))){
  graff <- datos[,la:(la+3)]
  graficas <- rbind(graficas,graff)
  la <- la + 4
  porcentaje<- sum(datos[,la-1])/repetir
  porcientos <- c(porcientos, porcentaje)
}
colnames(graficas)= c("Pasos","Dimension", "Valor M", "Tiempo")
#graficas <- t(graficas)
graficas$Pasos <- as.factor(graficas$Pasos)
graficas$Dimension <- as.factor(graficas$Dimension)

if (eucl) {
  #  png("p1er.png")
  #  boxplot(data.matrix(graficas), use.cols=FALSE, 
  #          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima", 
  #          main="Euclideana")
  png(paste("TiemposEu.png", sep=""), width=700, height=700)
  ggplot(data=graficas,aes(x=Dimension,y=Tiempo,fill=Pasos))+geom_boxplot()+xlab("Dimensi\u{F3}n (100 Rep)")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
  graphics.off()
  ###Largo de la caminata##
  png(paste("LargoEu.png", sep=""), width=700, height=700)
  ggplot(data=graficas,aes(x=Pasos,y=Tiempo,fill=Dimension))+geom_boxplot()+xlab("Largo de la Caminata")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
  graphics.off()
  #Dimensi??n de la caminata##
  png(paste("DimEu.png", sep=""), width=700, height=700)
  ggplot(data=graficas,aes(x=Dimension,y=Tiempo,fill=Pasos))+geom_boxplot()+xlab("Dimensi\u{F3}n")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
  graphics.off()
  
  
} else {
  #  png("p1mr.png")
  #  boxplot(data.matrix(graficas), use.cols=FALSE, 
  #          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima", 
  #          main="Manhattan")
  png(paste("TiemposMa.png", sep=""), width=700, height=700)
  ggplot(data=graficas,aes(x=Dimension,y=Tiempo,fill=Pasos))+geom_boxplot()+xlab("Dimensi\u{F3}n (100 Rep)")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
  graphics.off()
}
