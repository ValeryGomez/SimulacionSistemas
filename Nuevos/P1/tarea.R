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
colnames(datos)= rep(seq(1:4),dimm*length(dd))
for(i in 1:(dimm*length(dd)-1)){
  graff <- datos[,la:(la+3)]
  graficas <- rbind(graficas,graff)
  la <- la + 4
    porcentaje<- sum(datos[,la-1])/repetir
    porcientos <- c(porcientos, porcentaje)
}
colnames(graficas)= c("Pasos","Dimension", "Valor M", "Origen")
#graficas <- t(graficas)
graficas$Dimension <- as.factor(graficas$Dimension)

#png(paste("Porcentajes.png", sep=""), width=700, height=700)
ggplot(data=graficas,aes(x=Dimension,y=Origen,fill=Pasos))+geom_boxplot()+ylab("Cantidad de llegadas al Origen")

#if (eucl) {
#  png("p1er.png")
#  boxplot(data.matrix(graficas), use.cols=FALSE, 
#          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima", 
#          main="Euclideana")
#} else {
#  png("p1mr.png")
#  boxplot(data.matrix(graficas), use.cols=FALSE, 
#          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima", 
#          main="Manhattan")
#}
#graphics.off()


