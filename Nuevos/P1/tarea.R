repetir <- 100
dd <- c(200,500,700)
eucl <- TRUE
dimm <- 8
library(parallel)

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "duracion")
clusterExport(cluster, "eucl")
datos <- data.frame()
graficas<- data.frame()
porcientos <- c()
for (l in 1:length(dd)){
  duracion <- dd[l]
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
                           todo <- c(mayor,centro)
                           return(todo)
                         })
  datos <- rbind(datos, resultado)
}
}
stopCluster(cluster)

datos<- t(datos)
write.csv(datos,file = "datos.csv")

for(i in 1:(2*dimm)){
  if(i%%2==0){
    porcentaje<- sum(datos[,i])/repetir
    porcientos <- c(porcientos, porcentaje)
  }else{
    graficas <- rbind(graficas, datos[,i])
  } 
}


if (eucl) {
  png("p1er.png")
  boxplot(data.matrix(graficas), use.cols=FALSE, 
          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima", 
          main="Euclideana")
} else {
  png("p1mr.png")
  boxplot(data.matrix(graficas), use.cols=FALSE, 
          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima", 
          main="Manhattan")
}
graphics.off()


