repetir <- 100
dd <- c(500,800,2000,10000)
eucl <- TRUE
dimm <- 8
library('ggplot2')


datos <- data.frame()
graficas<- data.frame()
porcientos <- c()
tiempos <- data.frame()
for (l in 1:length(dd)){
  duracion <- dd[l]
  for (dimension in 1:dimm) {
    for(r in 1:repetir){
        c <- Sys.time()
        pos <- rep(0, dimension)
        mayor <- 0
        centro <- 0
        for (t in 1:duracion) {
          resultado <- c()
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
        resultado <- c(duracion,dimension,mayor,tie)
    datos <- rbind(datos, resultado)
  }
  }
}

graff <- data.frame()
la <- 1
valores <- c()
colnames(datos)=(c("Pasos","Dimension", "Valor M", "Tiempo"))
#graficas <- t(graficas)
datos$Pasos <- as.factor(datos$Pasos)
datos$Dimension <- as.factor(datos$Dimension)

if (eucl) {
  png(paste("TiemposEuN.png", sep=""), width=700, height=700)
  ggplot(data=datos,aes(x=Dimension,y=Tiempo,fill=Dimension))+geom_boxplot()+xlab("Dimensi\u{F3}n")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
  graphics.off()
  ###Largo de la caminata##
  png(paste("LargoEuN.png", sep=""), width=700, height=700)
  ggplot(data=datos,aes(x=Pasos,y=Tiempo,fill=Dimension))+geom_boxplot()+xlab("Largo de la Caminata")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
  graphics.off()
  #Dimensi??n de la caminata##
  png(paste("DimEuN.png", sep=""), width=700, height=700)
  ggplot(data=datos,aes(x=Dimension,y=Tiempo))+geom_boxplot()+xlab("Dimensi\u{F3}n")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
  graphics.off()
  write.csv(datos,file = "datosTiemposNoParEu.csv")
} else {
  png(paste("TiemposMaN.png", sep=""), width=700, height=700)
  ggplot(data=datos,aes(x=Dimension,y=Tiempo,fill=Pasos))+geom_boxplot()+xlab("Dimensi\u{F3}n (100 Rep)")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
  graphics.off()
  ###Largo de la caminata##
  png(paste("LargoMaN.png", sep=""), width=700, height=700)
  ggplot(data=datos,aes(x=Pasos,y=Tiempo,fill=Dimension))+geom_boxplot()+xlab("Largo de la Caminata")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
  graphics.off()
  #Dimensi??n de la caminata##
  png(paste("DimMaN.png", sep=""), width=700, height=700)
  ggplot(data=datos,aes(x=Dimension,y=Tiempo))+geom_boxplot()+xlab("Dimensi\u{F3}n")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
  graphics.off()
  write.csv(datos,file = "datosTiemposNoParMa.csv")
  
}
