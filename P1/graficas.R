library('ggplot2')
parEu <- as.data.frame(read.csv("datosTiemposParEu.csv"))
Tipo <- c(rep("Paralelo",3200))
parEu <- cbind(parEu,Tipo)
noparEu <-as.data.frame(read.csv("datosTiemposNoParEu.csv"))
Tipo <- c(rep("NoParalelo",3200))
noparEu <- cbind(noparEu,Tipo)
parMa <- as.data.frame(read.csv("datosTiemposParMa.csv"))
Tipo <- c(rep("Paralelo",3200))
parMa <- cbind(parMa,Tipo)
noparMa <-as.data.frame(read.csv("datosTiemposNoParMa.csv"))
Tipo <- c(rep("NoParalelo",3200))
noparMa <- cbind(noparMa,Tipo)

ambosEu<-rbind(parEu,noparEu)
ambosMa<-rbind(parMa,noparMa)

parEu$Pasos <- as.factor(parEu$Pasos)
parEu$Dimension <- as.factor(parEu$Dimension)
noparEu$Pasos <- as.factor(noparEu$Pasos)
noparEu$Dimension <- as.factor(noparEu$Dimension)
parMa$Pasos <- as.factor(parMa$Pasos)
parMa$Dimension <- as.factor(parMa$Dimension)
noparMa$Pasos <- as.factor(noparMa$Pasos)
noparMa$Dimension <- as.factor(noparMa$Dimension)


#????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
#Prueba Estad??stica
shapiro.test(ambos$Diferencia)
wilcox.test(ambos$Diferencia[ambos$Tipo=="Probabilidad"],ambos$Diferencia[ambos$Tipo=="Normal"] )
median(ambos$Diferencia[ambos$Tipo=="Probabilidad"])
median(ambos$Diferencia[ambos$Tipo=="Normal"])

###Largo de la caminata##
png(paste("LargoR2Eu.png", sep=""), width=700, height=700)
ggplot(data=ambosEu,aes(x=Pasos,y=Tiempo, fill= Tipo))+geom_boxplot()+xlab("Largo de la Caminata")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
graphics.off()
png(paste("LargoR2Ma.png", sep=""), width=700, height=700)
ggplot(data=ambosMa,aes(x=Pasos,y=Tiempo, fill= Tipo))+geom_boxplot()+xlab("Largo de la Caminata")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
graphics.off()
#Dimensi??n de la caminata##
png(paste("DimR2Eu.png", sep=""), width=700, height=700)
ggplot(data=ambosEu,aes(x=Dimension,y=Tiempo, fill=Tipo))+geom_boxplot()+xlab("Dimensi\u{F3}n")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
graphics.off()
png(paste("DimR2Ma.png", sep=""), width=700, height=700)
ggplot(data=ambosMa,aes(x=Dimension,y=Tiempo, fill=Tipo))+geom_boxplot()+xlab("Dimensi\u{F3}n")+ ylab("Tiempo de ejecuci\u{F3}n (s)")
graphics.off()