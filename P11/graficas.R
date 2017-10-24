library('ggplot2')
pa <- as.data.frame(read.csv("tiemposno.csv"))
sec <-as.data.frame(read.csv("tiempos.csv"))
por<-as.data.frame(read.csv("porciento200.csv"))
ambos<- data.frame()
ambos<-rbind(pa,sec)
ambos$Poblacion <- as.factor(ambos$Poblacion)
#png(paste("ambos.png", sep=""), width=700, height=700)
#ggplot(data=ambos,aes(x=Poblacion,y=Tiempo,fill=Tipo))+geom_boxplot()+ylab("Tiempos (s)")#stat_summary(fun.y=mean,geom="smooth",aes(group=Tipo,col=Tipo))
#boxplot(pa$Tiempos~pa$k, color="red")
#graphics.off()

png(paste("porcentaje.png", sep=""), width=700, height=700)
por$Funciones <- as.factor(por$Funciones)
ggplot(data=por,aes(x=Funciones,y=Dominadores,fill=Funciones)) + geom_violin(fill="orange", color="red", scale = "width") + geom_boxplot(width=0.2, fill="darkgreen", color="white", lwd=2) +
  xlab("Cantidad de funciones") +
  ylab("Frecuencia") +theme(axis.text=element_text(size=15),axis.title=element_text(size=15,face="bold"))
graphics.off()