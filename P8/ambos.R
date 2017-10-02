library('ggplot2')
pa <- as.data.frame(read.csv("tiempos.csv"))
sec <-as.data.frame(read.csv("Stiempos.csv"))
ambos<- data.frame()
ambos<-rbind(pa,sec)
ambos$k <- as.factor(ambos$k)
png(paste("ambos.png", sep=""), width=700, height=700)
ggplot(data=ambos,aes(x=k,y=Tiempos,fill=Tipo))+geom_boxplot()#stat_summary(fun.y=mean,geom="smooth",aes(group=Tipo,col=Tipo))
#boxplot(pa$Tiempos~pa$k, color="red")
graphics.off()

####
png(paste("Vnoparalelo.png", sep=""), width=700, height=700)
boxplot(sec$Tiempos~sec$k)
graphics.off()
####
png(paste("Vparalelo.png", sep=""), width=700, height=700)
boxplot(pa$Tiempos~pa$k)
graphics.off()
###



################Prueba Estadistica##################
wilcox.test(ambos$Tiempos[ambos$Tipo=="Paralelo"],ambos$Tiempos[ambos$Tipo=="NoParalelo"] )
median(ambos$Tiempos[ambos$Tipo=="Paralelo"])
median(ambos$Tiempos[ambos$Tipo=="NoParalelo"])
