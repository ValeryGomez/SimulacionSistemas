library('ggplot2')
pa <- as.data.frame(read.csv("TiemposNoP.csv"))
sec <-as.data.frame(read.csv("TiemposP.csv"))
ambos<- data.frame()
ambos<-rbind(pa,sec)
ambos$Población <- as.factor(ambos$Población)
png(paste("ambos.png", sep=""), width=700, height=700)
ggplot(data=ambos,aes(x=Población,y=Tiempos,fill=Tipo))+geom_boxplot()+ylab("Tiempos (s)")#stat_summary(fun.y=mean,geom="smooth",aes(group=Tipo,col=Tipo))
#boxplot(pa$Tiempos~pa$k, color="red")
graphics.off()

####
png(paste("Vnoparalelo.png", sep=""), width=700, height=700)
boxplot(sec$Tiempos~sec$Población)
graphics.off()
####
png(paste("Vparalelo.png", sep=""), width=700, height=700)
boxplot(pa$Tiempos~pa$Población)
graphics.off()
###
