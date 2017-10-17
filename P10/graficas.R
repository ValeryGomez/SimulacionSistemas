library('ggplot2')
noprob <- as.data.frame(read.csv("r1pfinal.csv"))
prob <-as.data.frame(read.csv("r1probfinal.csv"))
ambos<- data.frame()
ambos<-rbind(noprob,prob)
#colnames(ambos) <- c("Generación","Valor","Diferencia","Tipo")
ambos$Población <- as.factor(ambos$Población)
png(paste("ambosd.png", sep=""), width=700, height=700)
ggplot(data=ambos,aes(x=Población,y=Diferencia,fill=Tipo))+geom_boxplot()#+ylab("Tiempos (s)")#stat_summary(fun.y=mean,geom="smooth",aes(group=Tipo,col=Tipo))
#boxplot(pa$Tiempos~pa$k, color="red")
graphics.off()

####
png(paste("noprob.png", sep=""), width=700, height=700)
boxplot(noprob$X~noprob$Diferencia)
graphics.off()
####
png(paste("prob.png", sep=""), width=700, height=700)
prob$Población <- as.factor(prob$Población)
ggplot(data=prob,aes(x=Población,y=Diferencia))+geom_boxplot()#+ylab("Tiempos (s)")#stat_summary(fun.y=mean,geom="smooth",aes(group=Tipo,col=Tipo))

plot(prob$Población~prob$Diferencia)
graphics.off()
###
