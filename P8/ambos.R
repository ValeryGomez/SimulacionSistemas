pa <- as.data.frame(read.csv("tiempos.csv"))
sec <-as.data.frame(read.csv("Stiempos.csv"))
ambos<- data.frame()
ambos<-rbind(pa,sec)
png(paste("ambos.png", sep=""), width=300, height=300)
boxplot(pa$Tiempos~pa$k, color="red")
graphics.off()