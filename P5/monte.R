library(parallel)

maprox <- data.frame()
mtiempo<- data.frame()
merror<- data.frame()

rep<- 15
p<-c(500,600,700,800,900)
error <-function(x){return(abs(x-0.0488341111))}
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador <- r(AbscontDistribution(d = g)) # creamos un generador

desde <- 3
hasta <- 7

for(r in 1:5){
	ap<-numeric()
		ti<-numeric()
		er<-numeric()
		datos<-c()
	for(l in 1:rep){
pedazo <- p[r]
cuantos <- 100
parte <- function(z) {
valores <- generador(pedazo)
return(sum(valores >= desde & valores <= hasta))
}
cluster<-makeCluster(detectCores() - 1)
clusterExport(cluster,"generador")
clusterExport(cluster,"parte")
clusterExport(cluster,"pedazo")
clusterExport(cluster,"desde")
clusterExport(cluster,"hasta")

montecarlo <- parSapply(cluster, 1:cuantos, parte)
datos <- c(datos, montecarlo)
#suppressMessages(library(doParallel))
#registerDoParallel(makeCluster(detectCores() - 1))

#montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
stopCluster(cluster)
integral <- sum(datos)/(cuantos * pedazo)
aprox <-round((pi / 2)*integral,digits=10)
a<-error(aprox)
ap<-c(ap,aprox)
#ti<-c(ti,tiempo[3])
er<-c(er,a)
}
	maprox<-rbind(maprox,ap)
	#mtiempo<- rbind(mtiempo,ti)
	merror<-rbind(merror,er)
}

#colnames(maprox)=c("50000","60000","70000","80000","90000")
#colnames(mtiempo)=c("50000","60000","70000","80000","90000")
#colnames(merror)=c("50000","60000","70000","80000","90000")

write.csv(maprox,file="aprox.csv")
#write.csv(mtiempo,file="tiempos.csv")
write.csv(merror,file="error.csv")
#png("aprox.png")
#boxplot(data.matrix(maprox),use.cols=FALSE,xlab="Muestra",ylab="Valor",main="35 Repeticiones")
#graphics.off()

#print(paste(mtiempo[3],maprox))