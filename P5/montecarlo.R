library(doParallel)

maprox <- data.frame()
mtiempo<- data.frame()
merror<- data.frame()

c<-100
rep<- 15
p<-c(500,600,700,800,900)

error <-function(x){return(abs(x-0.0488341111))}
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador <- r(AbscontDistribution(d = g)) # creamos un generador

desde <- 3
hasta <- 7

for(i in 1:5){
	ap<-numeric()
		ti<-numeric()
		er<-numeric()
	for(l in 1:rep){
pedazo <- p[i]
cuantos <- c
parte <- function() {
valores <- generador(pedazo)
return(sum(valores >= desde & valores <= hasta))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

tiempo <- system.time(montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte())
stopImplicitCluster()
integral <- sum(montecarlo) / (cuantos * pedazo)
aprox <-round((pi / 2)*integral,digits=10)

a<-error(aprox)

ap<-c(ap,aprox)
ti<-c(ti,tiempo[3])
er<-c(er,a)
}
	maprox<-c(maprox,ap)
	mtiempo<- c(mtiempo,ti)
	merror<-c(merror,er)
}

#colnames(maprox)=c("50000","60000","70000","80000","90000")
#colnames(mtiempo)=c("50000","60000","70000","80000","90000")
#colnames(merror)=c("50000","60000","70000","80000","90000")

write.csv(maprox,file="aprox.csv")
write.csv(mtiempo,file="tiempos.csv")
write.csv(merror,file="error.csv")
#png("aprox.png")
#boxplot(data.matrix(maprox),use.cols=FALSE,xlab="Muestra",ylab="Valor",main="35 Repeticiones")
#graphics.off()

#print(paste(mtiempo[3],maprox))