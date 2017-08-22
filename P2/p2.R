library(parallel)
dim <- 10
num <-  dim^2
proba<- c(.1,.2,.3,.4,.5,.6,.7,.8,.9)
phi<-data.frame()

for(i in proba){
	paro<-numeric()
	for(a in 1:10){
numeritos <- 1*(runif(num)< i)

actual <- matrix(numeritos, nrow=dim, ncol=dim)

suppressMessages(library("sna"))
png("p2_t0.png")
plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
graphics.off()
 
paso <- function(pos) {
    fila <- floor((pos - 1) / dim) + 1
    columna <- ((pos - 1) %% dim) + 1
    vecindad <-  actual[max(fila - 1, 0) : min(fila + 1, dim),
                        max(columna - 1, 0): min(columna + 1, dim)]
    return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}
 
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")


for (iteracion in 1:30) {
    clusterExport(cluster, "actual")
    siguiente <- parSapply(cluster, 1:num, paso)
    if (sum(siguiente) == 0) { # todos murieron
        #print("Ya no queda nadie vivo.")
		paro <- c(paro,iteracion)
        break;
    }
    actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    salida = paste("p2_t", iteracion, ".png", sep="")
    tiempo = paste("Paso", iteracion)
    png(salida)
    plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
    graphics.off()
if(iteracion==30){
	paro<-c(paro,0)
}
}
stopCluster(cluster)
}
cat(paro,"\n")
phi<-rbind(phi,paro)
}
print(phi)
tao <- t(phi)
print(tao)
boxplot(tao, xlab="Probabilidades", ylab="Iteraciones")
graphics.off()