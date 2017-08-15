repetir <- 100
duracion <- 200

eucl <- FALSE
library(parallel)

datos <- data.frame()

experimento <- function(replica){
	pos <- rep(0, dimension)
	mayor <- 0
	for(t in 1:duracion){
		cambiar <- sample(1:dimension,1)
		cambio <- 1
		if(runif(1) < .5){
			cambio <- -1
		}
		pos[cambiar] <- pos[cambiar + cambio]
		if(eucl){
			d <- sum(sqrt(pos**2))
		}else{  #Manhattan
			d <- sum(abs(pos))
			}
			if(d > mayor){
				mayor <- d
			}
	}
	return(mayor)
}


cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "duracion")
clusterExport(cluster, "eucl")
clusterExport(cluster, "experimento")


for(dimension in 1:4){
	clusterExport(cluster, "dimension")
	resultado <- parSapply(cluster, 1:repetir, experimento)
	datos <- rbind(datos, resultado)
}

stopCluster(cluster)
if(eucl) {
	png("plmr.png")
	boxplot(data.matrix(datos), use.cols=FALSE, xlab="Dimensi\u{F3}n", ylab="Distanica m\u{E1}xima", main="Euclideana")
} else{
		png("plmr.png")
	boxplot(data.matrix(datos), use.cols=FALSE, xlab="Dimensi\u{F3}n", ylab="Distanica m\u{E1}xima", main="Manhatthan")
	}
	graphics.off()