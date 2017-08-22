library(parallel)

dim <- 50

num <-  dim^2

num.nucleo <- 10

actual <- matrix(rep(0, num), nrow=dim, ncol=dim)

a <- round(runif(num.nucleo, 1, num))



for (i in 1:num.nucleo){

  actual[a[i]]=i

}



suppressMessages(library("sna"))

png("p2_t0.png")

plot.sociomatrix(actual, main="Inicio", col=rainbow(num.nucleo))

graphics.off()



paso <- function(pos) {

  fila <- floor((pos - 1) / dim) + 1

  columna <- ((pos - 1) %% dim) + 1

  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),

                      max(columna - 1, 1): min(columna + 1, dim)] 

  

  crear <- c()

  if (actual[fila,columna] == 0){

  if(any(vecindad != 0)){

    for(j in 1:length(vecindad)){

      if(vecindad[j] != 0 ){

        crear <- c(crear, vecindad[j])

      }

    }

    val.nucleo <- crear[1]

    return(val.nucleo)

  }

  else {return (actual[fila, columna])}

  }

  else {return (actual[fila, columna])}

}



cluster <- makeCluster(detectCores() - 1)

clusterExport(cluster, "dim")

clusterExport(cluster, "paso")



for (it in 1:40) { #it es iteracion

  clusterExport(cluster, "actual")

  sig <- parSapply(cluster, 1:num, paso)

  actual <- matrix(sig, nrow=dim, ncol=dim, byrow=TRUE)

  salida = paste("p2_t", it, ".png", sep="")

  tiempo = paste("Paso", it)

  

  png(salida)

  plot.sociomatrix(actual, diaglab=FALSE, main=tiempo, col=rainbow(num.nucleo))

  graphics.off()

  

    if (all(actual != 0)) {

      break;

  }

}

stopCluster(cluster)
