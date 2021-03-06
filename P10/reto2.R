library(testit)
library(parallel)
tiemposec <- data.frame()
valo <- data.frame()
knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) 
  assert(n == length(valor))
  vt <- sum(valor) 
  if (pt < cap) { 
    return(vt)
  } else {
    filas <- cap + 1 
    cols <- n + 1 
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) 
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 
    }
    rownames(tabla) <- 0:cap 
    colnames(tabla) <- c(0, valor) 
    for (objeto in 1:n) { 
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}
################# mutacion #################
fun.mutar <- function(i){
  if (runif(1) < pm) {
    solm <- mutacion(p[i,], n)
    return(solm)
  }
  return(p[i,])
}
#################################################### 

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

######### reproduccion ##############
fun.reproduccion <- function(i){
  padres <- sample(1:tam, 2, replace=FALSE)
  hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
  return(hijos)
}
###########################################

###########################################
val.objetivo <- function(i){
  vobj <- objetivo(p[i,], valores)
  return(vobj)
}

fun.factibilidad <- function(i){
  esfact <- factible(p[i,], pesos, capacidad)
  return(esfact)
}
###################################################################

n <- 50
pesos <- generador.pesos(n, 15, 80)
valores <- generador.valores(pesos, 10, 500)
capacidad <- round(sum(pesos) * 0.65)
optimo <- knapsack(capacidad, pesos, valores)
valss<-c(20,50,70,100,200,350,400)
for(va in 1:4){
  for(la in 1:10){
  init <- valss[va]
  p <- poblacion.inicial(n, init)
  tam <- dim(p)[1]
  assert(tam == init)
  pm <- 0.05
  rep <- 50
  tmax <- 70
  mejores <- double()
  
  cluster <- makeCluster(detectCores() - 1)
  
  for (iter in 1:tmax) {
    c <- Sys.time()
    p$obj <- NULL
    p$fact <- NULL
    
    
    clusterExport(cluster, "pm")
    clusterExport(cluster, "p")
    clusterExport(cluster, "mutacion")
    clusterExport(cluster, "n")
    clusterExport(cluster, "tam")
    clusterExport(cluster, "fun.mutar")
    
    
    vec.mutados <- parSapply(cluster, 1:tam, fun.mutar)
    vec.mutados <- unlist(vec.mutados)
    for(auxiliar in 1:tam){
      p[auxiliar, ]<- c(vec.mutados[(52 * (auxiliar-1))+1 : (auxiliar * 52)])
    }
    
    clusterExport(cluster, "n")
    clusterExport(cluster, "tam")
    clusterExport(cluster, "rep")
    clusterExport(cluster, "p")
    clusterExport(cluster, "reproduccion")
    clusterExport(cluster, "fun.reproduccion")
    
    vec.hijos <-  parSapply(cluster, 1:rep, fun.reproduccion)
    vec.hijos <- unlist(vec.hijos)
    for(auxiliar in 1:rep){
      p <- rbind(p,vec.hijos[(50 * (auxiliar-1))+1 : (auxiliar * 50)])
    }
    
    ###### factibilidad y valor objetivo ############
    tam <- dim(p)[1]
    obj <- double()
    fact <- integer()
    
    clusterExport(cluster, "val.objetivo")
    clusterExport(cluster, "fun.factibilidad")
    clusterExport(cluster, "tam")
    clusterExport(cluster, "obj")
    clusterExport(cluster, "fact")
    clusterExport(cluster, "objetivo")
    clusterExport(cluster, "valores")
    clusterExport(cluster, "p")
    clusterExport(cluster, "factible")
    clusterExport(cluster, "pesos")
    clusterExport(cluster, "capacidad")
    
    obj <- parSapply(cluster, 1:tam, val.objetivo)
    fact <- parSapply(cluster, 1:tam, fun.factibilidad)
    
    p <- cbind(p, obj)
    p <- cbind(p, fact)
    ######Reto2#####
    suma <- sum(p$obj)
    probabilidad <- c()
    for(indice in 1:tam){
      pr <- (p$obj[indice]) / suma
      probabilidad <- c(probabilidad,pr)
    }
    p <- cbind(p, probabilidad)
    ################
    mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    p <- p[mantener,]
    tam <- dim(p)[1]
    assert(tam == init)
    factibles <- p[p$fact == TRUE,]
    mejor <- max(factibles$obj)
    mejores <- c(mejores, mejor)
    
    d <- Sys.time()
    ti <- c(c,d)
    tie <- diff(ti,units="secs")
    tiemposec <- rbind(tiemposec,c(init,tie))
  }
    valo<-rbind(valo,c(init,mejor, (optimo - mejor) / optimo))
  }
}
stopCluster(cluster)

tipos <- rep("Normal",40)
valo <-cbind(valo,tipos)
colnames(valo) <- c("Valor","Diferencia","Tipo")

#tipos <- rep("Paralelo",tmax*length(valss))
#tiemposec <-cbind(tiemposec,tipos)
#colnames(tiemposec) <- c("Poblaci?n","Tiempos","Tipo")
#print(paste(mejor, (optimo - mejor) / optimo))
#print(tiemposec)
#write.csv(tiemposec, file="TiemposP.csv")
#print(valo)
write.csv(valo, file="r2pfinal.csv")
