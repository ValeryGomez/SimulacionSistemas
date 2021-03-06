naa <- c(2,5,7,10)
tiempos <- data.frame()
repe<-25
for(la in 1:length(naa)){
library(parallel)
pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <- rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <- value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}
cluster <- makeCluster(detectCores() - 1)
vc <- 4
md <- 3
tc <- 5
k <- naa[la]
obj <- list()
for (i in 1:k) {
  obj[[i]] <- poli(md, vc, tc)
}
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
n <- 200
for(bla in 1:repe){
  sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
########
  valsol <- function(i){
    val <- matrix(rep(NA, k), ncol=k)
    for (j in 1:k) { # para todos los objetivos
      val[,j] <- eval(obj[[j]], sol[i,], tc)
    }
    return(val)
  }
########
  
  clusterExport(cluster, "n")
  clusterExport(cluster, "k")
  clusterExport(cluster, "sol")
  clusterExport(cluster, "tc")
  clusterExport(cluster, "obj")
  clusterExport(cluster, "eval")
  clusterExport(cluster, "dim")
  clusterExport(cluster, "valsol")
  
  val <- parSapply(cluster, 1:n, valsol)
  val <- t(val)
  
  mejor1 <- which.max(sign[1] * val[,1])
  mejor2 <- which.max(sign[2] * val[,2])
  cual <- c("max", "min")
  xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
  yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
  png("p11_init.png")
  plot(val[,1], val[,2], xlab=xl, ylab=yl, main="Ejemplo bidimensional")
  graphics.off()
  png("p11_mejores.png")
  plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
       ylab=paste(yl,"mejor con bolita naranja"),
       main="Ejemplo bidimensional")
  points(val[mejor1, 1], val[mejor1, 2], col="blue", pch=15, cex=1.5)
  points(val[mejor2, 1], val[mejor2, 2], col="orange", pch=16, cex=1.5)
  graphics.off()
  no.dom <- logical()
  dominadores <- integer()
  
#####
  domfun <- function(i){
    d <- logical()
    for (j in 1:n) {
      d <- c(d, domin.by(sign*val[i,], sign*val[j,], k))
    }
    cuantos <- sum(d)
    dominadores <- c(dominadores, cuantos)
    return(dominadores)
  }
  
  quiendomi <- function(i){
    no.dom <- c(no.dom, dominadores[i] == 0) # nadie le domina
    return(no.dom)
  }
#######
  
  clusterExport(cluster, "quiendomi")
  clusterExport(cluster, "n")
  clusterExport(cluster, "val")
  clusterExport(cluster, "k")
  clusterExport(cluster, "sign")
  clusterExport(cluster, "domin.by")
  clusterExport(cluster, "dominadores")
  clusterExport(cluster, "no.dom")
  clusterExport(cluster, "domfun")
  
  dominadores <- parSapply(cluster, 1:n, domfun)
  clusterExport(cluster, "dominadores")
  no.dom <- parSapply(cluster, 1:n, quiendomi)
  
  frente <- subset(val, no.dom) # solamente las no dominadas
  png("p11_frente.png")
  plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
       ylab=paste(yl,"mejor con bolita naranja"),
       main="Ejemplo bidimensional")
  points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
  graphics.off()
  library(ggplot2) # recordar instalar si hace falta
  data <- data.frame(pos=rep(0, n), dom=dominadores)
  png("p11_violin.png")
  gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
  gr + geom_boxplot(width=0.2, fill="darkgreen", color="white", lwd=2) +
    xlab("") +
    ylab("Frecuencia") +
    ggtitle("Cantidad de soluciones dominantes")
  graphics.off()
  tiempos <- rbind(tiempos,c(bla,k,cuantos,porcentaje=(dim(frente)[1]*100/n)))
}
}

colnames(tiempos) <- c("Iteracion","Funciones","Dominadores","Porcentaje")
write.csv(tiempos,file="Porciento200.csv")
stopCluster(cluster)

frente <- subset(val, no.dom) # solamente las no dominadas
png("p11_frente.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,"mejor con bolita naranja"),
     main="Ejemplo bidimensional")
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
graphics.off()
library(ggplot2) # recordar instalar si hace falta
data <- data.frame(pos=rep(0, n), dom=dominadores)
png("p11_violin.png")
gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
gr + geom_boxplot(width=0.2, fill="blue", color="white", lwd=2) +
  xlab("") +
  ylab("Frecuencia") +
  ggtitle("Cantidad de soluciones dominantes")
graphics.off()
print(tie)
