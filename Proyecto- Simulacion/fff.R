binario <- function(d, l) {
  b <- rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

modelos <- read.csv("clasificacion.csv", sep=",", header=FALSE, stringsAsFactors=F)
modelos[modelos=='1'] <- 0.995
modelos[modelos=='-1'] <- 0.92
modelos[modelos=='0'] <- 0.002

r <- 5
c <- 3
dim <- 4

tasa <- 0.15
tranqui <- 0.99

tope <- 81
digitos <- 1:tope
k <- length(digitos)
contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
rownames(contadores) <- 1:tope
colnames(contadores) <- c(1:tope, NA)

n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

for (t in 1:5000) { # entrenamiento
  d <- sample(1:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,]
  correcto <- binario(d, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    if (deseada != resultado) {
      ajuste <- tasa * (deseada - resultado)
      tasa <- tranqui * tasa
      neuronas[i,] <- w + ajuste * pixeles
    }
  }
}

fprueba <- function(i){
  d <- sample(1:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <- binario(d, n)
  salida <- rep(FALSE, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    salida[i] <- resultado
    if(correcto[i] == salida[i]){
      return(c(d,1))
    }else{return(c(decimal(salida, n),0))}
  }
}

cluster <- makeCluster(detectCores() - 1)
tiempopar <- data.frame()
ent <- c(300)
repeticiones <- 20
for(e in 1:length(ent)){
  for(re in 1:repeticiones){
    #  e<-1 
    #  re <- 1
    tiempopar <- data.frame()
    ciertos <- data.frame
    resu <- data.frame()
    
    aa <- Sys.time()
    entrena <- ent[e]
    clusterExport(cluster, "fprueba")
    clusterExport(cluster, "modelos")
    clusterExport(cluster, "binario")
    clusterExport(cluster, "decimal")
    clusterExport(cluster, "neuronas")
    clusterExport(cluster, "tope")
    clusterExport(cluster, "dim")
    clusterExport(cluster, c("n","ciertos"))
    # resu <- parSapplyLB(cluster,1:300,fprueba)
    resu <- parSapply(cluster, 1:entrena, fprueba)
    bb <- Sys.time()
    ti <- c(aa,bb)
    tie <- diff(ti,units="secs")
    resu <- t(resu)
    tiempopar <- rbind(tiempopar,c(re,ent[e],tie))#,correcpor))
  }
}

stopCluster(cluster)
#tipos <- rep("Paralelo",repeticiones*length(ent))
#tiempopar <- cbind(tiempopar,tipos)
#colnames(tiempopar)= c("Repeticion","Cantidad","Tiempo","Tipo")
print(resu)