library(parallel)

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

fprueba <- function(i){
  d <- sample(0:tope, 1)
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

modelos <- read.csv("digitos.modelo.csv", sep=" ", header=FALSE, stringsAsFactors=F)
mn <- c(.995,.9,.995,.9,.002,.0003)
mg <- c(.92,.825,.995,.9,.002,.0002)
mb <- c(.002,.093,.995,.9,.002,.0003)

r <- 5
c <- 3
dim <- r * c

tasa <- 0.15
tranqui <- 0.99

tope <- 9
digitos <- 0:tope
k <- length(digitos)
contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
rownames(contadores) <- 0:tope
colnames(contadores) <- c(0:tope, NA)

n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

cluster <- makeCluster(detectCores() - 1)
tiempopar <- data.frame()
ent <- c(300,500,700,1000)
repeticiones <- 20
for(vg in 1:length(mn)){
###
    modelos[modelos=='n'] <- mn[vg]
    modelos[modelos=='g'] <- mg[vg]
    modelos[modelos=='b'] <- mb[vg]
###

for (t in 1:5000) { # entrenamiento
  d <- sample(0:tope, 1)
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

for(e in 1:length(ent)){
  for(re in 1:repeticiones){
    correcpor <- c()
    ciertos <- data.frame
    resu <- data.frame()
    entrena <- ent[e]
    clusterExport(cluster, "fprueba")
    clusterExport(cluster, "modelos")
    clusterExport(cluster, "binario")
    clusterExport(cluster, "decimal")
    clusterExport(cluster, "neuronas")
    clusterExport(cluster, "tope")
    clusterExport(cluster, "dim")
    clusterExport(cluster, c("n","ciertos"))
    resu <- parSapply(cluster, 1:entrena, fprueba)
    resu <- t(resu)
    correcpor <- ((sum(resu[,2]) * 100)/entrena)
    tiempopar <- rbind(tiempopar,c(re,ent[e],correcpor,vg))
  }
}
}

stopCluster(cluster)
colnames(tiempopar)= c("Repeticion","Cantidad","Porcentaje","Combinacion")

#print(contadores)
write.csv(tiempopar, file="PorcentParIgual.csv")