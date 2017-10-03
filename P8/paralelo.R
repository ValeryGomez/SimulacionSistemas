v <- c(10000,20000,35000,50000)
veces <- length(v)
tiempos <- data.frame()

for(e in 1:veces){
library(parallel)
library(testit)
k <- v[e]
n <- 30*k
originales <- rnorm(k)
cumulos <- originales - min(originales) + 1
cumulos <- round(n * cumulos / sum(cumulos))
assert(min(cumulos) > 0)
diferencia <- n - sum(cumulos)
if (diferencia > 0) {
  for (i in 1:diferencia) {
    p <- sample(1:k, 1)
    cumulos[p] <- cumulos[p] + 1
  }
} else if (diferencia < 0) {
  for (i in 1:-diferencia) {
    p <- sample(1:k, 1)
    if (cumulos[p] > 1) {
      cumulos[p] <- cumulos[p] - 1
    }
  }
}

assert(length(cumulos[cumulos == 0]) == 0)
assert(sum(cumulos) == n)
c <- median(cumulos) 
d <- sd(cumulos) / 4

############uni??n############
union <- function(x) {
  return (exp(-x / c))
}

unirse <- function(tam, cuantos) {
  unir <- round(union(tam) * cuantos)
  if (unir > 0) {
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
    assert(sum(abs(division)) == tam * cuantos)
    return(division)
  } else {
    return(rep(tam, cuantos))
  }
}

faseunion <- function(i){
  urna <- freq[i,]
  return(unirse(urna$tam, urna$num))
}
#############Rotura #####################
rotura <- function(x) {
  return (1 / (1 + exp((c - x) / d)))
}
  romperse <- function(tam, cuantos) {
    romper <- round(rotura(tam) * cuantos) 
    resultado <- rep(tam, cuantos - romper) 
    if (romper > 0) {
      for (cumulo in 1:romper) { 
        t <- 1
        if (tam > 2) { 
          t <- sample(1:(tam-1), 1)
        }
        resultado <- c(resultado, t, tam - t)
      }
    }
   
    assert(sum(resultado) == tam * cuantos)
    return(resultado)
  }
  
faserotura <- function(i){
  urna <- freq[i,]
  if (urna$tam > 1) {
    return(romperse(urna$tam, urna$num))
  } else {
    return(rep(1, urna$num))
  }
}

###############################################

freq <- as.data.frame(table(cumulos))
names(freq) <- c("tam", "num")
freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
duracion <- 100
digitos <- floor(log(duracion, 10)) + 1

##################Clusters??????????????????????????????

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "faserotura")
clusterExport(cluster, "romperse")
clusterExport(cluster, "rotura")
clusterExport(cluster, "freq")
clusterExport(cluster, "c")
clusterExport(cluster, "d")
clusterExport(cluster, "union")
clusterExport(cluster, "unirse")
clusterExport(cluster, "faseunion")
clusterExport(cluster, "assert")
###########Paralelo#################################
for (paso in 1:duracion) {
  c <- Sys.time()
  
  assert(sum(cumulos) == n)
  cumulos <- integer()
  clusterExport(cluster, "freq")
  
  cumulos <- parSapply(cluster, 1:dim(freq)[1], faserotura)
  cumulos <- unlist(cumulos)
  
  assert(sum(cumulos) == n)
  assert(length(cumulos[cumulos == 0]) == 0)
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
  cumulos <- integer()
 
  clusterExport(cluster, "freq")
  cumulos <- parSapply(cluster, 1:dim(freq)[1], faseunion)
  cumulos <- unlist(cumulos)
  
  assert(sum(abs(cumulos)) == n)
  assert(length(cumulos[cumulos == 0]) == 0) 
  juntarse <- -cumulos[cumulos < 0]
  cumulos <- cumulos[cumulos > 0]
  assert(sum(cumulos) + sum(juntarse) == n)
  nt <- length(juntarse)
  if (nt > 0) {
    if (nt > 1) {
      juntarse <- sample(juntarse)
      for (i in 1:floor(nt / 2) ) {
        cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
      }
    }
    if (nt %% 2 == 1) {
      cumulos <- c(cumulos, juntarse[nt])
    }
  }
  assert(sum(cumulos) == n)
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
  tl <- paste(paso, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  d <- Sys.time()
  ti <- c(c,d)
  tie <- diff(ti,units="secs")
  tiempos <- rbind(tiempos,c(k,tie))
}
##########
stopCluster(cluster)
}

tipop <- rep("Paralelo",veces*duracion)
tiempos <-cbind(tiempos,tipop)
colnames(tiempos) <- c("k","Tiempos","Tipo")
write.csv(tiempos,file="tiempos.csv")

png(paste("paralelo.png", sep=""), width=300, height=300)
boxplot(tiempos$Tiempos~tiempos$k)
graphics.off()
