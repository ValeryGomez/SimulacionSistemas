g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -5
high <- 4
step <- 0.25
replicas <- 100
m<- replicas



  tmax <- 10^2
  ######
  t <- 10^2
  curr <- c(runif(1, low, high),runif(1, low, high))
  best <- curr
  t <- c(120,200,500)
  hx <- c()
  hy <- c()
  a <- c(.99,.95,.90,.87)
  for(te in 1:length(t)){
    temp <- data.frame()
    for(la in 1:length(a)){
      aa <- a[la]
    temperatura <- t[te]

  for (tie in 1:t) {
    delta <- runif(1, 0, step)
    pnts<- c(curr[1]+delta,curr[2],curr[1]-delta,curr[2],curr[1],curr[2]+delta,curr[1],curr[2]-delta)
    valores <- c(g(pnts[1],pnts[2]),g(pnts[3],pnts[4]),g(pnts[5],pnts[6]),g(pnts[7],pnts[8]))
    v <- sample(4,1)
    del <- (g(curr[1],curr[2]) - valores[v])
    if(g(curr[1],curr[2]) < valores[v]){
      curr<- c(pnts[(v*2)-1],pnts[v*2])
    }else{
      e <- exp(del/temperatura)
      if(runif(1,0,1) < e){
        curr <- c(pnts[(v*2)-1],pnts[v*2])
      }
    }
    if(g(curr[1],curr[2]) > g(best[1],best[2])){
      best <- curr
    }
    temp <- rbind(temp,c(tie,temperatura))
    temperatura <- temperatura*(aa)
    hx <- c(hx,curr[1])
    hy <- c(hy,curr[2])
    
  }
    }
    png(paste("tiempo_", te ,"_",aa,".png", sep=""), width=700, height=700)
    plot(temp , main= paste("Temperatura ",te), col="red")
    graphics.off()
  }
  ######
  for(i in 1:replicas){
    
  x <- seq(-6, 5, 0.25)
  y <- x
  z <- outer(x, y, g)
  dimnames(z) <- list(x, y)
  library(reshape2)
  d <- melt(z)
  names(d) <- c("x", "y", "z")
  library(lattice)
  png(paste("poo7_", i, ".png", sep=""), width=700, height=700)
  plot(levelplot(z ~ x * y, data = d) , main= paste("Paso ", i))
  trellis.focus("panel", 1, 1, highlight=FALSE)
  lpoints(hx[i],hy[i], pch=16, col="red")
  trellis.unfocus()
  graphics.off()
  }
  
