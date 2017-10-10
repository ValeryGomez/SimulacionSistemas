n <- 50
c=rnorm(n)
m <- (floor(abs(c)*50) + 1)
p <- data.frame(x = rnorm(n), y=rnorm(n),c, m)
tiemx <- c(p$x)
tiemy <- c(p$y)

xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
png("p9i.png")
library(lattice)
xyplot(y ~ x, group=g, data=p, auto.key=list(space="right"),
       xlab="X", ylab="Y", main="Part\u{00ed}culas generadas",
       par.settings = list(superpose.symbol = list(pch = 15, cex = 1.5,
                                                   col = colores)))
graphics.off()
eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  fx <- 0
  fy <- 0
  for (j in 1:n) {
    cj <- p[j,]$c
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj)/ (sqrt(dx^2 + dy^2) + eps)
    fx <- fx - dx * factor
    fy <- fy - dy * factor
  }
  return(c(fx, fy))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
system("rm -f p9_t*.png") # borramos anteriores en el caso que lo hayamos corrido
tmax <- 100
iteraciones <-c()
for(i in 1:(tmax)){ iteraciones <-c(iteraciones, seq(1,n)) }
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}
png(paste("p9_t", tl, ".png", sep=""))
plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
     main="Estado inicial", xlab="X", ylab="Y")
graphics.off()
tiemx <- c()
tiemy <- c()
for (iter in 1:tmax) {
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
  #tiem <- c(iter,p[iter]$x,p[iter]$y,p[iter]$m)
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + (delta) * f[c(TRUE, FALSE)][i], 1), 0)
  tiemx <- c(tiemx,p$x)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + (delta) * f[c(FALSE, TRUE)][i], 1), 0)
  tiemy <- c(tiemy,p$y)
  tl <- paste(iter, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("p9_t", tl, ".png", sep=""))
  plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
       main=paste("Paso", iter), xlab="X", ylab="Y")
  graphics.off()
}
stopImplicitCluster()

distancia <- c()
for(i in 1:(n*(tmax))){
  distancia <- c(distancia, ((tiemx[i]-tiemx[i+50])^2 + (tiemy[i]-tiemy[i+50])^2)^(1/2))
}
masa <-c()
for(i in 1:(tmax)){masa <-c(masa, m)}


totaloriginal <- data.frame(iteraciones,distancia,masa)
totaloriginal$masa <- as.factor(totaloriginal$masa)
library('ggplot2')
png(paste("totaloriginal.png", sep=""), width=700, height=700)
ggplot(data=totaloriginal,aes(x=masa,y=distancia,fill=iteraciones))+geom_boxplot()#stat_summary(fun.y=mean,geom="smooth",aes(group=Tipo,col=Tipo))
graphics.off()

write.csv(totaloriginal,file="TotalO.csv")
#system("convert -delay 50 -size 300x300 p9_t*.png -loop 0 p9.gif") # creamos animacion con ImageMagick