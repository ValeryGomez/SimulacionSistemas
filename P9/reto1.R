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
library(lattice)
png("p9i.png")
grafica <- ggplot(p, aes(x=p$x, y=p$y))
grafica+geom_point(aes(size= p$m, col=colores[p$g+6]))+ xlab("X")+ ylab("Y") + 
  labs(color= "carga", size="masa")+
  scale_color_manual(labels=seq(5,-5,-1),values=colores)+
  guides(col= guide_legend(override.aes = list(size=3, stroke=1.5))) +
  scale_size_continuous(breaks=seq(0,0.1,0.01),labels=seq(0,0.1,0.01))
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
graficas <- ggplot(p, aes(x=p$x, y=p$y))
graficas+geom_point(aes(size= p$m,col=colores[p$g+6]))+ xlab("X")+ ylab("Y") + 
  labs(color= "carga", size="masa")+
  scale_color_manual(labels=seq(5,-5,-1),values=colores)+
  guides(col= guide_legend(override.aes = list(size=3, stroke=1.5))) +
  scale_size_continuous(breaks=seq(0,0.1,0.01),labels=seq(0,0.1,0.01))
graphics.off()
tiemx <- c()
tiemy <- c()
for (iter in 1:tmax) {
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
  #tiem <- c(iter,p[iter]$x,p[iter]$y,p[iter]$m)
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + (delta/p[i,]$m) * f[c(TRUE, FALSE)][i], 1), 0)
  tiemx <- c(tiemx,p$x)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + (delta/p[i,]$m) * f[c(FALSE, TRUE)][i], 1), 0)
  tiemy <- c(tiemy,p$y)
  tl <- paste(iter, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("p9_t", tl, ".png", sep=""))
  graficass <- ggplot(p, aes(x=p$x, y=p$y))
  graficass = graficass+geom_point(aes(size= p$m, col=colores[p$g+6]))+ xlab("X")+ ylab("Y") + 
    labs(color= "carga", size="masa")+
    scale_color_manual(labels=seq(5,-5,-1),values=colores)+
    guides(col= guide_legend(override.aes = list(size=3, stroke=1.5))) +
    scale_size_continuous(breaks=seq(0,0.1,0.01),labels=seq(0,0.1,0.01))
  print(graficass)
  #plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
  #     main=paste("Paso", iter), xlab="X", ylab="Y")
  graphics.off()
}
stopImplicitCluster()

distancia <- c()
for(i in 1:(n*(tmax))){
  distancia <- c(distancia, ((tiemx[i]-tiemx[i+50])^2 + (tiemy[i]-tiemy[i+50])^2)^(1/2))
}
masa <-c()
for(i in 1:(tmax)){masa <-c(masa, m)}


total <- data.frame(iteraciones,distancia,masa)
total$masa <- as.factor(total$masa)
library('ggplot2')
png(paste("totalR.png", sep=""), width=700, height=700)
ggplot(data=total,aes(x=masa,y=distancia,fill=iteraciones))+geom_boxplot()#stat_summary(fun.y=mean,geom="smooth",aes(group=Tipo,col=Tipo))
graphics.off()

write.csv(total,file="TotalReto.csv")

---
  png("p9radios.png")
grafica <- ggplot(p, aes(x=p$x, y=p$y))
grafica+geom_point(aes(size= p$m,col=colores[p$g+6]))+ xlab("x")+ ylab("y") + 
  labs(color= "carga", size="masa")+
  scale_color_manual(labels=seq(5,-5,-1),values=colores)+
  guides(col= guide_legend(override.aes = list(size=3, stroke=1.5))) +
  scale_size_continuous(breaks=seq(0,0.1,0.01),labels=seq(0,0.1,0.01))
graphics.off()

#system("convert -delay 50 -size 300x300 p9_t*.png -loop 0 p9.gif") # creamos animacion con ImageMagick