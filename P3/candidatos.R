primo <- function(n) {
    if (n == 1 || n == 2) {
        return(TRUE)
    }
    if (n %% 2 == 0) {
        return(FALSE)
    }
    for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
        if ((n %% i) == 0) {
            return(FALSE)
        }
    }
    return(TRUE)
}
 
desde <- 1000
hasta <-  3000
original <- desde:hasta
invertido <- hasta:desde
dataoriginal<- data.frame()
datainvertido <- data.frame()
dataaleatorio<- data.frame()
replicas <- 20
suppressMessages(library(doParallel))
for(num in 1: (detectCores()-1)){
registerDoParallel(makeCluster(num))
ot <-  numeric()
it <-  numeric()
at <-  numeric()

for (r in 1:replicas) {
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
    at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio

mot <- c(ot)
mit <- c(it)
mat <- c(at)
}
stopImplicitCluster()
print(summary(ot))
print(summary(it))
print(summary(at))
salida = paste("p3_t", num, ".png", sep="")
png(salida)
boxplot(ot, it, at)
graphics.off()
}