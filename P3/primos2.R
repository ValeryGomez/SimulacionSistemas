primo <- function(n) {
    if (n == 1 || n == 2 || n== 3) {
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
 
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
primos <- foreach(n = 1:300, .combine=c) %dopar% primo(n)
stopImplicitCluster()
print(paste(sum(primos), "primos encontrados"))

primos <- numeric()
for (n in 1:300) {
    if (primo(n)) {
        primos <-  c(primos, n)
    }
}
cat(primos, "\n")