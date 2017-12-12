us <- as.data.frame(read.csv("usados.csv"))
col <- us$Month
columna <- us$Month[2:length(col)]
clin <-data.frame()
vnum<-c()
vnom <- c()
#library(parallel)


for(i in 1:length(columna)){
  p <- as.vector(columna[i])
  pp <- unlist(strsplit(p," "))
  nc <- pp[length(pp)]
  num <- substr(nc,2,nchar(nc)-1)
  nom <- substring(p, 1, nchar(p)-(nchar(num)+3))
  vnum <- c(vnum,num)
  vnom <- c(vnom,nom)
 #clin <- rbind(clin,c(num,nom))
}

cll <- cbind(vnum, vnom)
write.csv(cll, file="Nombre-codigo.csv")
