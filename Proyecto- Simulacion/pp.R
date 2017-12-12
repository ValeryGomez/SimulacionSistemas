datos <- read.csv("datos_clientes.csv", sep = ",", header = TRUE)
todos <- data.frame()
for(x in 2:length(datos[,1])) #Itera Renglones
{
 # x<-3
  tendencia <-datos[x,1]
  for(y in 2:40) #Itera Bloques
  {
   #y <- 40
    ten =  datos[x, y + 1] - datos[x, y]
    tendencia <- c(tendencia, ten)
}
  todos <- rbind(todos, tendencia)
}
cliente <- seq(1:1537)
todos[,1] <- cliente
names(todos)= c("Cliente", seq(1:39))
suma <- rowSums(todos[,2:40])
todos<- cbind(todos,suma)
