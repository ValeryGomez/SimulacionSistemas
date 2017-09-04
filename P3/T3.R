primo <- function(n){
	for (i in 2:(n-1)){
		if((n%%i)==0){  #Residuo es cero
			return(FALSE)
		}
	}
	return(TRUE)
}

resultados <- numeric() #un vector vacio
for(n in 1:100){
	resultados <- c(resultados, primo(n))
}
cat(resultados,"\n")