primo <- function(n){
		if(n==1 || n==2|| n==3){ 
			return(TRUE)
	}
	if(n%%2 ==0){
		return(FALSE)
	}
	for(i in seq(3,max(3,n-1),2)){
		if((n%%i)==0){
			return(FALSE)
		}
	}
	return(TRUE)
}


primos <- numeric() #un vector vacio
for(n in 1:120){
	if ( primo(n)){
		primos <- c(primos, n)
	}
}
cat(primos,"\n")
