caminata <- function(dim, dur, dist){
	pos <- rep(0, dim) 
	mayor <- 0 
for (t in 1:dur){
	if (runif(1)< 0.5){
		pos <- pos +1
	}else {
		pos <- pos -1
	}
	dist <- abs(pos)
	if (dist >mayor){
		mayor <- dist
	}
	print(pos)
}
}
