forward<-function(X,Y,objFunc=cv){
	accu <-NULL
	i<-1
	for(i in 1:ncol(X)){
		accu <- c(accu,objFunc(X[1:i],Y,10))
		i <- i + 1
	}
	return(accu)
}