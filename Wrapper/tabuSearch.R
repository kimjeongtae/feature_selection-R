evaluate<-function(th){
	if (sum(th) == 0)return(0)
	csRate <- cv(X[th==1],Y,10)
	penalty <- sum(th) * 0.0001
	return(csRate - penalty)
}

tabuSearch <- function(size, current=NULL, listSize = 50, objFunc = NULL){
	is.tabu <- function(x){any(sapply(tabuList,function(i) all(x==i)))}
	bestSolution <- current
	bestCost <- objFunc(bestSolution)
	tabuList <- list(current)
	listLocation <- 1 
	iter <- 0
	fn <- 0
	while(fn < 100 && iter < 3000){
		cat(iter,'\t',fn,'\n');iter<-iter+1
		neighbor <- matrix(current, size, size, byrow = T)
		diag(neighbor) <- abs(diag(neighbor) -1)
		tabu <- apply(neighbor, 1, is.tabu)
		neighbor[tabu, ] <- 0
		neighborCost <- apply(neighbor, 1, objFunc)
		move <- which.max(neighborCost)
		current <- neighbor[move, ]
		listLocation <- listLocation %% listSize + 1
		tabuList[[listLocation]] <- current
		if(bestCost < neighborCost[move]){
			bestSolution <- neighbor[move,]
			bestCost <- neighborCost[move]
			fn <- -1
		}
		fn <- fn + 1
	}
	bestSolution
}
library("compiler")
ts <- cmpfun(tabuSearch)

