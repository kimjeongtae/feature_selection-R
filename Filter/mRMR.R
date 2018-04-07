FCBF <- function(X,Y,a=0){
    su_ic <- SU(X,Y)
    su_ic <- sort(subset(su_ic, su_ic > a), de=T)
    L <- names(su_ic)[-1]
    S <- names(su_ic)[1]
    i <- 1
    repeat{
        if( length(L) == 0 ) break
        su_ij <- SU(X[L], X[S[i]])
        L <- subset(L,(su_ic[L] - su_ij) > 0 )
	  S <- c(S,L[1])
        L <- L[-1]     
        i <- i + 1
    }
    S[complete.cases(S)]
}