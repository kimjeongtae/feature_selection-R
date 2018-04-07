HFAM <- function(X,Y,k=ncol(X),th=0){
	su_ic <- SU(X,Y)
	m <- which.max(su_ic)
	S <- names(su_ic)[m]
	L <- names(su_ic)[-m]
	su_ij <- matrix(SU(X[L],X[S]),byrow=T)
	rownames(su_ij) <- L
	repeat{
		L <- L[which(su_ij[,1] - su_ic[L] < th)]
		if(length(L) == 0 || length(S)==k) break
		m <- which.max(su_ic[L] - apply(su_ij[L,,drop=F],1,mean))
		S <- c(L[m],S)
		L <- L[-m]
		if(length(L) ==0 || length(S)==k) break
		su_ij <- cbind(SU(X[L],X[S[1]]),su_ij[L,,drop=F])
	}
	rev(S)
}
