mRMR <- function(X,Y,k = 10){
	ig_ic <- IG(X,Y)
	m <- which.max(ig_ic)
	S <- names(ig_ic)[m]
	L <- names(ig_ic)[-m]
	ig_ij <- matrix(IG(X[L],X[S]),byrow=T)
	rownames(ig_ij) <- L
	while(length(S) < k){
		m <- which.max(ig_ic[L] - apply(ig_ij,1,mean))
		S <- c(L[m],S)
		L <- L[-m]
		ig_ij <- cbind(ig_ij[L,,drop=F],IG(X[L],X[S[1]]))
	}
	rev(S)
}
