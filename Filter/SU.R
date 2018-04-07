SU <- function(X,Y){
    X <- data.frame(X)
    if(is.data.frame(Y))
        Y <- Y[[1]]
    f <- function(x,y){
        n <- length(y)
        Px <- table(x) / n
        Py <- table(y) / n
        Pxy <- table(x,y) / n
        Hx <- -sum( ifelse( Px == 0, 0, Px * log2(Px) ) )
        Hy <- -sum( ifelse( Py == 0, 0, Py * log2(Py) ) )
        Hxy <- -sum( ifelse( Pxy == 0, 0, Pxy * log2(Pxy) ) )
        2 * ( Hx + Hy - Hxy ) / ( Hx + Hy )
    }
    sapply(X,f,Y)
}