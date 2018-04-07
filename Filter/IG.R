IG <- function(X,Y,base = 2){
    X <- data.frame(X)
    if(is.data.frame(Y))
        Y <- Y[[1]]
    f <- function(x,y){
        n <- length(y)
        Px <- table(x) / n
        Py <- table(y) / n
        Pxy <- table(x,y) / n
        Hx <- -sum( ifelse( Px == 0, 0, Px * log(Px,base) ) )
        Hy <- -sum( ifelse( Py == 0, 0, Py * log(Py,base) ) )
        Hxy <- -sum( ifelse( Pxy == 0, 0, Pxy * log(Pxy,base) ) )
        Hx + Hy - Hxy
    }
    sapply(X,f,Y)
}
