moda <- function(x) {
    y <- table(x)
    nmax <- max(y)
    m <- names(y)[which(y==nmax)]
    return(m)
}
