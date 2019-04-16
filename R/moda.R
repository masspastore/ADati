moda <- function(x) {
    y <- table(x)
    nmax <- max(y)
    m <- names(y)[which(y==nmax)]
    #cat(paste0("Moda = ",m, " con frequenza ",nmax),"\n")
    return(list(moda=m, frequenza.modale=nmax))
}
#' @examples 
#' x <- sample(5,10,replace=TRUE)
#' moda(x)
#' 

