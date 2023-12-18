"cramer.phi" <-
function(chi,N,r,c=r)
{
    k <- (min(r,c)-1)
    phi <- sqrt(chi/(N*k))
    return(phi)
}

