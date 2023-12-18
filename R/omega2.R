"omega2" <-
function(Fcal,k,n)
{
    num <- (k-1)*(Fcal-1)
    den <- num+(k*n)
    omega <- num/den
    return(omega)
}

