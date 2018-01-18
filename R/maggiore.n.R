"maggiore.n" <-
function(x)
{
    mx <- x[1]
    for (i in 2:length(x)) {
        if (x[i]>mx) mx <- x[i]
    }
    return(mx)
}

