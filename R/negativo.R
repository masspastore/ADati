"negativo" <-
function(x)
{
    i <- 0
    trovato <- FALSE
    while ((i<length(x))&(trovato==FALSE)) {
        i <- i+1
        if (x[i]<0) trovato <- TRUE
    }
    if (trovato) {
        return(x[i])
    } else {
        print("non ci sono negativi")
    }
}

