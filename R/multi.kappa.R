"multi.kappa" <-
function(r,nrater=NA) 
{
    r <- as.matrix(r) 
    if (is.na(nrater)) nrater <- mean(apply(r,1,sum))
    
    ### controllo su nrater
    somme <- apply(r,1,sum)
    if (length(which(nrater!=somme))>0) warning("Le righe non hanno tutte lo stesso numero di valutatori!")
    
    #######################################
    pj <- apply(r,2,sum)/(nrow(r)*nrater)
    ni2 <- apply(r^2,1,sum)
    pE <- sum(pj^2)
    pA <- (1/((nrow(r)*nrater)*(nrater-1)))*sum(r^2)-(1/(nrater-1))
    m.kappa <- (pA-pE)/(1-pE)
    
    var.k <- (2/(nrow(r)*nrater*(nrater-1)))*((pE-(2*nrater-3)*(pE^2)+2*(nrater-2)*sum(pj^3))/((1-pE)^2))
    z.k <- m.kappa/sqrt(var.k) 

    DNAME <- paste("N = ",nrow(r)," val = ",nrater,sep="")
    METHOD <- "Cohen's K test"
    PVAL <- pnorm(z.k,lower.tail=FALSE)
    ALTER <- "true K is greater than zero"
    
    names(z.k) <- "z"
    names(m.kappa) <- "K"
    RVAL <- list(statistic = z.k, p.value = PVAL, alternative = ALTER, 
                method = METHOD, data.name = DNAME, estimate = m.kappa)
        
    class(RVAL) <- "htest"
    return(RVAL)
}

