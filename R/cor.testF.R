"cor.testF" <-
function(r,N,rho=0,alternative = c("two.sided", "less", "greater"))
{
    alternative <- match.arg(alternative)
    DNAME <- paste("N = ",N," r = ",round(r,2),sep="")
    METHOD <- "Fisher's correlation test"

    r1 <- .5*log(abs((1+r)/(1-r)))
    rho1 <- .5*log(abs((1+rho)/(1-rho)))
    sr <- 1/sqrt(N-3)
    z <- (r1-rho1)/sr 
    
    if (alternative=="less") {
        PVAL <- pnorm(z)
        ALTER <- paste("true correlation is less than ",rho,sep="")
    }
    if (alternative=="greater") {
        PVAL <- pnorm(z,lower.tail=FALSE)
        ALTER <- paste("true correlation is greater than ",rho,sep="")
    }
    if (alternative=="two.sided") {
        if (z<=0) {
            PVAL <- pnorm(z)*2
        } else {
            PVAL <- pnorm(z,lower.tail=FALSE)*2
        }
        ALTER <- paste("true correlation is not equal to ",rho,sep="")
    }
    
    names(z) <- "z"
    RVAL <- list(statistic = z, p.value = PVAL, alternative = ALTER, 
                method = METHOD, data.name = DNAME)
        
    class(RVAL) <- "htest"
    return(RVAL)
}
