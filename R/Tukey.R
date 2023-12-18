"Tukey" <-
function(x,n,MSerr,MSdf,xlab=NA)
{
    x <- sort(x,index.return=TRUE)
    if (is.na(xlab[1])) {
        xlab <- paste("m",x$ix,sep="")
    } else {
        xlab <- xlab[x$ix]
    }
    x <- x$x
    
    delta <- matrix(NA,choose(length(x),2),1)
    nomic <- delta
    if (length(n)>1) n <- length(n)/sum(1/n) 
        
    k <- 1
    for (i in 1:(length(x)-1)) {
        for (j in (i+1):length(x)) {
            nomic[k] <- paste(xlab[j],"-",xlab[i],sep="")
            delta[k] <- (x[j]-x[i])/sqrt(MSerr/n)
            k <- k+1
        }
    }
    
    rownames(delta) <- nomic
    colnames(delta) <- "q"
    PVAL <- round(ptukey(delta,nmeans=length(x),df=MSdf,lower.tail=FALSE),3)
    colnames(PVAL) <- "pval"
    delta <- cbind(round(delta,3),PVAL)
    return(list(Tukey.test=delta))
}

