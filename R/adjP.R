"adjP" <-
function(x,type=c("BH","BY","B"))
{
    type <- match.arg(type)
    if (length(x)<2) stop("not enough data")
    index.x <- order(x)
    Prob <- sort(x)
    aProb <- rep(NA,length(x))

    ## Benjamini-Hochberg FDR
    if (type=="BH") {
        method <- "Benjamini-Hochberg FDR"
        FDR.p <- rep(1,length(x))
        for (i in length(x):1) {
             FDR.p[i] <- Prob[i]*length(x)/i
             aProb[i] <- min(FDR.p)
        } 
    }
    
    ## Benjamini-Yekutieli FDR
    if (type=="BY") {
        method <- "Benjamini-Yekutieli FDR"
        FDR.p <- rep(1,length(x)+1)
        inversi <- sum(1/(1:length(x)))
        for (i in length(x):1) {
             FDR.p[i] <- (Prob[i]*length(x)*inversi)/i
             aProb[i] <- min(FDR.p)
        } 
    }
     
    ## Bonferroni adjustment    
    if (type=="B") {
        method <- "Bonferroni"
        for (i in 1:length(x)) aProb[i] <- min(Prob[i]*length(x),1)
    }

    aProb <- cbind(round(Prob,3),round(aProb,3))
    colnames(aProb) <- c("Prob","aProb")
    return(list(metodo=method,adjusted.p=aProb,index=index.x))
}

