#rm(list=ls())
#load(file="~/lavori/proveR/ADati/n.rda")
#load(file="~/lavori/proveR/ADati/mx.rda")
#load(file="~/lavori/proveR/ADati/sx.rda")
#s2x <- sx^2

"Welch" <-
function(n,mx,s2x)
{
    k <- length(n)
    w <- n/s2x
    Xp <- sum(w*mx)/sum(w)
    
    Fnum <- sum(w*(mx-Xp)^2)/(k-1)
    
    Fden <- 0
    for (h in 1:k) {
        a <- 1/(n[h]-1)
        b <- (1-(w[h]/sum(w)))^2
        Fden <- Fden+(a*b)
    }
    gl2.den <- Fden*3
    Fden <- Fden*((2*(k-2))/(k^2-1))+1
    
    ## F corretto
    Fw <- Fnum/Fden
    STATISTIC <- Fw
    
    ### gradi di liberta'
    gl1 <- k-1
    gl2.num <- k^2-1
    gl2 <- gl2.num/gl2.den
    PARAMETER <- c(gl1,gl2)
    
    ## probabilita'
    PVAL <- pf(Fw,gl1,gl2,lower.tail=FALSE)
    
    METHOD <- "Welch ANOVA"
    DNAME <- NA
    names(STATISTIC) <- "F"
    names(PARAMETER) <- c("num df", "denom df")
    #RVAL <- list(statistic = STATISTIC, parameter = PARAMETER, 
    #    p.value = PVAL, method = METHOD) #, data.name = DNAME)
    #class(RVAL) <- "anova"
    
    table <- data.frame(PARAMETER, c(Fnum,Fden), STATISTIC, PVAL)
    table[length(PVAL)+1, 3:4] <- NA
    dimnames(table) <- list(c("Factor", "Residuals"), c("Df", 
                         "Mean Sq", "F value", "Pr(>F)"))

    RVAL <- structure(table, heading = c("Analysis of Variance Table (Welch correction)\n"), 
          class = c("anova", "data.frame"))
    return(RVAL)
}

#Welch(n,mx,s2x)