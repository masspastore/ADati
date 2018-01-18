# NOTA: applicabile solo per numerosita' uguali
# y = variabile risposta
# x = fattore
Cohen.f <- function(y,x)
{
    DNAME <- paste(deparse(substitute(y)), "by", deparse(substitute(x)))
    
    if (length(which(is.na(y)))>0) {
        togli <- which(is.na(y)) 
        y <- y[-togli]
        x <- x[-togli]
    }
    
    mx <- tapply(y,x,mean)    
    s2x <- tapply(y,x,var)
    n <- tapply(y,x,length)
    
    pool.s <- sqrt(sum((n-1)*s2x)/sum(n-1))
    if (pool.s==0) stop("pooled standard deviation is zero")
    
    Mx <- sum(n*mx)/sum(n)
    num <- sqrt(sum(n*(mx-Mx)^2)/sum(n))
    f <- num/pool.s
    names(f) <- "f"
    
    if (f<=.1) eff <- "small"
    if ((f>.1)&(f<=.20)) eff <- "small/medium"
    if ((f>.20)&(f<.30)) eff <- "medium"
    if ((f>=.30)&(f<.4)) eff <- "medium/large"
    if (f>=.4) eff <- "large"
    
    structure(list(data.name = DNAME,statistic = round(f,4), 
                effect = eff, method = "One-way ANOVA Cohen's f"), class = "power.htest")
    
    #return(f)
}
