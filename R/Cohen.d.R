### x <- variabile risposta numerica
### y <- seconda variabile risposta o fattore
#rm(list=ls())
#library(foreign)
#library(ADati)
#main <- "~/lavori/santinello/"
#datadir <- paste(main,"dati/",sep="")
##----------------------------------------

#X <- read.spss(paste(datadir,"PassCerletti_defT1-T4.sav",sep=""),
#    to.data.frame=TRUE,use.value.labels=FALSE)
#gruppo.item <- which(substr(colnames(X),1,6)=="gruppo")
#X[33,gruppo.item[3]] <- 1 ## CONTROLLARE SOGGETTO 33
#gruppo <- apply(X[,gruppo.item],1,mean,na.rm=TRUE)

#load(paste(datadir,"A2FS.rda",sep=""))
#x <- A2FS[,1]; y <- factor(gruppo); type="two.sample"

Cohen.d <- function(x,y=NULL,mu=0,type=c("two.sample","one.sample","paired"))
{
    ### controlli
    if ((is.null(y)==FALSE)&(is.factor(y)==FALSE)&(is.numeric(y)==FALSE)) stop("y must be numeric or factor")
    type <- match.arg(type)
    DNAME <- deparse(substitute(x))

    if (is.null(y)) {
        if (type!="one.sample") {
            type <- "one.sample"
        }
    } else {
        if (is.factor(y)) {
            DNAME <- paste(DNAME, "by", deparse(substitute(y)))
            if (length(levels(y))>2) stop("i livelli di y devono essere 2")
            z1 <- x[y==levels(y)[1]]
            z2 <- x[y==levels(y)[2]]
            x <- z1
            y <- z2
        } else {
            DNAME <- paste(DNAME, "and", deparse(substitute(y)))
        }
    }

    ### one.sample
    if (type=="one.sample") {
        METHOD <- "One Sample Cohen's d"
        m1 <- mean(x,na.rm=TRUE)
        m2 <- mu
        pool.s <- sd(x)
    }

    ### two.sample
    if (type=="two.sample") {
        METHOD <- "Two Sample Cohen's d"
        m1 <- mean(x,na.rm=TRUE)
        m2 <- mean(y,na.rm=TRUE)
        pool.s <- sqrt((sd(x,na.rm=TRUE)^2+sd(y,na.rm=TRUE)^2)/2)
    }
    
    ### paired.sample
    if (type=="paired") {
        METHOD <- "Paired Sample Cohen's d"
        m1 <- mean(x-y,na.rm=TRUE)
        m2 <- 0
        pool.s <- sqrt(var(x,na.rm=TRUE)+var(y,na.rm=TRUE)-
          2*cor(x,y,use="complete.obs")*sd(x,na.rm=TRUE)*sd(y,na.rm=TRUE))
        if ((length(which(is.na(x)))>0)|(length(which(is.na(y)))>0)) {
          warnings("Ci sono dati mancanti - meglio toglierli prima del calcolo")
        }
    }
    
    d <- abs(m1-m2)/pool.s
    names(d) <- "d"
    
    if (d<=.2) eff <- "small"
    if ((d>.2)&(d<=.4)) eff <- "small/medium"
    if ((d>.4)&(d<.6)) eff <- "medium"
    if ((d>=.6)&(d<=.8)) eff <- "medium/large"
    if (d>=.8) eff <- "large"
    
    structure(list(data.name = DNAME,statistic = round(d,4), 
                effect = eff, method = METHOD), class = "power.htest")
    
}
