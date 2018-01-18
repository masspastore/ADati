#rm(list=ls())
#medie <- c(26.50,37.75,57.50,61.75) # medie osservate
#MSerr <- 150.4583 # ricavata dall’ANOVA

#data(relbambini,package="ADati")
#A <- aov(comp.ling~cat.comp,data=relbambini)

#X <- read.table("/home/el/2013_03/didattica/ADsvil1213/data/eysenck.dat",header=TRUE)
#(MX <- aggregate(X$use,list(X$member,X$trait),mean))
#(mx <- MX$x)
#names(mx) <- paste(MX[,1],MX[,2],sep=".")
#(n <- aggregate(X$use,list(X$member,X$trait),length)$x)

#(A <- summary(aov(use~member*trait,data=X)))
#(MSerr <- A[[1]][4,1])

"Scheffe" <-
function(mx=NULL,n=NULL,MSerr=NULL,w=NULL)
{  
    ## se non ci sono pesi specificati 
    ## esegue tutti i confronti a coppie
    if (is.null(w)) {
        ff <- factor(1:length(mx))
        zero <- rep(0,length(mx))
        tram <- rep(NA,length(mx))
        nomic <- NA
        if (is.null(names(mx))) names(mx) <- paste("x",1:length(mx),sep="")
        for (i in 1:(length(mx)-1)) {
            for (j in (i+1):length(mx)) {
                cont <- zero
                cont[i] <- 1
                cont[j] <- -1
                tram <- rbind(tram,cont)
                nomi <- paste("c",names(mx)[i],"-",names(mx)[j],sep="")
                nomic <- c(nomic,nomi)
            }
        }
        tram <- tram[2:nrow(tram),]
        nomic <- nomic[2:length(nomic)]
        rownames(tram) <- nomic
        w <- tram
    } else {
        controllo <- round(apply(w,1,sum))
        if (length(which(controllo>0))>0) stop("matrice dei pesi errata, somme != zero")
        rownames(w) <- paste("c",1:nrow(w),sep="")
    }   
    
    # nomi delle medie sulle colonne
    xl <- round(mx,2)
    colnames(w) <- names(xl)

    # se le numerosita' sono diverse media armonica
    if (length(n)>1) n <- length(n)/sum(1/n) 
    
    ### calcolo le differenze e le F relative
    nomimx <- names(mx)
    mx <- matrix(mx,ncol=1); rownames(mx) <- nomimx
    num <- n*(w%*%mx)^2
    den <- (length(mx)-1)*MSerr*(apply(w^2,1,sum))
    Fs <- num/den
    
    ### probabilita'
    dfA <- length(mx)-1
    dferr <- n*dfA
    prob <- round(pf(Fs,dfA,dferr,lower.tail=FALSE),3)
    colnames(prob) <- "pval"
    return(list(medie=mx,prob=prob,w=w))
}

# esegue tutti i confronti a coppie
#Scheffe(mx,n,MSerr)

#data(relbambini,package="ADati")
#mx <- tapply(relbambini$comp.ling,relbambini$cat.comp,mean)
#n <- tapply(relbambini$comp.ling,relbambini$cat.comp,length)

#summary(aov(comp.ling~cat.comp,data=relbambini))
#Scheffe(mx,n,18.05)
