### distribuzione campionaria della media
#rm(list=ls())
#set.seed(20121018)
#Omega <- sample(1:20,150,TRUE)
##Omega <- Omega[1:5]
#n <- 2
#replace=FALSE;
#exact=FALSE;
#grafico=TRUE;
#B=1000
#parziali=FALSE

## richiede gtools
campionaria.media <- function(Omega,n=2,replace=FALSE,exact=FALSE,grafico=TRUE,B=1000,
                              parziali=FALSE) 
{
    # controlli    
    if (sum(is.na(Omega))>0) {
      warning(paste("Removing",sum(is.na(Omega)),"NA's"))
      Omega <- na.omit(Omega)
    }
    N <- length(Omega)
    if (N<2) stop('Population must have at least 2 elements')
    if (N<171) {
      (campioni.possibili <- factorial(N)/factorial(N-n))
    } else {
      campioni.possibili <- 50001
    }
    if ((exact==TRUE)&(campioni.possibili>50000)) {
      warning('Exact computation is not alloweed')
      exact <- FALSE
    }
    
    # parametri 
    (mi <- mean(Omega))
    (sigma2 <- var(Omega)*((N-1)/N))
    (sigma <- sqrt(sigma2))
    
    #################################################
    if (exact) {
        # campioni esaustivi
        X <- gtools::permutations(N,n,Omega,set=FALSE,repeats.allowed=replace)
        mx <- apply(X,1,mean)
        Ymax <- max(density(mx)$y)
        xmax <- density(mx)$x[which(density(mx)$y==Ymax)]
        M <- paste("N=",N," n=",n," campioni=",nrow(X),sep="")
    } else {
        # campioni casuali
        mx <- NULL
        for (b in 1:B) {
            x <- sample(Omega,n,replace=replace)
            mx <- c(mx,mean(x))
            if (parziali) {
                par(mfrow=c(1,1))
                H <- hist(mx,col="gray",main=paste("N=",N," n=",n," rep.=",b,sep=""),
                    freq=FALSE)
            }
        }
        H <- density(mx)
        xmax <- max(H$x)
        Ymax <- max(H$y)
        M <- paste("N=",N," n=",n," rep.=",b,sep="")
    }
    
    if (grafico) {
        par(mfrow=c(1,2),mar=c(3,2,2,2))
        ## popolazione
        hist(Omega,main='popolazione',col='yellow',freq=FALSE)
        abline(v=mi,col="red",lwd=2)
        
        ### campione
        ymax <- dnorm(xmax,mi,sigma/sqrt(n))
        hist(mx,col="gray",main=M,
                freq=FALSE,ylim=c(0,max(ymax,Ymax)),xlim=range(Omega))
        abline(v=mean(mx),col="red",lwd=2)
        curve(dnorm(x,mi,sigma/sqrt(n)),add=TRUE,col="blue",lwd=2)
    }
    
    ##### tabella risultati
    s2x <- var(mx)*(length(mx)-1)/length(mx)
    sx <- sqrt(s2x)
    k <- ifelse(replace,1,((N-n)/(N-1)))
    modo <- ifelse(replace,'con reinserimento','senza reinserimento')
    campionamento <- ifelse(exact,"esatta","approssimata")

    cat("Dati popolazione","\n")
    cat("------------------------------------","\n")
    cat(paste("N = ",N," - mi = ",round(mi,4),
        " - sigma2 = ",round(sigma2,4),
        " - sigma = ",round(sigma,4),
        " - st.err(mx) = ",round(sigma/sqrt(n)*sqrt(k),4),sep=""),"\n")
    cat("------------------------------------","\n")
    cat(" ","\n")
    
    cat("Dati distribuzione campionaria",campionamento,modo,"\n")
    cat("------------------------------------","\n")
        cat(paste("n = ",n," - E(mx) = ",round(mean(mx),4),
            " - V(mx) = ",round(s2x,4),
            " - ds(mx) = ",round(sx,4),sep=""),"\n")
    cat("------------------------------------","\n")

    return(list(Omega=Omega,Smx=mx))

}

#A <- campionaria.media(Omega,exact=TRUE)