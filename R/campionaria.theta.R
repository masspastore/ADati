### distribuzione campionaria 
## richiede gtools
campionaria.theta <- function(Omega,n=2,theta='mean',replace=FALSE,exact=FALSE,grafico=TRUE,B=1000,
                              parziali=FALSE) 
{
    # controlli
    funzione <- theta
    theta <- get(theta,mode='function')
    
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
    mi <- theta(Omega)
    
    #################################################
    if (exact) {
        # campioni esaustivi
        X <- gtools::permutations(N,n,Omega,set=FALSE,repeats.allowed=replace)
        mx <- apply(X,1,theta)
        Ymax <- max(density(mx)$y)
        xmax <- density(mx)$x[which(density(mx)$y==Ymax)]
        M <- paste("N=",N," n=",n," campioni=",nrow(X),sep="")
    } else {
        # campioni casuali
        x <- replicate( B, sample(Omega,n,replace=replace) )
        mx <- apply(x,2, theta)
        
        if (parziali) {
          for (b in 1:B) {
                par(mfrow=c(1,1))
                H <- hist(mx[1:b],col="gray",main=paste("N=",N," n=",n," rep.=",b,sep="",xlab=paste(funzione,'(x)',sep='')),
                    freq=FALSE)
            }
        }
        H <- density(mx)
        xmax <- max(H$x)
        Ymax <- max(H$y)
        M <- paste("N=",N," n=",n," rep.=",B,sep="")
    }
    
    if (grafico) {
        par(mfrow=c(2,1),mar=c(3,2,2,2))
        ## popolazione
        hist(Omega,main='popolazione',col='yellow',freq=FALSE,xlim=range(c(Omega,mx)))
        abline(v=mi,col="red",lwd=2)
        
        ### campione
        hist(mx,col="gray",main=M,
                freq=FALSE,xlim=range(c(Omega,mx)))
        abline(v=mean(mx),col="red",lwd=2)
    }
    
    ##### tabella risultati
    s2x <- var(mx)*(length(mx)-1)/length(mx)
    sx <- sqrt(s2x)
    k <- ifelse(replace,1,((N-n)/(N-1)))
    modo <- ifelse(replace,'con reinserimento','senza reinserimento')
    modo <- paste(funzione,modo,sep=' ')
    campionamento <- ifelse(exact,"esatta","approssimata")

    cat("Dati popolazione","\n")
    cat("------------------------------------","\n")
    cat(paste("N = ",N," - theta = ",round(mi,4),sep=""),"\n")
    cat("------------------------------------","\n")
    cat(" ","\n")
    
    cat("Dati distribuzione campionaria",campionamento,modo,"\n")
    cat("------------------------------------","\n")
        cat(paste("n = ",n," - E(theta) = ",round(mean(mx),4),
            " - se(theta) = ",round(sx,4),sep=""),"\n")
    cat("------------------------------------","\n")

    return(list(Omega=Omega,Stheta=mx,theta=theta))

}

#A <- campionaria.theta(rnorm(10,100,15),exact=TRUE,theta='sum')
