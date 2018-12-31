### distribuzione campionaria della media
## richiede gtools
#rm(list=ls())
#Omega1 <- rnorm(100)
#Omega2 <- rnorm(100)
#n1 <- 3
#n2 <- 4

campionaria.diffmedie <- function(Omega1,n1=2,n2=n1,Omega2=NULL,
                        replace=FALSE,exact=FALSE,grafico=TRUE,B=1000,
                                  parziali=FALSE) 
{
    # impostazione per evitare il warning nella compilazione
    x <- NULL
    if (sum(is.na(Omega1))>0) {
      warning(paste("Removing",sum(is.na(Omega1)),"NA's"))
      Omega1 <- na.omit(Omega1)
    }
    if (!is.null(Omega2)) {
      if (sum(is.na(Omega2))>0) {
        warning(paste("Removing",sum(is.na(Omega2)),"NA's"))
        Omega1 <- na.omit(Omega2)
      }      
    } else {
      Omega2 <- Omega1
    }
    
    # controlli    
    N1 <- length(Omega1)
    N2 <- length(Omega2)
    
    if ((N1<2)|(N2<2)) stop('Population must have at least 2 elements')
    
    if (N1<171) {
      ncampioni1 <- factorial(N1)/factorial(N1-n1)
    } else {
      ncampioni1 <- 500001
    }
    
    if (N2<171) {
      ncampioni2 <- factorial(N2)/factorial(N2-n2)
    } else {
      ncampioni2 <- 500001
    }

    if ((exact==TRUE)&((ncampioni1>50000)|(ncampioni2>50000))) {
        warning('Exact computation is not alloweed')
        exact <- FALSE
    }
    
    # parametri 
    mi1 <- mean(Omega1)
    sigma2.1 <- var(Omega1)*((N1-1)/N1)
    sigma.1 <- sqrt(sigma2.1)
    mi2 <- mean(Omega2)
    sigma2.2 <- var(Omega2)*((N2-1)/N2)
    sigma.2 <- sqrt(sigma2.2)
    
    #################################################
    if (exact) {
        # campioni esaustivi
        X1 <- gtools::permutations(N1,n1,Omega1,set=FALSE,repeats.allowed=replace)
        X2 <- gtools::permutations(N2,n2,Omega2,set=FALSE,repeats.allowed=replace)
        
        mx1 <- apply(X1,1,mean)
        mx2 <- apply(X2,1,mean)
        Scoppie <- expand.grid(mx2=mx2,mx1=mx1)
        Sdiff <- apply(Scoppie,1,diff)

        Ymax <- max(density(Sdiff)$y)
        xmax <- density(Sdiff)$x[which(density(Sdiff)$y==Ymax)]
        M <- paste("campioni = ",length(Sdiff),sep="")
    } else {
        # campioni casuali
        Sdiff <- sapply(1:B, function(b){
          x1 <- sample(Omega1,n1,replace=replace)
          x2 <- sample(Omega2,n2,replace=replace)
          mean(x1)-mean(x2)
        })
      
      
        if (parziali) {
          for (b in 1:B) {
                par(mfrow=c(1,1))
                H <- hist(Sdiff[1:b],col="gray",main=paste("repliche = ",b,sep=""),
                    freq=FALSE)
            }
        }
        H <- density(Sdiff)
        xmax <- max(H$x)
        Ymax <- max(H$y)
        M <- paste("repliche = ",B,sep="")
    }

    # grafico finale
    k1 <- ifelse(replace,1,((N1-n1)/(N1-1)))
    k2 <- ifelse(replace,1,((N2-n2)/(N2-1)))
    if (grafico) {
        ymax <- dnorm(xmax,(mi1-mi2),sqrt((sigma2.1/n1)*k1+(sigma2.2/n2)*k2))
        par(mfrow=c(1,1))
        H <- hist(Sdiff,col="gray",main=M,freq=FALSE,ylim=c(0,max(ymax,Ymax)))
        curve(dnorm(x,(mi1-mi2),sqrt((sigma2.1/n1)*k1+(sigma2.2/n2)*k2)),add=TRUE,col="red",lwd=2)
    }

    ##### tabella risultati
    h <- length(Sdiff)
    modo <- ifelse(replace,'con reinserimento','senza reinserimento')
    campionamento <- ifelse(exact,"esatta","approssimata")
    
    cat("Dati popolazione","\n")
    cat("------------------------------------","\n")
    cat(paste("N1 = ",N1," - mi = ",round(mi1,4),
        " - sigma2 = ",round(sigma2.1,4)),"\n")
    cat(paste("N2 = ",N2," - mi = ",round(mi2,4),
        " - sigma2 = ",round(sigma2.2,4)),"\n")
    cat(paste("(mi1-mi2) = ",round(mi1-mi2,4),
        " - sigma2(mi1-mi2) = ",round((sigma2.1/n1)*k1+(sigma2.2/n2)*k2,4)),"\n")
    cat("------------------------------------","\n")
    cat(" ","\n")

    cat("Dati distribuzione campionaria",campionamento,modo,"\n")
    cat("------------------------------------","\n")
    cat(paste("n1 = ",n1," - n2 = ",n2,sep=""),"\n")
    cat(paste("E(mi1-mi2) = ",round(mean(Sdiff),4),
        " - V(mi1-mi2) = ",round(var(Sdiff)*((h-1)/h),4)),"\n")
    cat("------------------------------------","\n")

    return(list(Omega1=Omega1,Omega2=Omega2,Sdiff=Sdiff))
    
}
