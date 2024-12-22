#################  parametri di prova
#rm(list=ls())
#set.seed(20121018)
#Omega <- c(1:5)
#Omega <- Omega[1:5]
#n <- 3
#replace=FALSE; exact=TRUE; grafico=FALSE; B=10; parziali=FALSE

### distribuzione campionaria della varianza
## richiede gtools
campionaria.varianza <- function(Omega,n=2,replace=FALSE,exact=FALSE,grafico=TRUE,B=1000,
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
    
    if (N<n){
      warning("Sample size greater than population size; replace must be TRUE")
      replace <- TRUE
    }
    
    # parametri 
    (mi <- mean(Omega))
    (sigma2 <- var(Omega)*((N-1)/N))
    (sigma <- sqrt(sigma2))    

    #################################################
    if (exact) {
        # campioni esaustivi
        X <- gtools::permutations(N,n,Omega,set=FALSE,repeats.allowed=replace)
        nx <- nrow(X)
        (sx <- apply(X,1,var))
        sx.nc <- sx*((n-1)/n)
        M <- paste("n=",n," campioni=",nrow(X),sep="")
    } else {
        # campioni casuali
        x <- replicate(B, sample(Omega,n,replace=replace))
        sx <- apply(x,2,var)
        sx.nc <- sx*(n-1)/n      
        
        if (parziali) {
          for (b in 1:B) {
              par(mfrow=c(1,1))
              H <- hist(sx.nc[1:b],col="gray",main=paste("N=",N," n=",n," rep.=",b,sep=""),
                  freq=FALSE) 
            }
        }
        M <- paste("n=",n," rep.=",B,sep="")
    }

    if (grafico) {
        layout(matrix(c(1,1,2,3),2,2))
        par(mar=c(3,2,2,2))
        ## popolazione
        hist(Omega,main=paste('popolazione: N = ',N,' sigma2 = ',round(sigma2,4)),
            col='yellow',freq=FALSE)
        
        ### campione
        #ymax <- dchisq(xmax,n-1)
        hist(sx.nc,col="gray",main=paste(M," E(s2x)=",round(mean(sx.nc),4),sep=""),
                freq=FALSE,xlim=range(c(sx.nc,sx))) #,ylim=c(0,max(ymax,Ymax)),xlim=range(Omega)
        abline(v=mean(sx.nc),col="red",lwd=2)
        #curve(dchisq(x,n-1),add=TRUE,col="blue",lwd=2)
        
        hist(sx,col="gray",main=paste("varianze corrette E(sigma2x)=",round(mean(sx),4),sep=""),
                                      freq=FALSE,xlim=range(c(sx.nc,sx)))
        abline(v=mean(sx),col="red",lwd=2)
        #curve(dchisq(x,n-1),add=TRUE,col="blue",lwd=2)
    }
    
    ##### tabella risultati
    modo <- ifelse(replace,'con reinserimento','senza reinserimento')
    campionamento <- ifelse(exact,"esatta","approssimata")
    cat("Dati popolazione","\n")
    cat("------------------------------------","\n")
    cat(paste("N = ",N," - mi = ",round(mi,4),
        " - sigma2 = ",round(sigma2,4),sep=""),"\n")
    cat("------------------------------------","\n")
    cat(" ","\n")

    cat("Dati distribuzione campionaria",campionamento,modo,"\n")
    cat("------------------------------------","\n")
        cat(paste("n = ",n," - E(s2x) = ",round(mean(sx.nc),4),
            " - E(sigma2x) = ",round(mean(sx),4),sep=''),"\n")
    cat("------------------------------------","\n")
    cat("NOTE: s2x = var. non corretta, sigma2x = var. corretta","\n")    
    
    return(list(Omega=Omega,sigma2x=sx,s2x=sx.nc))
}

#A <- campionaria.varianza(Omega,exact=TRUE,replace=TRUE)