### distribuzione campionaria della correlazione
## richiede gtools e MASS

#rho=.5
#N=100
#n <- 3
#exact <- TRUE
#B <- 100

campionaria.cor <- function(rho=0,N=10,n=3,replace=FALSE,exact=FALSE,grafico=TRUE,B=1000,
                            parziali=FALSE) 
{
    # controlli    
    if (N<3) stop('Population must have at least 3 elements')
    
    if (N<171) {
      (campioni.possibili <- factorial(N)/factorial(N-n))
    } else {
      campioni.possibili <- 50001
    }
    if ((exact==TRUE)&(campioni.possibili>50000)) {
      warning('Exact computation is not alloweed')
      exact <- FALSE
    }
    
    # creo la popolazione 
    Omega <- MASS::mvrnorm(N,c(0,0),matrix(c(1,rho,rho,1),2,2),empirical=TRUE)

    #################################################
    rx <- NULL
    if (exact) {
        # campioni esaustivi
        i <- gtools::permutations(N,n,1:N,set=FALSE,repeats.allowed=replace)
        
        for (j in 1:nrow(i)) {
            rx <- c(rx,cor(Omega[i[j,],])[2,1])
        }
        M <- paste("N=",N," n=",n," rho=",round(cor(Omega)[2,1],3),
                                    " campioni=",nrow(i),sep="")
    } else {
        # campioni casuali
        rx <- sapply(1:B, function(b){
          i <- sample(1:N,n,replace=replace)
          cor(Omega[i,])[2,1]
        })  
      
        if (parziali) {
          for (b in 1:B) {
                par(mfrow=c(1,1))
                H <- hist(rx[1:b],col="gray",main=paste("N=",N," n=",n,
                       " rho=",round(cor(Omega)[2,1],3)," rep.=",b,sep=""),
                       freq=FALSE,xlim=c(-1,1))
            }
        }
        M <- paste("N=",N," n=",n," rho=",round(cor(Omega)[2,1],3),
                                            " rep.=",B,sep="")
    }
    
    if (grafico) {
        par(mfrow=c(1,1),mar=c(3,2,2,2))
        ## popolazione
        hist(rx,main=M,col='yellow',freq=FALSE,xlim=c(-1,1))
        abline(v=mean(rx,na.rm=TRUE),col="red",lwd=2)
    }
    
    ##### tabella risultati
    rx <- na.omit(rx)
    s2x <- var(rx)*(length(rx)-1)/length(rx)
    sx <- sqrt(s2x)
    modo <- ifelse(replace,'con reinserimento','senza reinserimento')
    campionamento <- ifelse(exact,"esatta","approssimata")

    cat("Dati popolazione","\n")
    cat("------------------------------------","\n")
    cat(paste("N = ",N," - rho = ",rho,sep=''),"\n")
    cat("------------------------------------","\n")
    cat(" ","\n")
    
    cat("Dati distribuzione campionaria",campionamento,modo,"\n")
    cat("------------------------------------","\n")
    cat(paste("n = ",n," - E(rx) = ",round(mean(rx),4),
            " - se(rx) = ",round(sx,4),sep=""),"\n")
    cat("------------------------------------","\n")

    return(list(Omega=Omega,Srx=rx))

}
