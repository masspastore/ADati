#rm(list=ls())
#Y1 <- c(94,197,16,38,99,141,23)
#Y2 <- c(52,104,146,10,50,31,40,27,46)

#Omega1 <- Y1
#n1 <- 2
#Omega2 <- Y2
#n2 <- 2
#theta <- 'min'
#replace <- FALSE
#exact <-FALSE
#grafico <- TRUE
#B <- 1000

### distribuzione campionaria di un parametro theta
## richiede gtools
campionaria.theta2 <- function(Omega1,n1=2,n2=n1,Omega2=NULL,theta='diff',
           replace=FALSE,exact=FALSE,grafico=TRUE,B=1000,parziali=FALSE) 
{
    # controlli
    funzione <- theta # serve per output
    theta <- get(theta,mode='function')
    
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
    
    N1 <- length(Omega1)
    N2 <- length(Omega2)
    
    if ((N1<2)|(N2<2)) stop('Population must have at least 2 elements')
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
    Stheta <- NULL
    if (exact) {
        # campioni esaustivi
        #X1 <- permutations(N1,n1,Omega1,set=FALSE,repeats=replace)
        X1 <- gtools::combinations(N1,n1,Omega1,set=FALSE,repeats.allowed=replace)
        #X2 <- permutations(N2,n2,Omega2,set=FALSE,repeats=replace)
        X2 <- gtools::combinations(N2,n2,Omega2,set=FALSE,repeats.allowed=replace)
	
	for (j in 1:nrow(X1)) {
	  for (i in 1:nrow(X2)) {
	    Stheta <- c(Stheta,theta(X1[j,],X2[i,]))
	  }
	}
	
	DS <- density(Stheta)
        Ymax <- max(DS$y)
        xmax <- DS$x[which(DS$y==Ymax)]
        M <- paste("campioni = ",length(Stheta),sep="")
    } else {
        # campioni casuali
        for (b in 1:B) {
            x1 <- sample(Omega1,n1,replace=replace)
            x2 <- sample(Omega2,n2,replace=replace)
            Stheta <- c(Stheta,theta(x1,x2))
            if (parziali) {
                par(mfrow=c(1,1))
                H <- hist(Stheta,col="gray",main=paste("replicazioni = ",b,sep=""),
                    freq=FALSE)
            }
        }
        H <- density(Stheta)
        xmax <- max(H$x)
        Ymax <- max(H$y)
        M <- paste("replicazioni = ",b,sep="")
    }

    # grafico finale
    k1 <- ifelse(replace,1,((N1-n1)/(N1-1)))
    k2 <- ifelse(replace,1,((N2-n2)/(N2-1)))
    if (grafico) {
	par(mfrow=c(1,1))
        H <- hist(Stheta,col="gray",main=M,freq=FALSE,xlab=expression(hat(theta)))
    }

    ##### tabella risultati
    h <- length(Stheta)
    modo <- ifelse(replace,'con reinserimento','senza reinserimento')
    modo <- paste(funzione,modo)
    campionamento <- ifelse(exact,"esatta","approssimata")
    cat("Dati popolazione","\n")
    cat("------------------------------------","\n")
    cat(paste("N1 = ",N1," - mi = ",round(mi1,4),
        " - sigma2 = ",round(sigma2.1,4)),"\n")
    cat(paste("N2 = ",N2," - mi = ",round(mi2,4),
        " - sigma2 = ",round(sigma2.2,4)),"\n")
    cat(paste("theta = ",round(theta(Omega1,Omega2),4)),"\n")
    cat("------------------------------------","\n")
    cat(" ","\n")

    cat("Dati distribuzione campionaria",campionamento,modo,"\n")
    cat("------------------------------------","\n")
    cat(paste("n1 = ",n1," - n2 = ",n2,sep=""),"\n")
    cat(paste("E(theta) = ",round(mean(Stheta),4),
        " - V(theta) = ",round(var(Stheta)*((h-1)/h),4)),"\n")
    cat("------------------------------------","\n")

    return(list(Omega1=Omega1,Omega2=Omega2,Stheta=Stheta,theta=theta))
    
}

#campionaria.theta2(0:3,theta='max',exact=TRUE,replace=TRUE)
#campionaria.theta2(0:3,theta='max',exact=TRUE) # senza reinserimento
#fx <- function(x1,x2){max(mean(x1),mean(x2))}
#campionaria.theta2(rnorm(50),n1=8,theta='fx',B=50)
#campionaria.theta2(rnorm(50),n1=8,Omega2=rnorm(50,5,3),theta='fx',B=50)