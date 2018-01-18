### Bayes Factor approssimato
#rm(list=ls())
approxBF <- function(M0,M1) {
  (Bic0 <- BIC(M0))
  (Bic1 <- BIC(M1))
  (dBic <- Bic0-Bic1)
  (BF <- exp(-dBic/2))
  
  (support <- ifelse(dBic<=0,"supports H0","supports H1"))
  
  cat("-------------------","\n")
  cat(paste("delta.BIC",support),"\n")
  cat("-------------------","\n")
  cat(" ","\n")
  return(list(dBIC=dBic,BF=BF))
}

