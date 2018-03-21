### Bayes Factor approssimato
#rm(list=ls())
approxBF <- function(M0,M1) {
  (Bic0 <- BIC(M0))
  (Bic1 <- BIC(M1))
  (dBic <- Bic0-Bic1)
  (BF <- exp(dBic/2))
  
  (support <- ifelse(dBic<=0,"supports M0","supports M1"))

  cat("-------------------","\n")
  cat(paste("Bayes Factor",support),"\n")
  if (dBic<=0) print(formula(M0))
  if (dBic>0) print(formula(M1))
  
  cat("-------------------","\n")
  cat(" ","\n")
  return(list(dBIC=dBic,BF=BF))
}

