## calcolo TCD
#rm(list=ls())
##library(lavaan)
#data(Bullying,package="ADati")
#fitLM <- lm("PBB~PPP",data=Bullying)
#summary(fitLM)$r.squared

#fit <- lavaan::sem("PBB~PPP",data=Bullying)
#yvar <- NULL

TCD <- function(fit,yvar=NULL,allmatrices=FALSE) {
  
  # controllo se ci sono variabili latenti
  (modelvar <- unique(unlist(fit@Model@dimNames[[1]])))
  if (length(modelvar)!=length(fit@Data@ov.names[[1]])) {
    stop("Attualmente non funziona con variabili latenti")
  }
  
  if (is.null(yvar)) yvar <- setdiff(fit@Data@ov.names[[1]],fit@Data@ov.names.x[[1]])
  (PS <- lavaan::inspect(fit,"est")$psi[yvar,yvar])
  (Sy <- lavaan::fitted(fit)$cov[yvar,yvar])
  
  if (is.null(dim(PS))) {
    TCD <- 1-PS/Sy
  } else {
    TCD <- 1-det(PS)/det(Sy)  
  }
  
  if (allmatrices) {
    return(list(TCD=TCD,PS=PS,Sy=Sy))  
  } else {
    return(TCD)
  }
}

#TCD(fit)
