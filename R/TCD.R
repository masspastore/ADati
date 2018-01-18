## calcolo TCD
#rm(list=ls())
##library(lavaan)
#data(Bullying,package="ADati")
#fitLM <- lm("PBB~PPP",data=Bullying)
#summary(fitLM)$r.squared

#fit <- lavaan::sem("PBB~PPP",data=Bullying)
#yvar <- NULL

TCD <- function(fit,yvar=NULL) {
  if (is.null(yvar)) yvar <- setdiff(fit@Data@ov.names[[1]],fit@Data@ov.names.x[[1]])
  
  (PS <- lavaan::inspect(fit,"est")$psi[yvar,yvar])
  (Sy <- lavaan::fitted(fit)$cov[yvar,yvar])
  
  if (is.null(dim(PS))) {
    TCD <- 1-PS/Sy
  } else {
    TCD <- 1-det(PS)/det(Sy)  
  }
  return(list(TCD=TCD,PS=PS,Sy=Sy))
}

#TCD(fit)
