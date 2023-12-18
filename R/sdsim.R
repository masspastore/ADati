#rm(list=ls())
#n <- 50 
#srcdist <- "T"
#param1 <- NULL
#param2 <- NULL
#r <- 100
### original script by Nicole Radziwill
### http://qualityandinnovation.com/2015/03/30/sampling-distributions-and-central-limit-theorem-in-r/
sdsim <- function(n,popdist=c("Exp","Normal","Unif","Pois","Cauchy","Binom","Gamma","Xisq","Tstudent"),param1=NULL,param2=NULL,R=10000) {
  # r <- 10000  Number of replications/samples - DO NOT ADJUST
  # This produces a matrix of observations with  
  # n columns and r rows. Each row is one sample:
  popdist <- match.arg(popdist)
#  src.dist <- ifelse(srcdist=="Chisq","X",ifelse(srcdist=="Tstudent","T",srcdist))
  src.dist <- substr(popdist,1,1)
  
  ## controls
  if (is.null(param1)) {
    warning("param1 is missing!")
    param1 <- ifelse((src.dist=="Exp"|src.dist=="P"|src.dist=="X"|src.dist=="T"|src.dist=="B"),1,0)
  }
  if (src.dist=="N"|src.dist=="U"|src.dist=="C"|src.dist=="B"|src.dist=="G") {
    if (is.null(param2)) {
      warning("param2 is missing!")
      param2 <- ifelse((src.dist=="N"|src.dist=="C"|src.dist=="G"),1,
                       ifelse(src.dist=="U",(param1+1),.5))
    }
  }
  
  my.samples <- switch(src.dist,
                       "Exp" = matrix(rexp(n*R,param1),R),
                       "N" = matrix(rnorm(n*R,param1,param2),R),
                       "U" = matrix(runif(n*R,param1,param2),R),
                       "P" = matrix(rpois(n*R,param1),R),
                       "C" = matrix(rcauchy(n*R,param1,param2),R),
                       "B" = matrix(rbinom(n*R,param1,param2),R),
                       "G" = matrix(rgamma(n*R,param1,param2),R),
                       "X" = matrix(rchisq(n*R,param1),R),
                       "T" = matrix(rt(n*R,param1),R))
  all.sample.sums <- apply(my.samples,1,sum)
  all.sample.means <- apply(my.samples,1,mean)   
  all.sample.vars <- apply(my.samples,1,var) 
  
  par(mfrow=c(2,2),mar=c(2,2,2,2))
  hist(my.samples[1,],col="gray",main="Distribution of One Sample")
  hist(all.sample.sums,col="gray",main="Sampling Distribution of
       the Sum")
  hist(all.sample.means,col="gray",main="Sampling Distribution of the Mean")
  hist(all.sample.vars,col="gray",main="Sampling Distribution of
       the Variance")
  return(list(distribution=popdist,param1=param1,param2=param2))
}