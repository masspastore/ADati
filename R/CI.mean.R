"CI.mean" <-
function(x,sigma,n,level=.95)
{
  if (is.matrix(x)) {
    nomir <- rownames(x)
    nomic <- colnames(x)
    x <- as.vector(x)
    nomi <- NULL
    for (ic in nomic) {
      for (ir in nomir) {
        nomi <- c(nomi,paste(ic,ir,sep="-"))  
      }
    }            
    names(x) <- nomi
  }
  if (is.matrix(sigma)) sigma <- as.vector(sigma)
  if (is.matrix(n)) n <- as.vector(n)
  
  err.st <- sigma/sqrt(n)
  q.inf <- qt((1-level)/2,n-1)
  q.sup <- qt(1-(1-level)/2,n-1)
  CI <- cbind(x+err.st*q.inf,x+err.st*q.sup)
  input <- round(cbind(x,sigma,round(n,0),level),2)
  colnames(input) <- c("mean","sd","n","level")
  colnames(CI) <- c("inf","sup")
  return(list(input=input,err.st=err.st,CI=CI))
}
