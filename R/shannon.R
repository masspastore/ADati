shannon <- function(x) {
  f <- table(x)
  p <- f / sum(f)
  S <- sum( p * log(p) ) *(-1)
  return( S )
}
