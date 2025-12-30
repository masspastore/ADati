gini <- function(x) {
  f <- table(x)
  p <- f / sum(f)
  G <- 1-sum(p^2)
  return(G)
}
