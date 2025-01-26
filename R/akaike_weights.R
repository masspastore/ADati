#' Computing of Akaike weigths
#' @param x = vector of Information Criterion values
akaike_weights <- function(x) {
  (delta <- x-x[which.min(x)])
  num <- exp(-.5*delta)
  den <- sum(num)
  return(list(delta=delta,w=num/den))
}
