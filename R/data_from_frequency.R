data_from_frequency <- function(x,f) {
  y <- NULL
  for (i in 1:length(x)) {
    y <- c(y, rep(x[i],f[i]))
  }
  return( y )
}
