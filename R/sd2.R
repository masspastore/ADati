sd2 <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- na.omit(x)
  }
  n <- length(x)
  sqrt( var(x) * (n - 1)/n )
}