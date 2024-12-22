#' @name cooks_distance_stanarm
#' @description Calcola le distanze di Cook di un modello ottenuto con rstanarm
#' @param fit = fit del modello
cooks_distance_stanarm <- function(fit) {
  X <- model.matrix(fit)
  n <- nrow(X)
  p <- ncol(X)
  
  # Calcola gli hat values
  hat_values <- hat_values_stanarm(fit)
  
  # Calcola i residui
  .resid <- residuals(fit)
  MSE <- sum(.resid^2) / (n - p)
  .stdresid <- .resid / sqrt(MSE)
  
  # Calcolo della distanza di Cook
  cook_d <- (.stdresid^2 / p) * (hat_values / 
                                    (1 - hat_values)^2)
  return(cook_d)
}

