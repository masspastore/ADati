#' @name hat_values_stanarm
#' @description Calcola gli hat values di un modello ottenuto con rstanarm
#' @param fit = fit del modello
#' @return Restituisce il vettore di hatvalues
#' @note Nel caso di modelli con effetti random vengono calcolati come pareto k 
hat_values_stanarm <- function(fit) {
  
  X <- model.matrix(fit)
  
  if ("lmerMod" %in% class(fit)) {
    
    loo_result <- loo(fit)
    hatvalues <- loo_result$diagnostics$pareto_k
    
  } else {
    H <- X %*% solve(t(X) %*% X) %*% t(X)
    hatvalues <- diag(H)
  }
  
  return(hatvalues)
}

