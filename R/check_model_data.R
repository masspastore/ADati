
check_model_data <- function(fit, cook_levels = c(.5,1)) {

  modelData <- fit$data
  modelData$.x <- 1:nrow(modelData)
  modelData$.resid <- residuals(fit)  
  modelData$.fitted <- fitted(fit)
  modelData$.stdresid <- c(scale(residuals(fit)))  
  modelData$.hat <- hat_values_stanarm(fit)
  modelData$.cook <- cooks_distance_stanarm(fit)
  modelData$.k <- loo(fit)$diagnostics$pareto_k
  modelData$.influential <- modelData$.cook > min(cook_levels)
  
  return(modelData)
}

