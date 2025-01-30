
check_model_data <- function(fit, cook_levels = c(.5,1)) {

  # per gestire i mancanti
  coef.names <- names(rstanarm::fixef(fit))
  npred <- length(rstanarm::fixef(fit))
  FORMULA <- formula(fit)
  X <- attr(terms(FORMULA), which = "term.labels")
  Y <- as.character(attr(terms(FORMULA), which = "variables")[[2]])
  FORMULA <- paste0("y ~ ",paste(X, collapse=" + "))
  PREDS <-  X <- X[!grepl(":",X)] 
  
  modelData <- na.omit( fit$data[c(Y,PREDS)] )
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

